-module(voodoo_backend).
-behaviour(vegur_interface).
-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         feature/2,
         additional_headers/4,
         error_page/4
        ]).

-record(state, {tries = [] :: list()}).

init(_AcceptTime, Upstream) ->
  {ok, Upstream, #state{}}.

lookup_domain_name(_ReqDomain, Upstream, State) ->
  Servers = [
    {1, {127, 0, 0, 1}, 8081},
    {2, {127, 0, 0, 1}, 8082}
  ],
  {ok, Servers, Upstream, State}.

checkout_service(Servers, Upstream, State=#state{tries=Tried}) ->
  Available = Servers -- Tried,
  case Available of
    [] ->
      {error, all_blocked, Upstream, State};
    _ ->
      N = rand:uniform(length(Available)),
      Pick = lists:nth(N, Available),
      {service, Pick, Upstream, State#state{tries=[Pick | Tried]}}
  end.

service_backend({_Id, IP, Port}, Upstream, State) ->
  {{IP, Port}, Upstream, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
  {ok, Upstream, State}.

feature(_WhoCares, State) ->
  {disabled, State}.

additional_headers(_Direction, _Log, _Upstream, State) ->
  {[], State}.

error_page(all_blocked, _DomainGroup, Upstream, State) ->
    {{502, [], <<>>}, Upstream, State}; % Bad Gateway
error_page({upstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Blame the caller
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Blame the server
    {{500, [], <<>>}, Upstream, HandlerState};
error_page({undefined, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Who knows who was to blame!
    {{500, [], <<>>}, Upstream, HandlerState};
%% Specific error codes from middleware
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
    {{417, [], <<>>}, Upstream, HandlerState};
%% Catch-all
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{500, [], <<>>}, Upstream, HandlerState}.

terminate(_, _, _) ->
  ok.
