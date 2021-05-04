% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank PSD2 OAuth API.

-module(swedbank_auth).

-export([authorize/2, authorize/3, token/2]).

-export_type([scope/0,
              code/0,
              state/0,
              authorize_response/0,
              token_response/0,
              access_token/0,
              expires_in/0,
              refresh_token/0,
              token_type/0
             ]).


%%%%%%%%%%%
%% TYPES %%
%%%%%%%%%%%

-type scope() :: list(
             'PSD2' | 'PSD2sandbox' |
             'PSD2account_list' | 'PSD2sandboxaccount_list' |
             'PSD2account_balances' | 'PSD2sandboxaccount_balances' |
             'PSD2account_transactions' | 'PSD2sandboxaccount_transactions' |
             'PSD2account_transactions_over90' | 'PSD2sandboxaccount_transactions_over90'
            ).
-type code() :: binary().
-type state() :: binary().
-type authorize_response() :: uri_string:uri_string().
-type token_response() :: #{access_token := access_token(),
                            expires_in := expires_in(),
                            refresh_token := refresh_token(),
                            scope := scope(),
                            token_type := token_type()
                           }.
-type access_token() :: binary().
-type expires_in() :: integer().
-type refresh_token() :: binary().
-type token_type() :: binary().


%%%%%%%%%
%% API %%
%%%%%%%%%

-spec authorize(scope(), swedbank_client:options()) ->
          {ok, authorize_response()} |
          {error, swedbank_client:request_error()}.
authorize(Scope, Options) ->
    authorize(Scope, <<"">>, Options).

-spec authorize(scope(), state(), swedbank_client:options()) ->
          {ok, authorize_response()} |
          {error, swedbank_client:request_error()}.
authorize(Scope, State, Options) ->
    Query = [{<<"bic">>, atom_to_binary(maps:get(bic, Options))},
             {<<"client_id">>, maps:get(client_id, Options)},
             {<<"redirect_uri">>, maps:get(redirect_uri, Options)},
             {<<"response_type">>, <<"code">>},
             {<<"scope">>, scope_to_binary(Scope)}
            ] ++ (case State of <<"">> -> []; State -> [{<<"state">>, State}] end),
    Uri = swedbank_client:request_uri(swedbank_client:api_path(psd2_authorize, Options), Query, Options),
    case httpc:request(get, {Uri, []}, [{autoredirect, false}], []) of
        {ok, {{_,302,_},Headers,_Body}} ->
            {ok, proplists:get_value("location", Headers)};
        Response ->
            {error, Response}
    end.

-spec token(code(), swedbank_client:options()) ->
          {ok, token_response()} |
          {error, swedbank_client:request_error()}.
token(Code, Options) ->
    Uri = swedbank_client:request_uri(swedbank_client:api_path(psd2_token, Options), [], Options),
    Body = uri_string:compose_query(
        [{<<"code">>, Code},
         {<<"grant_type">>, <<"authorization_code">>},
         {<<"client_id">>, maps:get(client_id, Options)},
         {<<"client_secret">>, maps:get(client_secret, Options)},
         {<<"redirect_uri">>, maps:get(redirect_uri, Options)}
        ]),
    case httpc:request(post, {Uri, [], "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_,200,_},_Headers,Body}} ->
            {ok, access_token_response(jsx:decode(Body))};
        Response ->
            {error, Response}
    end.


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec scope_to_binary(scope()) -> binary().
scope_to_binary(Scope) when is_list(Scope) ->
    list_to_binary(lists:join(" ", lists:map(fun(S) -> atom_to_list(S) end, Scope))).

-spec binary_to_scope(binary()) -> scope().
binary_to_scope(Binary) ->
    lists:map(fun decode_scope/1, string:split(Binary, " ", all)).

decode_scope(<<"PSD2">>) -> 'PSD2';
decode_scope(<<"PSD2sandbox">>) -> 'PSD2sandbox'; 
decode_scope(<<"PSD2account_list">>) -> 'PSD2account_list';
decode_scope(<<"PSD2sandboxaccount_list">>) -> 'PSD2sandboxaccount_list';
decode_scope(<<"PSD2account_balances">>) -> 'PSD2account_balances';
decode_scope(<<"PSD2sanboxaccount_balances">>) -> 'PSD2sandboxaccount_balances';
decode_scope(<<"PSD2account_transactions">>) -> 'PSD2account_transactions';
decode_scope(<<"PSD2sandboxaccount_transactions">>) -> 'PSD2sandboxaccount_transactions';
decode_scope(<<"PSD2account_transactions_over90">>) -> 'PSD2account_transactions_over90';
decode_scope(<<"PSD2sandboxaccount_transactions_over90">>) -> 'PSD2sandbox_account_transactions_over90'.

-spec access_token_response(map()) -> token_response().
access_token_response(#{<<"access_token">>:=AccessToken,
                        <<"expires_in">>:=ExpiresIn,
                        <<"refresh_token">>:=RefreshToken,
                        <<"scope">>:=Scope,
                        <<"token_type">>:=TokenType
                       }) ->
    #{access_token => AccessToken,
      expires_in => ExpiresIn,
      refresh_token => RefreshToken,
      scope => binary_to_scope(Scope),
      token_type => TokenType
     }.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

scope_to_binary_test() ->
    ?assertEqual(<<"foo bar">>, scope_to_binary([foo, bar])).

access_token_response_test() ->
    ?assertEqual(
       #{access_token => <<"test">>,
         expires_in => 3600,
         refresh_token => <<"test">>,
         scope => ['PSD2', 'PSD2account_list'],
         token_type => <<"test">>
        },
       access_token_response(
         #{<<"access_token">> => <<"test">>,
           <<"expires_in">> => 3600,
           <<"refresh_token">> => <<"test">>,
           <<"scope">> => <<"PSD2 PSD2account_list">>,
           <<"token_type">> => <<"test">>
          })).

-endif.
