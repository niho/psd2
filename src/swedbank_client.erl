% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank PSD2 API client.

-module(swedbank_client).

-define(ENDPOINT, "https://psd2.api.swedbank.com").
-define(TIMEOUT, 5000).

-export([api_path/2,
         api_path/3,
         request_uri/3,
         get/5,
         get/6,
         post/6,
         post/7,
         put/6,
         put/7,
         delete/5,
         delete/6,
         tpp_headers/1,
         decode_links/2
        ]).

-export_type([api/0,
              bic/0,
              options/0,
              access_token/0,
              psu/0,
              tpp/0,
              request_error/0,
              response/0,
              links/1
             ]).

%%%%%%%%%%%
%% TYPES %%
%%%%%%%%%%%

-type api() :: v3_accounts | v3_consents | v3_payments | psd2_authorize | psd2_token | sandbox_sign.
-type bic() :: 'SANDSESS' | 'SWEDSESS' | % Sweden
               'SANDLT22' | 'HABALT22' | % Lithuania
               'SANDLV22' | 'HABALV22' | % Latvia
               'SANDEE2X' | 'HABAEE2X'.  % Estonia
-type options() :: #{bic := bic(),
                     client_id := binary(),
                     client_secret := binary(),
                     redirect_uri := binary(),
                     timeout => integer(),
                     endpoint => httpc:url()
                    }.
-type access_token() :: binary().
% Payment Service User (PSU)
-type psu() :: undefined |
               #{ip_address := inet:ip_address(),
                 ip_port := inet:port_number(),
                 http_method := httpc:method(),
                 user_agent := string()
                }.
% Third Party Provider (TPP)
-type tpp() :: #{redirect_preferred := boolean(),
                 redirect_uri := binary(),
                 nok_redirect_uri := binary(),
                 explicit_authorisation_preferred := boolean()
                }.
-type request_error() :: any().
-type query() :: list({unicode:chardata(), unicode:chardata()}).
-type response() :: {httpc:status_code(),
                     httpc:headers(),
                     string() | binary() | jsx:json_term()
                    }.
-type links(Rel) :: #{Rel => binary()}.


%%%%%%%%%
%% API %%
%%%%%%%%%

-spec api_path(api(), options()) -> httpc:path().
api_path(v3_accounts, Options) ->
    case is_sandbox(Options) of
        false -> "v3/accounts";
        true -> "sandbox/v3/accounts"
    end;
api_path(v3_consents, Options) ->
    case is_sandbox(Options) of
        false -> "v3/consents";
        true -> "sandbox/v3/consents"
    end;
api_path(v3_payments, Options) ->
    case is_sandbox(Options) of
        false -> "v3/payments";
        true -> "sandbox/v3/payments"
    end;
api_path(psd2_authorize, _Options) -> "psd2/authorize";
api_path(psd2_token, _Options) -> "psd2/token";
api_path(sandbox_sign, _Options) -> "sandbox/sign".

-spec api_path(api(), list(string()), swedbank_client:options()) -> httpc:path().
api_path(Api, Parts, Options) ->
    string:join([api_path(Api, Options)] ++ Parts, "/").

-spec request_uri(httpc:path(), query(), options()) ->
          uri_string:uri_string().
request_uri(Path, Query, Options) ->
    uri_string:resolve(#{
                         path => Path,
                         query => uri_string:compose_query(Query)
                        }, maps:get(endpoint, Options, ?ENDPOINT)).

-spec get(httpc:path(), query(), httpc:headers(), access_token(), options()) -> response().
get(Path, Query, Headers, AccessToken, Options) ->
    get(Path, Query, Headers, AccessToken, undefined, Options).

-spec get(httpc:path(), query(), httpc:headers(), access_token(), psu(), options()) -> response().
get(Path, Query, Headers, AccessToken, PSU, Options) ->
    request(get,
            {request_uri(Path, Query ++ default_query(Options), Options),
             Headers ++ request_headers(AccessToken) ++ psu_headers(PSU)
            }, Options).

-spec post(httpc:path(), query(), httpc:headers(), jsx:json_term(), access_token(), options()) -> response().
post(Path, Query, Headers, Body, AccessToken, Options) ->
    post(Path, Query, Headers, Body, AccessToken, undefined, Options).

-spec post(httpc:path(), query(), httpc:headers(), jsx:json_term(), access_token(), psu(), options()) -> response().
post(Path, Query, Headers, Body, AccessToken, PSU, Options) ->
    request(post,
            {request_uri(Path, Query ++ default_query(Options), Options),
             Headers ++ request_headers(AccessToken) ++ psu_headers(PSU),
             "application/json",
             jsx:encode(Body)
            }, Options).

-spec put(httpc:path(), query(), httpc:headers(), jsx:json_term(), access_token(), options()) -> response().
put(Path, Query, Headers, Body, AccessToken, Options) ->
    put(Path, Query, Headers, Body, AccessToken, undefined, Options).

-spec put(httpc:path(), query(), httpc:headers(), jsx:json_term(), access_token(), psu(), options()) -> response().
put(Path, Query, Headers, Body, AccessToken, PSU, Options) ->
    request(put,
            {request_uri(Path, Query ++ default_query(Options), Options),
             Headers ++ request_headers(AccessToken) ++ psu_headers(PSU),
             "application/json",
             jsx:encode(Body)
            }, Options).

-spec delete(httpc:path(), query(), httpc:headers(), access_token(), options()) -> response().
delete(Path, Query, Headers, AccessToken, Options) ->
    delete(Path, Query, Headers, AccessToken, undefined, Options).

-spec delete(httpc:path(), query(), httpc:headers(), access_token(), psu(), options()) -> response().
delete(Path, Query, Headers, AccessToken, PSU, Options) ->
    request(delete, 
            {request_uri(Path, Query ++ default_query(Options), Options),
             Headers ++ request_headers(AccessToken) ++ psu_headers(PSU)
            }, Options).

-spec tpp_headers(tpp()) -> httpc:headers().
tpp_headers(#{redirect_preferred := RedirectPreferred,
              redirect_uri := RedirectUri,
              nok_redirect_uri := NokRedirectUri,
              explicit_authorisation_preferred := ExplicitAuthorisationPreferred
             }) ->
    [{"TPP-Redirect-Preferred", atom_to_list(RedirectPreferred)},
     {"TPP-Redirect-URI", binary_to_list(RedirectUri)},
     {"TPP-Nok-Redirect-URI", binary_to_list(NokRedirectUri)},
     {"TPP-Explicit-Authorisation-Preferred", atom_to_list(ExplicitAuthorisationPreferred)}
    ].

-spec decode_links(list(atom()), map()) -> links(atom()).
decode_links(Rels, Links) ->
    maps:from_list(
      lists:filtermap(fun(Rel) ->
                        case maps:get(atom_to_binary(Rel), Links, undefined) of
                            #{<<"href">> := Href} -> {true, {Rel, Href}};
                            undefined -> false
                        end
                end, Rels)).


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec is_sandbox(options() | bic()) -> boolean().
is_sandbox(#{bic := Bic}) -> is_sandbox(Bic);
is_sandbox('SANDSESS') -> true;
is_sandbox('SANDLT22') -> true;
is_sandbox('SANDLV22') -> true;
is_sandbox('SANDEE2X') -> true;
is_sandbox(_) -> false.

-spec authorization_bearer(binary()) -> string().
authorization_bearer(AccessToken) ->
    io_lib:format("Bearer ~s", [AccessToken]).

-spec generate_request_id() -> string().
generate_request_id() ->
    binary_to_list(uuid:get_v4_urandom()).

-spec default_query(options()) -> list({unicode:chardata(),unicode:chardata()}).
default_query(Options) ->
    [{<<"bic">>, atom_to_binary(maps:get(bic, Options))},
     {<<"app-id">>, maps:get(client_id, Options)}
    ].

-spec request_headers(access_token()) -> httpc:headers().
request_headers(AccessToken) ->
    [{"Authorization", authorization_bearer(AccessToken)},
     {"X-Request-ID", generate_request_id()},
     {"Date", httpd_util:rfc1123_date()}
                                                %{"Signature", "xx"},
                                                %{"Digest", "xx"}
    ].

-spec psu_headers(psu()) -> httpc:headers().
psu_headers(undefined) -> [];
psu_headers(PSU) ->
    [{"PSU-IP-Address", inet:ntoa(maps:get(ip_address, PSU))},
     {"PSU-IP-Port", integer_to_list(maps:get(ip_port, PSU))},
     {"PSU-Http-Method", atom_to_list(maps:get(http_method, PSU))},
     {"PSU-User-Agent", maps:get(user_agent, PSU)}
    ].

-spec request(httpc:method(), httpc:request(), options()) -> response().
request(Method, Request, Options) ->
    logger:debug(#{method => Method,
                   request => Request,
                   options => Options
                  }),
    {ok,{{_,Status,_},ResponseHeaders,ResponseBody}} =
        httpc:request(Method, Request,
                      [{timeout, maps:get(timeout, Options, ?TIMEOUT)}],
                      []),
    logger:debug(#{status => Status,
                   headers => ResponseHeaders,
                   body => ResponseBody
                  }),
    handle_response(Status, ResponseHeaders, ResponseBody).

-spec handle_response(httpc:status_code(), httpc:headers(), string() | binary()) ->
          response().
handle_response(Status, ResponseHeaders, []) -> {Status,ResponseHeaders,[]};
handle_response(Status, ResponseHeaders, ResponseBody) ->
    case content_type(ResponseHeaders) of
        {"application/json", _} ->
            BinaryBody = unicode:characters_to_binary(ResponseBody, utf8),
            {Status,ResponseHeaders,jsx:decode(BinaryBody)};
        "application/json" ->
            {Status,ResponseHeaders,jsx:decode(iolist_to_binary(ResponseBody))};
        _ ->
            {Status,ResponseHeaders,ResponseBody}
    end.

content_type(Headers) ->
    case proplists:get_value("content-type", Headers) of
        undefined -> undefined;
        ContentType when is_list(ContentType) ->
            case string:split(string:lowercase(ContentType), ";") of
                [MimeType] -> MimeType;
                [MimeType, Charset] -> {MimeType, Charset}
            end
    end.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

api_path_test() ->
    ?assertEqual("sandbox/v3/accounts", api_path(v3_accounts, #{bic => 'SANDSESS'})),
    ?assertEqual("sandbox/v3/accounts/123/transactions", api_path(v3_accounts, ["123","transactions"], #{bic => 'SANDSESS'})),
    ?assertEqual("v3/accounts", api_path(v3_accounts, #{bic => 'SWEDSESS'})).

content_type_test() ->
    ?assertEqual("application/json", content_type([{"content-type", "application/json"}])),
    ?assertEqual({"application/json","charset=utf-8"}, content_type([{"content-type", "application/json;charset=utf-8"}])),
    ?assertEqual({"application/json","charset=utf8"}, content_type([{"content-type", "application/json;charset=utf8"}])),
    ?assertEqual({"application/json","charset=utf-8"}, content_type([{"content-type", "application/json;charset=UTF-8"}])),
    ?assertEqual(undefined, content_type([])).

decode_links_test() ->
    ?assertEqual(#{test => <<"#">>}, decode_links([test, foo], #{<<"test">> => #{<<"href">> => <<"#">>}})).

-endif.%% TESTS
