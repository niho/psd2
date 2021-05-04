% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank PSD2 test suite.

-module(swedbank_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(BIC, 'SANDSESS').
-define(ACCESS_TOKEN, <<"dummyToken">>).
-define(PSU, #{ip_address => {0,0,0,0},
               ip_port => 80,
               http_method => post,
               user_agent => "CommonTest"
              }).
-define(TPP, #{redirect_preferred => true,
               redirect_uri => <<"http://localhost:8080/success">>,
               nok_redirect_uri => <<"http://localhost:8080/failed">>,
               explicit_authorisation_preferred => false
              }).

suite() ->
    [{timetrap,{seconds,30}}].

all() ->
    [{group, swedbank}].

groups() ->
    [{swedbank, [sequence],
      [authorize_test,
       %token_test,
       consent_test,
       accounts_test,
       balances_test,
       transactions_test,
       revoke_consent_test,
       payment_test,
       sca_test
      ]}].

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
    Options = #{client_id => <<"l7e01c4816e2dd468f90b594aae9386a6f">>,
                client_secret => <<"2946bf8c2b6d4a37a55e2c9509c24609">>,
                redirect_uri => <<"http://localhost:8080/">>,
                bic => ?BIC
               },
    [{swedbank_options, Options}] ++ Config.

end_per_suite(Config) ->
    Config.

authorize_test(Config) ->
    Options = ?config(swedbank_options, Config),
    {ok,RedirectUrl} = swedbank_auth:authorize(['PSD2sandbox'], Options),
    #{scheme := "https",
      host := "psd2.api.swedbank.com",
      path := "/sandbox/login/",
      query := _Query
     } = uri_string:parse(RedirectUrl),
    %QueryList = uri_string:dissect_query(Query),
    %NewQuery = proplists:delete("action", QueryList) ++ [{"userId", "190001010101"}],
    %LoginUrl = uri_string:recompose(#{scheme => "https",
    %                                  host => "psd2.api.swedbank.com",
    %                                  path => "/sandbox/login/login-as",
    %                                  query => uri_string:compose_query(NewQuery)}),
    %{ok, {_,_,Body}} = httpc:request(get, {LoginUrl, []}, [], []),
    %logger:info(Body),
    %Bic = proplists:get_value("bic", QueryList),
    %SessionId = proplists:get_value("sessionID", QueryList),
    %SessionData = proplists:get_value("sessionData", QueryList),
    %ConsentBody = uri_string:compose_query(
    %         [{<<"bic">>,Bic},
    %          {<<"action">>,<<"grant">>},
    %          {<<"sessionID">>, SessionId},
    %          {<<"sessionData">>,SessionData}
    %         ]),
    %{ok, {_,Headers,ResponseBody}} = httpc:request(
    %                        post,
    %                        {"https://psd2.api.swedbank.com/psd2/authorize/consent",
    %                         [],
    %                         "application/x-www-form-urlencoded",
    %                         ConsentBody
    %                        }, [{autoredirect, false}], []),
    %logger:info(#{headers => Headers, body => ResponseBody}),
    %{ok, _Token} = swedbank:token(<<"f51276a5-6920-4c6c-9e83-b84b463e1a53">>, Options),
    ok.

consent_test(Config) ->
    Options = ?config(swedbank_options, Config),
    {ok,Consent} = swedbank_consent:create(
                     #{access => available_accounts}, ?TPP, ?ACCESS_TOKEN, ?PSU, Options),
    {ok,received} = swedbank_consent:status(Consent, ?ACCESS_TOKEN, Options),
    {ok,_} = swedbank_consent:details(Consent, ?ACCESS_TOKEN, Options),
    swedbank_sca:sandbox_sign(Consent, Options),
    {ok,valid} = swedbank_consent:status(Consent, ?ACCESS_TOKEN, Options),
    {save_config, [{consent, Consent}]}.

accounts_test(Config) ->
    Options = ?config(swedbank_options, Config),
    {consent_test, [{consent, Consent}]} = ?config(saved_config, Config),
    {ok,[Account | _]} = swedbank_ais:accounts(Consent, ?ACCESS_TOKEN, ?PSU, Options),
    {save_config, [{account, Account}]}.

balances_test(Config) ->
    Options = ?config(swedbank_options, Config),
    {accounts_test, [{account, Account}]} = ?config(saved_config, Config),
    #{iban := Iban} = Account,
    Access =  #{balances => [{iban, Iban}]},
    {ok,Consent} = swedbank_consent:create(
                     #{access => Access}, ?TPP, ?ACCESS_TOKEN, ?PSU, Options),
    swedbank_sca:sandbox_sign(Consent, Options),
    {ok,_Balances} = swedbank_ais:balances(Account, Consent, ?ACCESS_TOKEN, ?PSU, Options),
    {save_config, [{account, Account}]}.

transactions_test(Config) ->
    Options = ?config(swedbank_options, Config),
    {balances_test, [{account, Account}]} = ?config(saved_config, Config),
    #{iban := Iban} = Account,
    Access =  #{transactions => [{iban, Iban}]},
    {ok,Consent} = swedbank_consent:create(
                     #{access => Access}, ?TPP, ?ACCESS_TOKEN, ?PSU, Options),
    swedbank_sca:sandbox_sign(Consent, Options),
    {ok,Transactions} = swedbank_ais:transactions(
                          Account, Consent, ?ACCESS_TOKEN, ?PSU, Options),
    {comment, {Transactions}}.

revoke_consent_test(Config) ->
    Options = ?config(swedbank_options, Config),
    {ok,Consent} = swedbank_consent:create(
                     #{access => available_accounts}, ?TPP, ?ACCESS_TOKEN, ?PSU, Options),
    ok = swedbank_consent:delete(Consent, ?ACCESS_TOKEN, Options),
    ok.

payment_test(Config) ->
    Options = ?config(swedbank_options, Config),
    {ok,Payment} = swedbank_pis:initiate_payment(
                     {se_domestic_credit_transfer,
                      #{creditor_account => {iban, <<"EE662200001101140677">>},
                        instructed_amount => {<<"100.00">>, <<"SEK">>}
                       }
                     }, ?TPP, ?ACCESS_TOKEN, ?PSU, Options),
    swedbank_sca:sandbox_sign(Payment, Options),
    {ok,'ACTC'} = swedbank_pis:payment_status(Payment, ?ACCESS_TOKEN, Options),
    {ok,'CANC'} = swedbank_pis:cancel_payment(Payment, ?ACCESS_TOKEN, Options),
    ok.

sca_test(_Config) ->
    ok.
