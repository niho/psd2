% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank Consent API (v3).
-module(swedbank_consent).

-export([consent_id/1,
         create/4,
         create/5,
         delete/3,
         details/3,
         status/3
        ]).

-export_type([consent_request/0,
              accounts_access/0,
              account_reference_iban/0,
              consent/0,
              consent_id/0,
              consent_status/0,
              consent_details/0
             ]).


%%%%%%%%%%%
%% TYPES %%
%%%%%%%%%%%

-type consent_request() :: #{valid_until => calendar:date(),
                             recurring_indicator => boolean(),
                             frequency_per_day => 1..4,
                             combined_service_indicator => boolean(),
                             access := accounts_access()
                            }.
-type accounts_access() :: available_accounts |
                           #{accounts => list(account_reference_iban()),
                             balances => list(account_reference_iban()),
                             transactions => list(account_reference_iban()),
                             transactions_over_90_days => list(account_reference_iban())
                            }.
-type account_reference_iban() :: {iban, swedbank_ais:iban()}.
-type consent() :: consent_response() | consent_id().
-type consent_response() :: #{consent_id := consent_id(),
                              consent_status := consent_status(),
                              links := swedbank_client:links(
                                         scaRedirect |
                                         scaStatus |
                                         startAuthorisation |
                                         startAuthorisationWithAuthenticationMethodSelection |
                                         status
                                        ),
                              chosen_sca_method => swedbank_sca:authentication_object(),
                              sca_methods => list(swedbank_sca:authentication_object())
                             }.
-type consent_id() :: binary().
-type consent_status() :: received |
                          rejected |
                          valid |
                          revoked_by_psu |
                          expired |
                          terminated_by_tpp.
-type consent_details() :: #{access := accounts_access(),
                             consent_status := consent_status(),
                             frequency_per_day := 1..4,
                             last_action_date := calendar:date(),
                             recurring_indicator := boolean(),
                             valid_until := calendar:date(),
                             links := swedbank_client:links(account)
                            }.


%%%%%%%%%
%% API %%
%%%%%%%%%

-spec consent_id(consent()) -> consent_id().
consent_id(#{consent_id := ConsentId}) -> ConsentId;
consent_id(ConsentId) when is_binary(ConsentId) -> ConsentId.

-spec create(consent_request(),
             swedbank_client:tpp(),
             swedbank_client:access_token(),
             swedbank_client:options()) ->
          {ok, consent_response()} |
          {error, swedbank_client:request_error()}.
create(Consents, TPP, AccessToken, Options) ->
    create(Consents, TPP, AccessToken, undefined, Options).

-spec create(consent_request(),
             swedbank_client:tpp(),
             swedbank_client:access_token(),
             swedbank_client:psu(),
             swedbank_client:options()) ->
          {ok, consent_response()} |
          {error, swedbank_client:request_error()}.
create(Consents, TPP, AccessToken, PSU, Options) ->
    case swedbank_client:post(
           swedbank_client:api_path(v3_consents, Options),
           [],
           swedbank_client:tpp_headers(TPP),
           encode_consent_request(Consents),
           AccessToken,
           PSU,
           Options) of
        {201,_Headers,Body} -> {ok, decode_consent_response(Body)};
        Response -> {error, Response}
    end.

-spec delete(consent(),
             swedbank_client:access_token(),
             swedbank_client:options()) ->
          ok | {error, swedbank_client:request_error()}.
delete(Consent, AccessToken, Options) ->
    ConsentId = consent_id(Consent),
    case swedbank_client:delete(
           swedbank_client:api_path(v3_consents, [binary_to_list(ConsentId)], Options),
           [], [], AccessToken, Options) of
        {204,_Headers,_Body} -> ok;
        Response -> {error, Response}
    end.

-spec details(consent(),
              swedbank_client:access_token(),
              swedbank_client:options()) ->
          {ok, consent_details()} | {error, swedbank_client:request_error()}.
details(Consent, AccessToken, Options) ->
    ConsentId = consent_id(Consent),
    case swedbank_client:get(
           swedbank_client:api_path(v3_consents, [binary_to_list(ConsentId)], Options),
           [], [], AccessToken, Options) of
        {200,_Headers,Body} ->
            {ok, decode_consent_details(Body)};
        Response ->
            {error, Response}
    end.

-spec status(consent(),
             swedbank_client:access_token(),
             swedbank_client:options()) ->
          {ok, consent_status()} | {error, swedbank_client:request_error()}.
status(Consent, AccessToken, Options) ->
    ConsentId = consent_id(Consent),
    case swedbank_client:get(
           swedbank_client:api_path(v3_consents, [binary_to_list(ConsentId), "status"], Options),
           [], [], AccessToken, Options) of
        {200,_Headers,#{<<"consentStatus">>:=ConsentStatus}} ->
            {ok, decode_consent_status(ConsentStatus)};
        Response ->
            {error, Response}
    end.


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec encode_consent_request(consent_request()) -> jsx:json_term().
encode_consent_request(Consent) ->
    [{<<"access">>, encode_accounts_access(maps:get(access, Consent))},
     {<<"combinedServiceIndicator">>, maps:get(combined_service_indicator, Consent, false)},
     {<<"frequencyPerDay">>, maps:get(frequency_per_day, Consent, 1)},
     {<<"recurringIndicator">>, maps:get(recurring_indicator, Consent, false)},
     {<<"validUntil">>,
      swedbank_utils:date_to_iso_binary(
        maps:get(valid_until, Consent,
                 swedbank_utils:date_90_days_from_now()))}
    ].

-spec encode_accounts_access(accounts_access()) -> jsx:json_term().
encode_accounts_access(available_accounts) ->
    [{<<"availableAccounts">>, <<"allAccounts">>}];
encode_accounts_access(Access) when is_map(Access) ->
    lists:filter(
      fun({_,X}) -> case X of [] -> false; _ -> true end end,
      [{<<"accounts">>,
        lists:map(fun encode_iban_account/1, maps:get(accounts, Access, []))},
       {<<"balances">>,
        lists:map(fun encode_iban_account/1, maps:get(balances, Access, []))},
       {<<"transactions">>,
        lists:map(fun encode_iban_account/1,
                  maps:get(transactions, Access, []))},
       {<<"transactionsOver90Days">>,
        lists:map(fun encode_iban_account/1,
                  maps:get(transactions_over_90_days, Access, []))}
      ]).

-spec encode_iban_account(account_reference_iban()) -> jsx:json_term().
encode_iban_account({iban, Account}) ->
    [{<<"iban">>, iolist_to_binary(Account)}].

-spec decode_consent_response(map()) -> consent_response().
decode_consent_response(Rest = #{<<"consentId">> := ConsentId,
                                 <<"consentStatus">> := ConsentStatus,
                                 <<"_links">> := Links
                                }) ->
    swedbank_sca:decode_sca_method(
      #{consent_id => ConsentId,
        consent_status => decode_consent_status(ConsentStatus),
        links => swedbank_client:decode_links(
                   [scaRedirect,
                    scaStatus,
                    startAuthorisation,
                    startAuthorisationWithAuthenticationMethodSelection,
                    status
                   ], Links)
       }, Rest).

-spec decode_consent_status(binary()) -> consent_status().
decode_consent_status(<<"received">>) -> received;
decode_consent_status(<<"rejected">>) -> rejected;
decode_consent_status(<<"valid">>) -> valid;
decode_consent_status(<<"revokedByPsu">>) -> revoked_by_psu;
decode_consent_status(<<"expired">>) -> expired;
decode_consent_status(<<"terminatedByTpp">>) -> terminated_by_tpp.

-spec decode_consent_details(map()) -> consent_details().
decode_consent_details(#{<<"access">>:=Access,
                         <<"consentStatus">>:=ConsentStatus,
                         <<"frequencyPerDay">>:=FrequencyPerDay,
                         <<"lastActionDate">>:=LastActionDate,
                         <<"recurringIndicator">>:=RecurringIndicator,
                         <<"validUntil">>:=ValidUntil,
                         <<"_links">>:=Links}) ->
    #{access => decode_accounts_access(Access),
      consent_status => decode_consent_status(ConsentStatus),
      frequency_per_day => FrequencyPerDay,
      last_action_date => swedbank_utils:iso_binary_to_date(LastActionDate),
      recurring_indicator => RecurringIndicator,
      valid_until => swedbank_utils:iso_binary_to_date(ValidUntil),
      links => swedbank_client:decode_links([account], Links)
     }.

-spec decode_access(map()) -> accounts_access().
decode_accounts_access(#{<<"availableAccounts">> := <<"allAccounts">>}) ->
    available_accounts;
decode_accounts_access(Access) ->
    decode_access(Access).

decode_access(Rest = #{<<"accounts">> := Accounts}) ->
    maps:merge(#{accounts => lists:map(fun decode_iban_account/1, Accounts)},
               decode_access(maps:without([<<"accounts">>], Rest)));
decode_access(Rest = #{<<"balances">> := Balances}) ->
    maps:merge(#{balances => lists:map(fun decode_iban_account/1, Balances)},
               decode_access(maps:without([<<"balances">>], Rest)));
decode_access(Rest = #{<<"transactions">> := Transactions}) ->
    maps:merge(#{transactions => lists:map(fun decode_iban_account/1, Transactions)},
               decode_access(maps:without([<<"transactions">>], Rest)));
decode_access(Rest = #{<<"transactionsOver90Days">> := TransactionsOver90Days}) ->
    maps:merge(#{transactions_over_90_days => lists:map(fun decode_iban_account/1, TransactionsOver90Days)},
               decode_access(maps:without([<<"transactionsOver90Days">>], Rest)));
decode_access(_) -> #{}.


-spec decode_iban_account(map()) -> account_reference_iban().
decode_iban_account(#{<<"iban">>:=Iban}) ->
    {iban, Iban}.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

consent_id_test() ->
    ?assertEqual(<<"test">>, consent_id(<<"test">>)),
    ?assertEqual(<<"test">>, consent_id(#{consent_id => <<"test">>})).

encode_consent_request_test() ->
    ?assertEqual(
      [{<<"access">>, [{<<"availableAccounts">>, <<"allAccounts">>}]},
       {<<"combinedServiceIndicator">>, false},
       {<<"frequencyPerDay">>, 1},
       {<<"recurringIndicator">>, false},
       {<<"validUntil">>, <<"2021-05-01">>}],
       encode_consent_request(#{access => available_accounts,
                                valid_until => {2021,5,1}
                               })).

encode_accounts_access_test() ->
    ?assertEqual(
       [{<<"availableAccounts">>, <<"allAccounts">>}],
       encode_accounts_access(available_accounts)),
    ?assertEqual(
       [{<<"accounts">>, [[{<<"iban">>, <<"LT7612345987650123456789014">>}]]}],
       encode_accounts_access(#{accounts => [{iban, "LT7612345987650123456789014"}]})),
    ?assertEqual(
       [{<<"balances">>, [[{<<"iban">>, <<"LT7612345987650123456789014">>}]]}],
       encode_accounts_access(#{balances => [{iban, "LT7612345987650123456789014"}]})),
    ?assertEqual(
       [{<<"transactions">>, [[{<<"iban">>, <<"LT7612345987650123456789014">>}]]}],
       encode_accounts_access(#{transactions => [{iban, "LT7612345987650123456789014"}]})),
    ?assertEqual(
       [{<<"transactionsOver90Days">>, [[{<<"iban">>, <<"LT7612345987650123456789014">>}]]}],
       encode_accounts_access(#{transactions_over_90_days => [{iban, "LT7612345987650123456789014"}]})).

decode_consent_response_test() ->
    ?assertEqual(
       #{consent_id => <<"5306db61-740b-4041-bfcf-c9718c50a0de">>,
         consent_status => received,
         links => #{status => <<"#">>,
                    scaRedirect => <<"#">>,
                    scaStatus => <<"#">>,
                    startAuthorisation => <<"#">>,
                    startAuthorisationWithAuthenticationMethodSelection => <<"#">>
                   }
        }, decode_consent_response(
             #{<<"consentId">> => <<"5306db61-740b-4041-bfcf-c9718c50a0de">>,
               <<"consentStatus">> => <<"received">>,
               <<"_links">> =>
                   #{<<"status">> => #{<<"href">> => <<"#">>},
                     <<"scaRedirect">> => #{<<"href">> => <<"#">>},
                     <<"scaStatus">> => #{<<"href">> => <<"#">>},
                     <<"startAuthorisation">> => #{<<"href">> => <<"#">>},
                     <<"startAuthorisationWithAuthenticationMethodSelection">> => #{<<"href">> => <<"#">>}
                    }
              })).

decode_consent_details_test() ->
    ?assertEqual(
       #{consent_status => received,
        frequency_per_day => 1,
        last_action_date => {2021,5,1},
        recurring_indicator => true,
        valid_until => {2021,7,29},
        access => available_accounts,
        links => #{account => <<"#">>}
       }, decode_consent_details(
            #{<<"consentStatus">> => <<"received">>,
              <<"frequencyPerDay">> => 1,
              <<"lastActionDate">> => <<"2021-05-01">>,
              <<"recurringIndicator">> => true,
              <<"validUntil">> => <<"2021-07-29">>,
              <<"access">> => #{<<"availableAccounts">> => <<"allAccounts">>},
              <<"_links">> => #{<<"account">> => #{<<"href">> => <<"#">>}}
             })).

decode_accounts_access_test() ->
    ?assertEqual(
       #{accounts => [{iban, <<"LT7612345987650123456789014">>}],
         balances => [{iban, <<"LT7612345987650123456789014">>}],
         transactions => [{iban, <<"LT7612345987650123456789014">>}],
         transactions_over_90_days => [{iban, <<"LT7612345987650123456789014">>}]
        },
       decode_accounts_access(
         #{<<"accounts">> => [#{<<"iban">> => <<"LT7612345987650123456789014">>}],
           <<"balances">> => [#{<<"iban">> => <<"LT7612345987650123456789014">>}],
           <<"transactions">> => [#{<<"iban">> => <<"LT7612345987650123456789014">>}],
           <<"transactionsOver90Days">> => [#{<<"iban">> => <<"LT7612345987650123456789014">>}]
          })).

-endif.
