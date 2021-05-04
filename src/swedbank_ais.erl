% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank Account Information Services API (v3).

-module(swedbank_ais).

-export([accounts/4,
         accounts_with_balance/4,
         account/5,
         account_with_balance/5,
         balances/5,
         transactions/5,
         transactions/6
        ]).

-define(TRANSACTIONS_LIMIT_DAYS, 90).

%%%%%%%%%%%
%% TYPES %%
%%%%%%%%%%%

-type accounts() :: list(account()).
-type account() :: #{bban => bban(),
                     cash_account_type := cash_account_type(),
                     currency := currency(),
                     iban := iban(),
                     name => binary(),
                     product := binary(),
                     resource_id := account_id(),
                     balances => list(balance())
                    }.
-type cash_account_type() :: 'CACC'.
-type iban() :: binary().
-type bban() :: binary().
-type account_id() :: binary().
-type balance() :: #{balance_type := balance_type(),
                     balance_amount := balance_amount(),
                     reference_date => calendar:date()
                    }.
-type balance_type() :: authorised | interimAvailable.
-type balance_amount() :: {amount(), currency()}.
-type amount() :: binary().
-type currency() :: binary().
-type account_balances() :: #{account := account_reference(),
                              balances := list(balance())
                             }.
-type account_reference() :: {iban, iban()} |
                             {iban, iban(), owner_name()} |
                             {bban, bban()}.
-type owner_name() :: binary().
-type account_transactions() :: #{account := account_reference(),
                                  transactions := transactions()
                                 }.
-type transactions() :: #{booked := list(transaction()),
                          pending => list(transaction())
                         }.
-type transaction() :: #{transaction_amount := transaction_amount(),
                         balance_after_transaction => balance(),
                         bank_transaction_code => binary(),
                         booking_date => calendar:date(),
                         creditor_account => account_reference(),
                         creditor_id => binary(),
                         creditor_name => binary(),
                         end_to_end_id => binary(),
                         remittance_information_structured => binary(),
                         remittance_information_unstructured => binary(),
                         transaction_id => binary(),
                         ultimate_creditor => binary(),
                         ultimate_debtor => binary(),
                         value_date => calendar:date()
                        }.
-type transaction_amount() :: {amount(), currency()}.
-type transactions_query() :: {calendar:date(), calendar:date()} |
                              {booked, calendar:date(), calendar:date()} |
                              {pending, calendar:date(), calendar:date()}.


%%%%%%%%%
%% API %%
%%%%%%%%%

-spec accounts(swedbank_consent:consent(),
               swedbank_client:access_token(),
               swedbank_client:psu(),
               swedbank_client:options()) ->
          {ok, accounts()} | {error, swedbank_client:request_error()}.
accounts(Consent, AccessToken, PSU, Options) ->
    accounts(false, Consent, AccessToken, PSU, Options).

-spec accounts_with_balance(swedbank_consent:consent(),
                            swedbank_client:access_token(),
                            swedbank_client:psu(),
                            swedbank_client:options()) ->
          {ok, accounts()} | {error, swedbank_client:request_error()}.
accounts_with_balance(Consent, AccessToken, PSU, Options) ->
    accounts(true, Consent, AccessToken, PSU, Options).

-spec accounts(boolean(),
               swedbank_consent:consent(),
               swedbank_client:access_token(),
               swedbank_client:psu(),
               swedbank_client:options()) ->
          {ok, accounts()} | {error, swedbank_client:request_error()}.
accounts(WithBalance, Consent, AccessToken, PSU, Options) ->
    ConsentId = swedbank_consent:consent_id(Consent),
    case swedbank_client:get(
           swedbank_client:api_path(v3_accounts, Options),
           [{"withBalance", atom_to_list(WithBalance)}],
           [{"Consent-ID", binary_to_list(ConsentId)}],
           AccessToken,
           PSU,
           Options) of
        {200,_Headers,Body} ->
            {ok, decode_accounts(Body)};
        Response ->
            {error, Response}
    end.

-spec account(account() | account_id(),
              swedbank_consent:consent(),
              swedbank_client:access_token(),
              swedbank_client:psu(),
              swedbank_client:options()) ->
          {ok, account()} | {error, swedbank_client:request_error()}.
account(Account, Consent, AccessToken, PSU, Options) ->
    account(false, Account, Consent, AccessToken, PSU, Options).


-spec account_with_balance(account() | account_id(),
                           swedbank_consent:consent(),
                           swedbank_client:access_token(),
                           swedbank_client:psu(),
                           swedbank_client:options()) ->
          {ok, account()} | {error, swedbank_client:request_error()}.
account_with_balance(Account, Consent, AccessToken, PSU, Options) ->
    account(true, Account, Consent, AccessToken, PSU, Options).

-spec account(boolean(),
              account() | account_id(),
              swedbank_consent:consent(),
              swedbank_client:access_token(),
              swedbank_client:psu(),
              swedbank_client:options()) ->
          {ok, account()} | {error, swedbank_client:request_error()}.
account(WithBalance, #{resource_id := AccountId}, Consent, AccessToken, PSU, Options) ->
    account(WithBalance, AccountId, Consent, AccessToken, PSU, Options);
account(WithBalance, AccountId, Consent, AccessToken, PSU, Options) ->
    ConsentId = swedbank_consent:consent_id(Consent),
    case swedbank_client:get(
           swedbank_client:api_path(v3_accounts, [binary_to_list(AccountId)], Options),
           [{"withBalance", atom_to_list(WithBalance)}],
           [{"Consent-ID", binary_to_list(ConsentId)}],
           AccessToken,
           PSU,
           Options) of
        {200,_Headers,Body} ->
            {ok, decode_account(Body)};
        Response ->
            {error, Response}
    end.

-spec balances(account() | account_id(),
               swedbank_consent:consent(),
               swedbank_client:access_token(),
               swedbank_client:psu(),
               swedbank_client:options()) ->
          {ok, account_balances()} | {error, swedbank_client:request_error()}.
balances(#{resource_id := AccountId}, Consent, AccessToken, PSU, Options) ->
    balances(AccountId, Consent, AccessToken, PSU, Options);
balances(AccountId, Consent, AccessToken, PSU, Options) ->
    ConsentId = swedbank_consent:consent_id(Consent),
    case swedbank_client:get(
           swedbank_client:api_path(v3_accounts, [binary_to_list(AccountId), "balances"], Options),
           [],
           [{"Consent-ID", binary_to_list(ConsentId)}],
           AccessToken,
           PSU,
           Options) of
        {200,_Headers,Body} ->
            {ok, decode_account_balances(Body)};
        Response ->
            {error, Response}
    end.

-spec transactions(account() | account_id(),
                   swedbank_consent:consent(),
                   swedbank_client:access_token(),
                   swedbank_client:psu(),
                   swedbank_client:options()) ->
          {ok, account_transactions()} | {error, swedbank_client:request_error()}.
transactions(Account, Consent, AccessToken, PSU, Options) ->
    ToDate = swedbank_utils:local_date(),
    FromDate = swedbank_utils:date_x_days_from(-(?TRANSACTIONS_LIMIT_DAYS - 1), ToDate),
    transactions({FromDate, ToDate}, Account, Consent, AccessToken, PSU, Options).

-spec transactions(transactions_query(),
                   account() | account_id(),
                   swedbank_consent:consent(),
                   swedbank_client:access_token(),
                   swedbank_client:psu(),
                   swedbank_client:options()) ->
          {ok, account_transactions()} | {error, swedbank_client:request_error()}.
transactions(Query, #{resource_id := AccountId}, Consent, AccessToken, PSU, Options) ->
    transactions(Query, AccountId, Consent, AccessToken, PSU, Options);
transactions(Query, AccountId, Consent, AccessToken, PSU, Options) ->
    ConsentId = swedbank_consent:consent_id(Consent),
    case swedbank_client:get(
           swedbank_client:api_path(v3_accounts, [binary_to_list(AccountId), "transactions"], Options),
           transactions_query(Query),
           [{"Consent-ID", binary_to_list(ConsentId)}],
           AccessToken,
           PSU,
           Options) of
        {200,_Headers,Body} ->
            {ok, decode_account_transactions(Body)};
        Response ->
            {error, Response}
    end.


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec decode_accounts(map()) -> accounts().
decode_accounts(#{<<"accounts">> := Accounts}) ->
    lists:map(fun decode_account/1, Accounts).

-spec decode_account(map()) -> account().
decode_account(Rest = #{<<"name">> := Name}) ->
    maps:merge(#{name => Name},
               decode_account(maps:without([<<"name">>], Rest)));
decode_account(Rest = #{<<"bban">> := Bban}) ->
    maps:merge(#{bban => Bban},
               decode_account(maps:without([<<"bban">>], Rest)));
decode_account(Rest = #{<<"balances">> := Balances}) ->
    maps:merge(#{balances => decode_balances(Balances)},
               decode_account(maps:without([<<"balances">>], Rest)));
decode_account(#{<<"cashAccountType">> := CashAccountType,
                 <<"currency">> := Currency,
                 <<"iban">> := Iban,
                 <<"product">> := Product,
                 <<"resourceId">> := ResourceId
                }) ->
    #{cash_account_type => decode_cash_account_type(CashAccountType),
      currency => Currency,
      iban => Iban,
      product => Product,
      resource_id => ResourceId
     }.

-spec decode_cash_account_type(binary()) -> cash_account_type().
decode_cash_account_type(<<"CACC">>) ->
    'CACC'.

-spec decode_balances(list(map())) -> list(balance()).
decode_balances(Balances) ->
    lists:map(fun decode_balance/1, Balances).

-spec decode_balance(map()) -> balance().
decode_balance(#{<<"balanceType">> := BalanceType,
                 <<"balanceAmount">> := BalanceAmount,
                 <<"referenceDate">> := ReferenceDate
                }) ->
    #{balance_type => decode_balance_type(BalanceType),
      balance_amount => decode_balance_amount(BalanceAmount),
      reference_date => swedbank_utils:iso_binary_to_date(ReferenceDate)
     }.

-spec decode_balance_type(binary()) -> balance_type().
decode_balance_type(<<"authorised">>) -> authorised;
decode_balance_type(<<"interimAvailable">>) -> interimAvailable.

-spec decode_balance_amount(map()) -> balance_amount().
decode_balance_amount(#{<<"amount">> := Amount,
                        <<"currency">> := Currency
                       }) ->
    {Amount, Currency}.

-spec decode_account_balances(map()) -> account_balances().
decode_account_balances(#{<<"account">> := Account,
                          <<"balances">> := Balances
                         }) ->
    #{account => decode_account_reference(Account),
      balances => decode_balances(Balances)
     }.

-spec decode_account_reference(map()) -> account_reference().
decode_account_reference(#{<<"iban">> := Iban, <<"ownerName">> := OwnerName}) ->
    {iban, Iban, OwnerName};
decode_account_reference(#{<<"iban">> := Iban}) ->
    {iban, Iban};
decode_account_reference(#{<<"bban">> := Bban}) ->
    {bban, Bban}.

-spec decode_account_transactions(map()) -> account_transactions().
decode_account_transactions(#{<<"account">> := Account,
                              <<"transactions">> := Transactions
                             }) ->
    #{account => decode_account_reference(Account),
      transactions => decode_transactions(Transactions)
     }.

-spec decode_transactions(map()) -> transactions().
decode_transactions(Rest = #{<<"pending">> := Pending}) ->
    maps:merge(#{pending => lists:map(fun decode_transaction/1, Pending)},
               decode_transactions(maps:without([<<"pending">>], Rest)));
decode_transactions(#{<<"booked">> := Booked}) ->
    #{booked => lists:map(fun decode_transaction/1, Booked)}.

-spec decode_transaction(map()) -> transaction().
decode_transaction(Rest = #{<<"balanceAfterTransaction">> := Balance}) ->
    maps:merge(#{balance_after_transaction => decode_balance(Balance)},
               decode_transaction(maps:without([<<"balanceAfterTransaction">>], Rest)));
decode_transaction(Rest = #{<<"bankTransactionCode">> := BankTransactionCode}) ->
    maps:merge(#{bank_transaction_code => BankTransactionCode},
               decode_transaction(maps:without([<<"bankTransactionCode">>], Rest)));
decode_transaction(Rest = #{<<"bookingDate">> := BookingDate}) ->
    maps:merge(#{booking_date => swedbank_utils:iso_binary_to_date(BookingDate)},
               decode_transaction(maps:without([<<"bookingDate">>], Rest)));
decode_transaction(Rest = #{<<"valueDate">> := ValueDate}) ->
    maps:merge(#{value_date => swedbank_utils:iso_binary_to_date(ValueDate)},
               decode_transaction(maps:without([<<"valueDate">>], Rest)));
decode_transaction(Rest = #{<<"remittanceInformationStructured">> := Value}) ->
    maps:merge(#{remittance_information_structured => Value},
               decode_transaction(
                 maps:without([<<"remittanceInformationStructured">>], Rest)));
decode_transaction(Rest = #{<<"remittanceInformationUnstructured">> := Value}) ->
    maps:merge(#{remittance_information_unstructured => Value},
               decode_transaction(
                 maps:without([<<"remittanceInformationUnstructured">>], Rest)));
decode_transaction(Rest = #{<<"creditorAccount">> := CreditorAccount}) ->
    maps:merge(#{creditor_account => decode_account_reference(CreditorAccount)},
               decode_transaction(maps:without([<<"creditorAccount">>], Rest)));
decode_transaction(Rest = #{<<"creditorId">> := CreditorId}) ->
    maps:merge(#{creditor_id => CreditorId},
               decode_transaction(maps:without([<<"creditorId">>], Rest)));
decode_transaction(Rest = #{<<"creditorName">> := CreditorName}) ->
    maps:merge(#{creditor_name => CreditorName},
               decode_transaction(maps:without([<<"creditorName">>], Rest)));
decode_transaction(Rest = #{<<"endToEndId">> := EndToEndId}) ->
    maps:merge(#{end_to_end_id => EndToEndId},
               decode_transaction(maps:without([<<"endToEndId">>], Rest)));
decode_transaction(Rest = #{<<"transactionId">> := TransactionId}) ->
    maps:merge(#{transaction_id => TransactionId},
               decode_transaction(maps:without([<<"transactionId">>], Rest)));
decode_transaction(Rest = #{<<"ultimateCreditor">> := UltimateCreditor}) ->
    maps:merge(#{ultimate_creditor => UltimateCreditor},
               decode_transaction(maps:without([<<"ultimateCreditor">>], Rest)));
decode_transaction(Rest = #{<<"ultimateDebtor">> := UltimateDebtor}) ->
    maps:merge(#{ultimate_debtor => UltimateDebtor},
               decode_transaction(maps:without([<<"ultimateDebtor">>], Rest)));
decode_transaction(#{<<"transactionAmount">> := TransactionAmount}) ->
    #{transaction_amount => decode_transaction_amount(TransactionAmount)}.

-spec decode_transaction_amount(map()) -> transaction_amount().
decode_transaction_amount(#{<<"amount">> := Amount,
                            <<"currency">> := Currency
                           }) ->
    {Amount, Currency}.

-spec transactions_query(transactions_query()) -> swedbank_client:query().
transactions_query({booked, DateFrom, DateTo}) ->
    [{"dateFrom", binary_to_list(swedbank_utils:date_to_iso_binary(DateFrom))},
     {"dateTo", binary_to_list(swedbank_utils:date_to_iso_binary(DateTo))},
     {"bookingStatus", "booked"}
    ];
transactions_query({pending, DateFrom, DateTo}) ->
    [{"dateFrom", binary_to_list(swedbank_utils:date_to_iso_binary(DateFrom))},
     {"dateTo", binary_to_list(swedbank_utils:date_to_iso_binary(DateTo))},
     {"bookingStatus", "pending"}
    ];
transactions_query({DateFrom, DateTo}) ->
    [{"dateFrom", binary_to_list(swedbank_utils:date_to_iso_binary(DateFrom))},
     {"dateTo", binary_to_list(swedbank_utils:date_to_iso_binary(DateTo))},
     {"bookingStatus", "both"}
    ].


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

decode_accounts_test() ->
    ?assertEqual(
      [#{cash_account_type => 'CACC',
        currency => <<"SEK">>,
        iban => <<"EE662200001101140677">>,
        product => <<"CURRENT">>,
        resource_id => <<"3dc3d5b3702348489853f5400a64e80f">>
       }],
      decode_accounts(
        #{<<"accounts">> =>
              [#{<<"cashAccountType">> => <<"CACC">>,
                 <<"currency">> => <<"SEK">>,
                 <<"iban">> => <<"EE662200001101140677">>,
                 <<"product">> => <<"CURRENT">>,
                 <<"resourceId">> => <<"3dc3d5b3702348489853f5400a64e80f">>
                }]})),
    ?assertEqual(
      [#{cash_account_type => 'CACC',
         currency => <<"SEK">>,
         iban => <<"EE662200001101140677">>,
         product => <<"CURRENT">>,
         resource_id => <<"3dc3d5b3702348489853f5400a64e80f">>,
         name => <<"My first account">>,
         bban => <<"2200001101140677">>,
         balances => [#{balance_type => authorised,
                        balance_amount => {<<"100.00">>,<<"SEK">>},
                        reference_date => {2019,4,1}
                       }]
       }],
      decode_accounts(
        #{<<"accounts">> => 
              [#{<<"cashAccountType">> => <<"CACC">>,
                 <<"currency">> => <<"SEK">>,
                 <<"iban">> => <<"EE662200001101140677">>,
                 <<"product">> => <<"CURRENT">>,
                 <<"resourceId">> => <<"3dc3d5b3702348489853f5400a64e80f">>,
                 <<"name">> => <<"My first account">>,
                 <<"bban">> => <<"2200001101140677">>,
                 <<"balances">> => [#{<<"balanceType">> => <<"authorised">>,
                                      <<"balanceAmount">> =>
                                          #{<<"amount">> => <<"100.00">>,
                                            <<"currency">> => <<"SEK">>
                                           },
                                      <<"referenceDate">> => <<"2019-04-01">>
                                     }
                                   ]}]})).
                

decode_account_balances_test() ->
    ?assertEqual(
       #{account => {iban, <<"EE662200001101140677">>},
         balances => [#{balance_type => authorised,
                        balance_amount => {<<"100.00">>,<<"SEK">>},
                        reference_date => {2019,4,1}
                       }]
        },
       decode_account_balances(
         #{<<"account">> => #{<<"iban">> => <<"EE662200001101140677">>},
           <<"balances">> => [#{<<"balanceType">> => <<"authorised">>,
                                <<"balanceAmount">> => #{<<"amount">> => <<"100.00">>,
                                                         <<"currency">> => <<"SEK">>
                                                        },
                                <<"referenceDate">> => <<"2019-04-01">>
                               }]
          })).

decode_account_transactions_test() ->
    ?assertEqual(
       #{account => {iban, <<"BB4880000123450002227321">>, <<"John Doe">>},
         transactions => #{booked =>
                               [#{bank_transaction_code => <<"PMNT">>,
                                  booking_date => {2019,4,12},
                                  creditor_account => {iban, <<"BB4880000123450002227005">>},
                                  creditor_name => <<"John Miles">>,
                                  remittance_information_unstructured =>
                                      <<"PIRKINYS 5167999999993932 2019.04.10 0.90 EUR (188888) UAB Selecta LT-03210 Vilnius">>,
                                  transaction_amount => {<<"-0.90">>,<<"EUR">>},
                                  transaction_id => <<"2019041200528830-1">>,
                                  value_date => {2019,4,12}}],
                           pending =>
                               [#{bank_transaction_code => <<"PMNT">>,
                                  booking_date => {2019,2,10},
                                  creditor_account => {iban, <<"EE632200221001126348">>},
                                  creditor_id => <<"10234957">>,
                                  creditor_name => <<"TELIA EESTI AS">>,
                                  end_to_end_id => <<"02201901130557">>,
                                  remittance_information_structured => <<"30340567011">>,
                                  remittance_information_unstructured => <<"e-arve 02201901130557">>,
                                  transaction_amount => {<<"-7.80">>,<<"EUR">>},
                                  transaction_id => <<"2019021000400944-1">>,
                                  ultimate_creditor => <<"UAB">>,
                                  ultimate_debtor => <<"Paul Simpson">>,
                                  value_date => {2019,2,10}
                                 }]
                          }
        },
       decode_account_transactions(
         #{<<"account">> => #{<<"iban">> => <<"BB4880000123450002227321">>,
                              <<"ownerName">> => <<"John Doe">>
                             },
           <<"transactions">> =>
               #{<<"booked">> =>
                     [#{<<"bankTransactionCode">> => <<"PMNT">>,
                        <<"bookingDate">> => <<"2019-04-12">>,
                        <<"creditorAccount">> =>
                            #{<<"iban">> => <<"BB4880000123450002227005">>},
                        <<"creditorName">> => <<"John Miles">>,
                        <<"remittanceInformationUnstructured">> =>
                            <<"PIRKINYS 5167999999993932 2019.04.10 0.90 EUR (188888) UAB Selecta LT-03210 Vilnius">>,
                        <<"transactionAmount">> => #{<<"amount">> => <<"-0.90">>,
                                                     <<"currency">> => <<"EUR">>
                                                    },
                        <<"transactionId">> => <<"2019041200528830-1">>,
                        <<"valueDate">> => <<"2019-04-12">>
                       }],
                 <<"pending">> =>
                     [#{<<"bankTransactionCode">> => <<"PMNT">>,
                        <<"bookingDate">> => <<"2019-02-10">>,
                        <<"creditorAccount">> =>
                            #{<<"iban">> => <<"EE632200221001126348">>},
                        <<"creditorId">> => <<"10234957">>,
                        <<"creditorName">> => <<"TELIA EESTI AS">>,
                        <<"endToEndId">> => <<"02201901130557">>,
                        <<"remittanceInformationStructured">> => <<"30340567011">>,
                        <<"remittanceInformationUnstructured">> => <<"e-arve 02201901130557">>,
                        <<"transactionAmount">> => #{<<"amount">> => <<"-7.80">>,
                                                     <<"currency">> => <<"EUR">>
                                                    },
                        <<"transactionId">> => <<"2019021000400944-1">>,
                        <<"ultimateCreditor">> => <<"UAB">>,
                        <<"ultimateDebtor">> => <<"Paul Simpson">>,
                        <<"valueDate">> => <<"2019-02-10">>
                       }]
                }})).

-endif.
