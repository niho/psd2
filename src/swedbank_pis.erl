% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank Payment Initiation Services API (v3).

-module(swedbank_pis).

-export([initiate_payment/5,
         payment_status/3,
         cancel_payment/3
        ]).


%%%%%%%%%%%
%% TYPES %%
%%%%%%%%%%%

-type payment_product() :: cross_border_credit_transfer |
                           instant_sepa_credit_transfer |
                           pain_001_sepa_credit_transfer |
                           se_domestic_credit_transfer |
                           se_international_credit_transfer |
                           se_international_express_credit_transfer |
                           se_international_financial_credit_transfer |
                           sepa_credit_transfer.
-type payment_initiation() :: {cross_border_credit_transfer, cross_border_credit_transfer()} |
                              {instant_sepa_credit_transfer, sepa_credit_transfer()} |
                              {pain_001_sepa_credit_transfer, pain_001_sepa_credit_transfer()} |
                              {se_domestic_credit_transfer, se_domestic_credit_transfer()} |
                              {se_international_credit_transfer, se_international_credit_transfer()} |
                              {se_international_express_credit_transfer, se_international_credit_transfer()} |
                              {se_international_financial_credit_transfer, correspondent_bank(), se_international_credit_transfer()} |
                              {sepa_credit_transfer, execution_date(), sepa_credit_transfer()}.
-type cross_border_credit_transfer() :: #{creditor_account := account_reference_sepa(),
                                          creditor_agent => binary(),
                                          creditor_name => binary(),
                                          debtor_account => account_reference_sepa(),
                                          instructed_amount := instructed_amount(),
                                          remittance_information_unstructured => binary(),
                                          requested_execution_date => calendar:date()
                                         }.
-type sepa_credit_transfer() :: #{creditor_account := account_reference_sepa(),
                                  creditor_agent => binary(),
                                  creditor_name := binary(),
                                  debtor_account => account_reference_sepa(),
                                  debtor_identification => debtor_identification(),
                                  end_to_end_identification := binary(),
                                  instructed_amount := instructed_amount(),
                                  remittance_information_structured => #{reference := binary(),
                                                                         reference_type := binary(),
                                                                         reference_type_issuer => binary()
                                                                        },
                                  remittance_information_unstructured => binary()
                                 }.
-type pain_001_sepa_credit_transfer() :: binary(). %% SEPA XML payment initiation based on pain.001.001.03 format.
-type se_domestic_credit_transfer() :: #{creditor_account := account_reference(),
                                         creditor_friendly_name => binary(),
                                         debtor_account => account_reference(),
                                         debtor_account_statement_text => binary(),
                                         execution_date => execution_date(),
                                         instructed_amount := instructed_amount(),
                                         remittance_information_structured => #{reference := binary(),
                                                                                reference_type := 'OCR' | 'MSG'
                                                                               },
                                         remittance_information_unstructured => binary()
                                        }.
-type se_international_credit_transfer() :: #{bic => binary(),
                                              charge_bearer := 'OUR' | 'SHA',
                                              creditor_account := account_reference(),
                                              creditor_address := binary(),
                                              creditor_id => binary(),
                                              creditor_name := binary(),
                                              debtor_account := account_reference(),
                                              debtor_fee_account => account_reference(),
                                              execution_date => execution_date(),
                                              instructed_amount := instructed_amount(),
                                              national_bank_id => binary(),
                                              originator_id => binary(),
                                              originator_payment_reference => binary(),
                                              priority := 'URGENT' | 'NORMAL',
                                              regulatory_reporting_code => binary(),
                                              remittance_information_unstructured := binary()
                                             }.
-type account_reference_sepa() :: {iban, binary()}.
-type account_reference() :: {bban, binary()} |
                             {iban, binary()}.
-type instructed_amount() :: {amount(), currency()}.
-type amount() :: binary().
-type currency() :: binary().
-type debtor_identification() :: #{organisation_identification => binary(),
                                   private_identification => binary(),
                                   scheme_name => 'COID' | 'CUST' | 'TXID' | 'NIDN'
                                  }.
-type correspondent_bank() :: swift_id().
-type swift_id() :: binary().
-type execution_date() :: calendar:date().
-type payment_response() :: #{payment_id := binary(),
                              transaction_status := transaction_status(),
                              sca_methods => list(swedbank_consent:authentication_object()),
                              chosen_sca_method => swedbank_consent:authentication_object(),
                              links := swedbank_client:links(
                                         scaRedirect |
                                         scaStatus |
                                         self |
                                         startAuthorisation |
                                         startAuthorisationWithAuthenticationMethodSelection |
                                         status
                                        )
                             }.
-type transaction_status() ::
        'ACTC' | % "AcceptedTechnicalValidation"
        'ACSC' | % "AcceptedSettlementCompleted"
        'ACCP' | % "AcceptedCustomerProfile"
        'ACSP' | % "AcceptedSettlementInProcess"
        'PATC' | % "PartiallyAcceptedTechnicalCorrect"
        'CANC' | % "Cancelled"
        'RJCT'.  % "Rejected"


%%%%%%%%%
%% API %%
%%%%%%%%%

-spec initiate_payment(payment_initiation(),
                       swedbank_client:tpp(),
                       swedbank_client:access_token(),
                       swedbank_client:psu(),
                       swedbank_client:options()) ->
          {ok, payment_response()} | {error, swedbank_client:request_error()}.
initiate_payment(Payment, TPP, AccessToken, PSU, Options) ->
    case swedbank_client:post(swedbank_client:api_path(v3_payments, [payment_product_path(Payment)], Options),
                              [],
                              swedbank_client:tpp_headers(TPP),
                              encode_payment_initiation(Payment),
                              AccessToken,
                              PSU,
                              Options) of
        {201,_Headers,Body} ->
            {ok, decode_payment_response(Body)};
        Response ->
            {error, Response}
    end.

-spec payment_status(#{links := #{status := binary()}},
                     swedbank_client:access_token(),
                     swedbank_client:options()) ->
          {ok, transaction_status()} | {error, swedbank_client:request_error()}.
payment_status(#{links := #{status := Href}}, AccessToken, Options) ->
    case swedbank_client:get(Href, [], [], AccessToken, Options) of
        {200,_Headers,#{<<"transactionStatus">>:=TransactionStatus}} ->
            {ok, decode_transaction_status(TransactionStatus)};
        Response ->
            {error, Response}
    end.

-spec cancel_payment(#{links := #{self := binary()}},
                     swedbank_client:access_token(),
                     swedbank_client:options()) ->
          {ok, transaction_status()} | {error, swedbank_client:request_error()}.
cancel_payment(#{links := #{self := Href}}, AccessToken, Options) ->
    case swedbank_client:delete(Href, [], [], AccessToken, Options) of
        {200,_Headers,#{<<"transactionStatus">>:=TransactionStatus}} ->
            {ok, decode_transaction_status(TransactionStatus)};
        Response ->
            {error, Response}
    end.


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec payment_product_path(payment_initiation() | payment_product()) -> string().
payment_product_path({PaymentProduct,_}) -> payment_product_path(PaymentProduct);
payment_product_path({PaymentProduct,_,_}) -> payment_product_path(PaymentProduct);
payment_product_path(cross_border_credit_transfer) -> "cross-border-credit-transfers";
payment_product_path(instant_sepa_credit_transfer) -> "instant-sepa-credit-transfers";
payment_product_path(pain_001_sepa_credit_transfer) -> "pain.001-sepa-credit-transfers";
payment_product_path(se_domestic_credit_transfer) -> "se-domestic-credit-transfers"; 
payment_product_path(se_international_credit_transfer) -> "se-international-credit-transfers"; 
payment_product_path(se_international_express_credit_transfer) -> "se-international-express-credit-transfers";
payment_product_path(se_international_financial_credit_transfer) -> "se-international-financial-credit-transfers"; 
payment_product_path(sepa_credit_transfer) -> "sepa-credit-transfers".

-spec encode_payment_initiation(payment_initiation()) -> jsx:json_term().
encode_payment_initiation({cross_border_credit_transfer, Payment}) ->
    encode_cross_border_credit_transfer(Payment);
encode_payment_initiation({instant_sepa_credit_transfer, Payment}) ->
    encode_sepa_credit_transfer(Payment);
encode_payment_initiation({pain_001_sepa_credit_transfer, Payment}) ->
    encode_pain_001_sepa_credit_transfer(Payment);
encode_payment_initiation({se_domestic_credit_transfer, Payment}) ->
    encode_se_domestic_credit_transfer(Payment);
encode_payment_initiation({se_international_credit_transfer, Payment}) ->
    encode_se_international_credit_transfer(Payment);
encode_payment_initiation({se_international_express_credit_transfer, Payment}) ->
    encode_se_international_credit_transfer(Payment);
encode_payment_initiation({se_international_financial_credit_transfer, CorrespondentBank, Payment}) ->
    encode_se_international_credit_transfer({Payment, CorrespondentBank});
encode_payment_initiation({sepa_credit_transfer, ExecutionDate, Payment}) ->
    encode_sepa_credit_transfer({Payment, ExecutionDate}).

-spec encode_cross_border_credit_transfer(cross_border_credit_transfer()) -> jsx:json_term().
encode_cross_border_credit_transfer(Rest = #{creditor_agent := CreditorAgent}) ->
    maps:merge(#{<<"creditorAgent">> => CreditorAgent},
               encode_cross_border_credit_transfer(maps:without([creditor_agent], Rest)));
encode_cross_border_credit_transfer(Rest = #{creditor_name := CreditorName}) ->
    maps:merge(#{<<"creditorName">> => CreditorName},
               encode_cross_border_credit_transfer(maps:without([creditor_name], Rest)));
encode_cross_border_credit_transfer(Rest = #{debtor_account := DebtorAccount}) ->
    maps:merge(#{<<"debtorAccount">> => encode_account_reference(DebtorAccount)},
               encode_cross_border_credit_transfer(maps:without([debtor_account], Rest)));
encode_cross_border_credit_transfer(Rest = #{remittance_information_unstructured := RemittanceInformationUnstructured}) ->
    maps:merge(#{<<"remittanceInformationUnstructured">> => RemittanceInformationUnstructured},
               encode_cross_border_credit_transfer(maps:without([remittance_information_unstructured], Rest)));
encode_cross_border_credit_transfer(Rest = #{requested_execution_date := ExecutionDate}) ->
    maps:merge(#{<<"requestedExecutionDate">> => swedbank_utils:date_to_iso_binary(ExecutionDate)},
               encode_cross_border_credit_transfer(maps:without([requested_execution_date], Rest)));
encode_cross_border_credit_transfer(#{creditor_account := CreditorAccount,
                                     instructed_amount := InstructedAmount
                                    }) ->
    #{<<"creditorAccount">> => encode_account_reference(CreditorAccount),
      <<"instructedAmount">> => encode_instructed_amount(InstructedAmount)
     }.

-spec encode_sepa_credit_transfer(
        sepa_credit_transfer() |
        {sepa_credit_transfer(), execution_date()}
       ) -> jsx:json_term().
encode_sepa_credit_transfer({Payment, ExecutionDate}) ->
    maps:merge(#{<<"requestedExecutionDate">> => swedbank_utils:date_to_iso_binary(ExecutionDate)},
               encode_sepa_credit_transfer(Payment));
encode_sepa_credit_transfer(Rest = #{creditor_agent := CreditorAgent}) ->
    maps:merge(#{<<"creditorAgent">> => CreditorAgent},
              encode_sepa_credit_transfer(maps:without([creditor_agent], Rest)));
encode_sepa_credit_transfer(Rest = #{debtor_account := DebtorAccount}) ->
    maps:merge(#{<<"debtorAccount">> => encode_account_reference(DebtorAccount)},
              encode_sepa_credit_transfer(maps:without([debtor_account], Rest)));
encode_sepa_credit_transfer(Rest = #{debtor_identification := DebtorIdentification}) ->
    maps:merge(#{<<"debtorIdentification">> => encode_debtor_identification(DebtorIdentification)},
              encode_sepa_credit_transfer(maps:without([debtor_identification], Rest)));
encode_sepa_credit_transfer(Rest = #{remittance_information_structured := RemittanceInformationStructured}) ->
    maps:merge(#{<<"remittanceInformationStructured">> => encode_remittance_information_structured(RemittanceInformationStructured)},
              encode_sepa_credit_transfer(maps:without([remittance_information_structured], Rest)));
encode_sepa_credit_transfer(Rest = #{remittance_information_unstructured := RemittanceInformationUnstructured}) ->
    maps:merge(#{<<"remittanceInformationUnstructured">> => RemittanceInformationUnstructured},
               encode_sepa_credit_transfer(maps:without([remittance_information_unstructured], Rest)));
encode_sepa_credit_transfer(#{creditor_account := CreditorAccount,
                              creditor_name := CreditorName,
                              end_to_end_identification := EndToEndId,
                              instructed_amount := InstructedAmount
                             }) ->
    #{<<"creditorAccount">> => encode_account_reference(CreditorAccount),
      <<"creditorName">> => CreditorName,
      <<"endToEndIdentification">> => EndToEndId,
      <<"instructedAmount">> => encode_instructed_amount(InstructedAmount)
     }.

-spec encode_pain_001_sepa_credit_transfer(pain_001_sepa_credit_transfer()) -> {httpc:content_type(), binary()}.
encode_pain_001_sepa_credit_transfer(Payment) ->
    {"application/xml", Payment}.

-spec encode_se_domestic_credit_transfer(se_domestic_credit_transfer()) -> jsx:json_term().
encode_se_domestic_credit_transfer(Rest = #{creditor_friendly_name := CreditorFriendlyName}) ->
    maps:merge(#{<<"creditorFriendlyName">> => CreditorFriendlyName},
               encode_se_domestic_credit_transfer(maps:without([creditor_friendly_name], Rest)));
encode_se_domestic_credit_transfer(Rest = #{debtor_account := DebtorAccount}) ->
    maps:merge(#{<<"debtorAccount">> => encode_account_reference(DebtorAccount)},
               encode_se_domestic_credit_transfer(maps:without([debtor_account], Rest)));
encode_se_domestic_credit_transfer(Rest = #{debtor_account_statement_text := Text}) ->
    maps:merge(#{<<"debtorAccountStatementText">> => Text},
               encode_se_domestic_credit_transfer(maps:without([debtor_account_statement_text], Rest)));
encode_se_domestic_credit_transfer(Rest = #{execution_date := ExecutionDate}) ->
    maps:merge(#{<<"executionDate">> => swedbank_utils:date_to_iso_binary(ExecutionDate)},
               encode_se_domestic_credit_transfer(maps:without([execution_date], Rest)));
encode_se_domestic_credit_transfer(Rest = #{remittance_information_structured := Info}) ->
    maps:merge(#{<<"remittanceInformationStructured">> => encode_remittance_information_structured(Info)},
               encode_se_domestic_credit_transfer(maps:without([remittance_information_structured], Rest)));
encode_se_domestic_credit_transfer(Rest = #{remittance_information_unstructured := Info}) ->
    maps:merge(#{<<"remittanceInformationUnstructured">> => Info},
               encode_se_domestic_credit_transfer(maps:without([remittance_information_unstructured], Rest)));
encode_se_domestic_credit_transfer(#{creditor_account := CreditorAccount,
                                     instructed_amount := InstructedAmount
                                    }) ->
    #{<<"creditorAccount">> => encode_account_reference(CreditorAccount),
      <<"instructedAmount">> => encode_instructed_amount(InstructedAmount)
     }.

-spec encode_se_international_credit_transfer(
        se_international_credit_transfer() |
        {se_international_credit_transfer(), correspondent_bank()}
       ) -> jsx:json_term().
encode_se_international_credit_transfer({Payment, CorrespondentBank}) ->
    maps:merge(#{<<"correspondentBank">> => CorrespondentBank},
               encode_se_international_credit_transfer(Payment));
encode_se_international_credit_transfer(Rest = #{bic := Bic}) ->
    maps:merge(#{<<"bic">> => Bic},
               encode_se_international_credit_transfer(maps:without([bic], Rest)));
encode_se_international_credit_transfer(Rest = #{creditor_id := CreditorId}) ->
    maps:merge(#{<<"creditorId">> => CreditorId},
               encode_se_international_credit_transfer(maps:without([creditor_id], Rest)));
encode_se_international_credit_transfer(Rest = #{debtor_fee_account := DebtorFeeAccount}) ->
    maps:merge(#{<<"debtorFeeAccount">> => encode_account_reference(DebtorFeeAccount)},
               encode_se_international_credit_transfer(maps:without([debtor_fee_account], Rest)));
encode_se_international_credit_transfer(Rest = #{execution_date := ExecutionDate}) ->
    maps:merge(#{<<"executionDate">> => swedbank_utils:date_to_iso_binary(ExecutionDate)},
               encode_se_international_credit_transfer(maps:without([execution_date], Rest)));
encode_se_international_credit_transfer(Rest = #{national_bank_id := NationalBankId}) ->
    maps:merge(#{<<"nationalBankId">> => NationalBankId},
               encode_se_international_credit_transfer(maps:without([national_bank_id], Rest)));
encode_se_international_credit_transfer(Rest = #{originator_id := OriginatorId}) ->
    maps:merge(#{<<"originatorId">> => OriginatorId},
               encode_se_international_credit_transfer(maps:without([originator_id], Rest)));
encode_se_international_credit_transfer(Rest = #{originator_payment_reference := OriginatorPaymentReference}) ->
    maps:merge(#{<<"originatorPaymentReference">> => OriginatorPaymentReference},
               encode_se_international_credit_transfer(maps:without([originator_payment_reference], Rest)));
encode_se_international_credit_transfer(Rest = #{regulatory_reporting_code := RegulatoryReportingCode}) ->
    maps:merge(#{<<"regulatoryReportingCode">> => RegulatoryReportingCode},
               encode_se_international_credit_transfer(maps:without([regulatory_reporting_code], Rest)));
encode_se_international_credit_transfer(#{charge_bearer := ChargeBearer,
                                          creditor_account := CreditorAccount,
                                          creditor_address := CreditorAddress,
                                          creditor_name := CreditorName,
                                          debtor_account := DebtorAccount,
                                          instructed_amount := InstructedAmount,
                                          priority := Priority,
                                          remittance_information_unstructured := RemittanceInformationUnstructured
                                         }) ->
    #{<<"chargeBearer">> => atom_to_binary(ChargeBearer),
      <<"creditorAccount">> => encode_account_reference(CreditorAccount),
      <<"creditorAddress">> => CreditorAddress,
      <<"creditorName">> => CreditorName,
      <<"debtorAccount">> => encode_account_reference(DebtorAccount),
      <<"instructedAmount">> => encode_instructed_amount(InstructedAmount),
      <<"priority">> => atom_to_binary(Priority),
      <<"remittanceInformationUnstructured">> => RemittanceInformationUnstructured
     }.

-spec encode_instructed_amount(instructed_amount()) -> jsx:json_term().
encode_instructed_amount({Amount, Currency}) ->
    #{<<"amount">> => Amount,
      <<"currency">> => Currency
     }.

-spec encode_account_reference(account_reference()) -> jsx:json_term().
encode_account_reference({bban, Bban}) ->
    #{<<"bban">> => Bban};
encode_account_reference({iban, Iban}) ->
    #{<<"iban">> => Iban}.

encode_remittance_information_structured(#{reference := Reference,
                                           reference_type := ReferenceType
                                          }) when is_atom(ReferenceType) ->
    #{<<"reference">> => Reference,
      <<"referenceType">> => atom_to_binary(ReferenceType)
     };
encode_remittance_information_structured(#{reference := Reference,
                                           reference_type := ReferenceType
                                          }) ->
    #{<<"reference">> => Reference,
      <<"referenceType">> => ReferenceType
     };
encode_remittance_information_structured(#{reference := Reference,
                                           reference_type := ReferenceType,
                                           reference_type_issuer := ReferenceTypeIssuer
                                          }) ->
    #{<<"reference">> => Reference,
      <<"referenceType">> => ReferenceType,
      <<"referenceTypeIssuer">> => ReferenceTypeIssuer
     }.

-spec encode_debtor_identification(debtor_identification()) -> jsx:json_term().
encode_debtor_identification(#{organisation_identification := OrganisationIdentification,
                               private_identification := PrivateIdentification,
                               scheme_name := SchemeName
                              }) ->
    #{<<"organisationIdentification">> => OrganisationIdentification,
      <<"privateIdentification">> => PrivateIdentification,
      <<"schemeName">> => atom_to_binary(SchemeName)
     }.

-spec decode_payment_response(map()) -> payment_response().
decode_payment_response(Rest = #{<<"paymentId">> := PaymentId,
                                 <<"transactionStatus">> := TransactionStatus,
                                 <<"_links">> := Links
                                }) ->
    swedbank_sca:decode_sca_method(
      #{payment_id => PaymentId,
        transaction_status => decode_transaction_status(TransactionStatus),
        links => swedbank_client:decode_links(
                   [scaRedirect,
                    scaStatus,
                    self,
                    startAuthorisation,
                    startAuthorisationWithAuthenticationMethodSelection,
                    status
                   ], Links)
       }, Rest).

-spec decode_transaction_status(binary()) -> transaction_status().
decode_transaction_status(<<"ACTC">>) -> 'ACTC';
decode_transaction_status(<<"ACSC">>) -> 'ACSC';
decode_transaction_status(<<"ACCP">>) -> 'ACCP';
decode_transaction_status(<<"ACSP">>) -> 'ACSP';
decode_transaction_status(<<"PATC">>) -> 'PATC';
decode_transaction_status(<<"CANC">>) -> 'CANC';
decode_transaction_status(<<"RJCT">>) -> 'RJCT'.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_payment_initiation_test() ->
    ?assertEqual(
       #{<<"creditorAccount">> => #{<<"iban">> => <<"EE662200001101140677">>},
         <<"instructedAmount">> =>
             #{<<"amount">> => <<"100.00">>,
               <<"currency">> => <<"SEK">>
              }
        },
       encode_payment_initiation(
         {se_domestic_credit_transfer,
          #{creditor_account => {iban, <<"EE662200001101140677">>},
            instructed_amount => {<<"100.00">>, <<"SEK">>}
           }
         })).

encode_account_reference_test() ->
    ?assertEqual(#{<<"iban">> => <<"EE662200001101140677">>}, encode_account_reference({iban, <<"EE662200001101140677">>})),
    ?assertEqual(#{<<"bban">> => <<"BG 5051-6905">>}, encode_account_reference({bban, <<"BG 5051-6905">>})).

encode_cross_border_credit_transfer_test() ->
    ?assertEqual(
       #{<<"creditorAccount">> =>
             #{<<"iban">> => <<"LT151515152626264848">>},
         <<"debtorAccount">> =>
             #{<<"iban">> => <<"LT707650123456789014">>},
         <<"instructedAmount">> =>
             #{<<"amount">> => <<"12">>,
               <<"currency">> => <<"EUR">>},
         <<"remittanceInformationUnstructured">> => <<"1687622124 Ref Number Merchant">>
        },
       encode_cross_border_credit_transfer(
         #{creditor_account => {iban, <<"LT151515152626264848">>},
           debtor_account => {iban, <<"LT707650123456789014">>},
           instructed_amount => {<<"12">>, <<"EUR">>},
           remittance_information_unstructured => <<"1687622124 Ref Number Merchant">>
          })).

encode_instant_sepa_credit_transfer_test() ->
    ?assertEqual(
       #{<<"creditorAccount">> =>
             #{<<"iban">> => <<"LT151515152626264848">>},
         <<"creditorName">> => <<"Name">>,
         <<"debtorAccount">> =>
             #{<<"iban">> => <<"LT707650123456789014">>},
         <<"endToEndIdentification">> => <<"test">>,
         <<"instructedAmount">> =>
             #{<<"amount">> => <<"12">>,
               <<"currency">> => <<"EUR">>},
         <<"remittanceInformationStructured">> =>
             #{<<"reference">> => <<"1687622124">>,
               <<"referenceType">> => <<"55">>}
        },
       encode_sepa_credit_transfer(
         #{creditor_account => {iban, <<"LT151515152626264848">>},
           creditor_name => <<"Name">>,
           debtor_account => {iban, <<"LT707650123456789014">>},
           end_to_end_identification => <<"test">>,
           instructed_amount => {<<"12">>, <<"EUR">>},
           remittance_information_structured =>
               #{reference => <<"1687622124">>,
                 reference_type => <<"55">>
                }
          })).

encode_pain_001_sepa_credit_transfer_test() ->
    ?assertEqual(
       {"application/xml", <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><paymentInitiationBodyPain001001003>string</paymentInitiationBodyPain001001003>">>},
       encode_pain_001_sepa_credit_transfer(<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><paymentInitiationBodyPain001001003>string</paymentInitiationBodyPain001001003>">>)
      ).

encode_se_domestic_credit_transfer_test() ->
    ?assertEqual(
       #{<<"creditorAccount">> =>
             #{<<"bban">> => <<"BG 5051-6905">>},
         <<"debtorAccount">> =>
             #{<<"iban">> => <<"SE612345987650123456789014">>},
         <<"debtorAccountStatementText">> => <<"TestTPP DAST">>,
         <<"instructedAmount">> =>
             #{<<"amount">> => <<"12">>,
               <<"currency">> => <<"SEK">>
              },
         <<"remittanceInformationStructured">> =>
             #{<<"reference">> => <<"1687622124">>,
               <<"referenceType">> => <<"OCR">>
              }
        },
       encode_se_domestic_credit_transfer(
         #{creditor_account => {bban, <<"BG 5051-6905">>},
           debtor_account => {iban, <<"SE612345987650123456789014">>},
           debtor_account_statement_text => <<"TestTPP DAST">>,
           instructed_amount => {<<"12">>, <<"SEK">>},
           remittance_information_structured =>
               #{reference => <<"1687622124">>,
                 reference_type => <<"OCR">>
                }
          })).

encode_se_international_credit_transfer_test() ->
    ?assertEqual(
       #{<<"chargeBearer">> => <<"SHA">>,
         <<"creditorAccount">> =>
             #{<<"iban">> => <<"DK9605555555555555">>},
         <<"creditorAddress">> => <<"Mock street">>,
         <<"creditorName">> => <<"Name">>,
         <<"debtorAccount">> =>
             #{<<"iban">> => <<"SE4980000833333333333333">>},
         <<"executionDate">> => <<"2019-07-09">>,
         <<"instructedAmount">> =>
             #{<<"amount">> => <<"3.1">>,
               <<"currency">> => <<"SEK">>
              },
         <<"priority">> => <<"NORMAL">>,
         <<"remittanceInformationUnstructured">> => <<"European payment">>
        },
       encode_se_international_credit_transfer(
         #{charge_bearer => 'SHA',
           creditor_account => {iban, <<"DK9605555555555555">>},
           creditor_address => <<"Mock street">>,
           creditor_name => <<"Name">>,
           debtor_account => {iban, <<"SE4980000833333333333333">>},
           execution_date => {2019,7,9},
           instructed_amount => {<<"3.1">>, <<"SEK">>},
           priority => 'NORMAL',
           remittance_information_unstructured => <<"European payment">>
          })).

encode_sepa_credit_transfer_test() ->
    ?assertEqual(
       #{<<"creditorAccount">> =>
             #{<<"iban">> => <<"LT151515152626264848">>},
         <<"creditorName">> => <<"Name">>,
         <<"debtorAccount">> =>
             #{<<"iban">> => <<"LT707650123456789014">>},
         <<"endToEndIdentification">> => <<"test">>,
         <<"instructedAmount">> =>
             #{<<"amount">> => <<"12">>,
               <<"currency">> => <<"EUR">>},
         <<"remittanceInformationStructured">> =>
             #{<<"reference">> => <<"1687622124">>,
               <<"referenceType">> => <<"55">>},
         <<"requestedExecutionDate">> => <<"2021-05-08">>
        },
       encode_sepa_credit_transfer(
         {#{creditor_account => {iban, <<"LT151515152626264848">>},
            creditor_name => <<"Name">>,
            debtor_account => {iban, <<"LT707650123456789014">>},
            end_to_end_identification => <<"test">>,
            instructed_amount => {<<"12">>, <<"EUR">>},
            remittance_information_structured =>
                #{reference => <<"1687622124">>,
                  reference_type => <<"55">>
                 }
           },
          {2021,5,8}
         })).

decode_payment_response_test() ->
    ?assertEqual(
       #{payment_id => <<"test">>,
         transaction_status => 'ACTC',
         links => #{scaRedirect => <<"#">>,
                    scaStatus => <<"#">>,
                    self => <<"#">>,
                    startAuthorisation => <<"#">>,
                    startAuthorisationWithAuthenticationMethodSelection => <<"#">>,
                    status => <<"#">>
                   }
        },
      decode_payment_response(
       #{<<"paymentId">> => <<"test">>,
         <<"transactionStatus">> => <<"ACTC">>,
         <<"_links">> => #{<<"scaRedirect">> => #{<<"href">> => <<"#">>},
                           <<"scaStatus">> => #{<<"href">> => <<"#">>},
                           <<"self">> => #{<<"href">> => <<"#">>},
                           <<"startAuthorisation">> => #{<<"href">> => <<"#">>},
                           <<"startAuthorisationWithAuthenticationMethodSelection">> => #{<<"href">> => <<"#">>},
                           <<"status">> => #{<<"href">> => <<"#">>}
                          }
        })).

-endif.
