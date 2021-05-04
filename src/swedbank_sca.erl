% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank Strong Customer Authentication (SCA).

-module(swedbank_sca).

-export([start_authorisation/5,
         authorisation_status/3,
         select_authentication_method/4,
         sandbox_sign/2,
         decode_sca_method/2
        ]).

-export_type([authentication_object/0,
              authorisation/0,
              sca_status/0,
              authentication_method/0,
              authentication_method_id/0,
              psu_data/0,
              authentication_challenge/0
             ]).


%%%%%%%%%%%
%% TYPES %%
%%%%%%%%%%%

-type authentication_object() :: #{authentication_method_id := authentication_method_id(),
                                   authentication_type := binary()
                                  }.
-type authorisation() :: #{authorisation_id := binary(),
                           sca_status := sca_status(),
                           sca_methods => list(authentication_object()),
                           chosen_sca_method => authentication_object(),
                           links := swedbank_client:links(scaRedirect | scaStatus | selectAuthenticationMethod)
                          }.
-type sca_status() :: received | started | finalised | failed.
-type authentication_method() :: {authentication_method_id(), psu_data()}.
-type authentication_method_id() :: binary().
-type psu_data() :: #{personal_id => binary(),
                      phone_number => binary()
                     }.
-type authentication_challenge() :: #{challenge_data => challenge_data(),
                                      chosen_sca_method := authentication_object(),
                                      psu_message := binary(),
                                      sca_status := sca_status(),
                                      tpp_messages => list(add_recipient()),
                                      links => swedbank_client:links(scaStatus)
                                     }.
-type challenge_data() :: #{auto_start_token => binary(),
                            code := binary(),
                            image_link => binary()
                           }.
-type add_recipient() :: #{category := binary(),
                           code := binary(),
                           text := binary()
                          }.


%%%%%%%%%
%% API %%
%%%%%%%%%

-spec start_authorisation(#{links := #{startAuthorisation := binary()}},
                          swedbank_client:tpp(),
                          swedbank_client:access_token(),
                          swedbank_client:psu(),
                          swedbank_client:options()) ->
          {ok, authorisation()} | {error, swedbank_client:request_error()}.
start_authorisation(#{links:=#{startAuthorisation:=Href}}, TPP, AccessToken, PSU, Options) ->
    case swedbank_client:post(Href, [], swedbank_client:tpp_headers(TPP), [], AccessToken, PSU, Options) of
        {201,_Headers,Body} ->
            {ok, decode_authorisation(Body)};
        Response ->
            {error, Response}
    end.

-spec authorisation_status(#{links := #{scaStatus := binary()}},
                           swedbank_client:access_token(),
                           swedbank_client:options()) ->
          {ok, authorisation()} | {error, swedbank_client:request_error()}.
authorisation_status(#{links:=#{scaStatus:=Href}}, AccessToken, Options) ->
    case swedbank_client:get(Href, [], [], AccessToken, Options) of
        {201,_Headers,#{<<"scaStatus">>:=ScaStatus}} ->
            {ok, decode_sca_status(ScaStatus)};
        Response ->
            {error, Response}
    end.

-spec select_authentication_method(authentication_method(),
                                   #{links := #{selectAuthenticationMethod := binary()}},
                                   swedbank_client:access_token(),
                                   swedbank_client:options()) ->
          {ok, authentication_challenge()} | {error, swedbank_client:request_error()}.
select_authentication_method(AuthenticationMethod, #{links:=#{selectAuthenticationMethod:=Href}}, AccessToken, Options) ->
    case swedbank_client:put(Href, [], [], encode_authentication_method(AuthenticationMethod), AccessToken, Options) of
        {200,_Headers,Body} ->
            {ok, decode_authentication_challenge(Body)};
        Response ->
            {error, Response}
    end.

-spec sandbox_sign(#{links := #{scaRedirect := binary()}},
                   swedbank_client:options()
                  ) -> ok | {error, swedbank_client:request_error()}.
sandbox_sign(#{links := #{scaRedirect := ScaRedirect}}, Options) ->
    #{path := Path} = uri_string:parse(ScaRedirect),
    Token = lists:last(string:split(Path, "/", all)),
    Body = [{<<"token">>, Token},
            {<<"bic">>, atom_to_binary(maps:get(bic, Options))},
            {<<"action">>, <<"sign">>}
           ],
    Uri = swedbank_client:request_uri(swedbank_client:api_path(sandbox_sign, Options), [], Options),
    case httpc:request(
           post,
           {Uri, [], "application/x-www-form-urlencoded", uri_string:compose_query(Body)},
           [{autoredirect, false}], []) of 
        {ok, {{_,200,_},_Headers,_ResponseBody}} -> ok;
        Response -> {error, Response}
    end.

-spec decode_sca_method(map(), map()) -> map().
decode_sca_method(Result, #{<<"chosenScaMethod">> := ChosenScaMethod,
                            <<"scaMethods">> := ScaMethods
                           }) ->
    maps:merge(#{chosen_sca_method => decode_authentication_object(ChosenScaMethod),
                 sca_methods => lists:map(fun decode_authentication_object/1, ScaMethods)
                }, Result);
decode_sca_method(Result, _) ->
    Result.


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec encode_authentication_method(authentication_method()) -> jsx:json_term().
encode_authentication_method({MethodId, PsuData}) ->
    #{<<"authenticationMethodId">> => MethodId,
      <<"psuData">> => encode_psu_data(PsuData)
     };
encode_authentication_method(MethodId) ->
    #{<<"authenticationMethodId">> => MethodId}.

-spec encode_psu_data(psu_data()) -> jsx:json_term().
encode_psu_data(#{personal_id := PersonalId,
                  phone_number := PhoneNumber
                 }) ->
    #{<<"personalID">> => PersonalId,
      <<"phoneNumber">> => PhoneNumber
     };
encode_psu_data(#{personal_id := PersonalId}) ->
    #{<<"personalID">> => PersonalId};
encode_psu_data(#{phone_number := PhoneNumber}) ->
    #{<<"phoneNumber">> => PhoneNumber}.

-spec decode_authentication_challenge(map()) -> authentication_challenge().
decode_authentication_challenge(Rest = #{<<"challengeData">> := ChallengeData}) ->
    maps:merge(#{callenge_data => decode_challenge_data(ChallengeData)},
               decode_authentication_challenge(maps:without([<<"challengeData">>], Rest)));
decode_authentication_challenge(Rest = #{<<"tppMessages">> := TppMessages}) ->
    maps:merge(#{tpp_messages => lists:map(fun decode_add_recipient/1, TppMessages)},
               decode_authentication_challenge(maps:without([<<"tppMessages">>], Rest)));
decode_authentication_challenge(#{<<"chosenScaMethod">> := ChosenScaMethod,
                                  <<"psuMessage">> := PsuMessage,
                                  <<"scaStatus">> := ScaStatus,
                                  <<"_links">> := Links
                                 }) ->
    #{chosen_sca_method => decode_authentication_object(ChosenScaMethod),
      psu_message => PsuMessage,
      sca_status => decode_sca_status(ScaStatus),
      links => swedbank_client:decode_links([scaStatus], Links)
     }.

-spec decode_challenge_data(map()) -> challenge_data().
decode_challenge_data(Rest = #{<<"code">> := Code}) ->
    #{auto_start_token => maps:get(<<"autoStartToken">>, Rest, undefined),
      code => Code,
      image_link => maps:get(<<"imageLink">>, Rest, undefined)
     }.

-spec decode_add_recipient(map()) -> add_recipient().
decode_add_recipient(#{<<"category">> := Category,
                       <<"code">> := Code,
                       <<"text">> := Text
                      }) ->
    #{category => Category,
      code => Code,
      text => Text
     }.

-spec decode_authentication_object(map()) -> authentication_object().
decode_authentication_object(#{<<"authenticationMethodId">>:=AuthenticationMethodId,
                               <<"authenticationType">>:=AuthenticationType}) ->
    #{authentication_method_id => AuthenticationMethodId,
      authentication_type => AuthenticationType
     }.

-spec decode_authorisation(map()) -> authorisation().
decode_authorisation(Rest = #{<<"chosenScaMethod">> := ChosenScaMethod,
                              <<"scaMethods">> := ScaMethods
                             }) ->
    maps:merge(#{chosen_sca_method => decode_authentication_object(ChosenScaMethod),
                 sca_methods => lists:map(fun decode_authentication_object/1, ScaMethods)
                }, decode_authorisation(
                     maps:without([<<"chosenScaMethod">>, <<"scaMethods">>], Rest)));
decode_authorisation(Rest = #{<<"authorisationId">>:=AuthorisationId,
                              <<"scaStatus">>:=ScaStatus,
                              <<"_links">>:=Links
                             }) ->
    decode_sca_method(
      #{authorisation_id => AuthorisationId,
        sca_status => decode_sca_status(ScaStatus),
        links => swedbank_client:decode_links(
                   [scaRedirect,
                    scaStatus,
                    selectAuthenticationMethod
                   ], Links)
       }, Rest).

-spec decode_sca_status(binary()) -> sca_status().
decode_sca_status(<<"received">>) -> received;
decode_sca_status(<<"started">>) -> started;
decode_sca_status(<<"finalised">>) -> finalised;
decode_sca_status(<<"failed">>) -> failed.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_authentication_method_test() ->
    ?assertEqual(
       #{<<"authenticationMethodId">> => <<"SMART_ID">>,
         <<"psuData">> =>
             #{<<"personalID">> => <<"string">>,
               <<"phoneNumber">> => <<"string">>
              }
        },
      encode_authentication_method(
        {<<"SMART_ID">>, #{personal_id => <<"string">>, phone_number => <<"string">>}}
       )),
    ?assertEqual(
       #{<<"authenticationMethodId">> => <<"SMART_ID">>},
       encode_authentication_method(<<"SMART_ID">>)
      ).

decode_authorisation_test() ->
    ?assertEqual(
       #{authorisation_id => <<"test">>,
         sca_status => started,
         links => #{scaRedirect => <<"#">>,
                    scaStatus => <<"#">>,
                    selectAuthenticationMethod => <<"#">>
                   }
        }, decode_authorisation(
             #{<<"authorisationId">> => <<"test">>,
               <<"scaStatus">> => <<"started">>,
               <<"_links">> =>
                   #{<<"scaRedirect">> => #{<<"href">> => <<"#">>},
                     <<"scaStatus">> => #{<<"href">> => <<"#">>},
                     <<"selectAuthenticationMethod">> => #{<<"href">> => <<"#">>}
                    }
              })).

decode_sca_method_test() ->
    ?assertEqual(
       #{sca_methods =>
             [#{authentication_method_id => <<"SMART_ID">>,
                authentication_type => <<"Smart-Id">>
               }
             ],
         chosen_sca_method =>
             #{authentication_method_id => <<"SMART_ID">>,
               authentication_type => <<"Smart-Id">>
              },
         test => <<"test">>
        },
      decode_sca_method(
        #{test => <<"test">>},
        #{<<"scaMethods">> =>
             [#{<<"authenticationMethodId">> => <<"SMART_ID">>,
                <<"authenticationType">> => <<"Smart-Id">>
               }
             ],
          <<"chosenScaMethod">> =>
             #{<<"authenticationMethodId">> => <<"SMART_ID">>,
               <<"authenticationType">> => <<"Smart-Id">>
              }
        })).

decode_authentication_challenge_test() ->
    ?assertEqual(
       #{callenge_data =>
             #{auto_start_token => undefined,code => "3748",
               image_link => undefined},
         chosen_sca_method =>
             #{authentication_method_id => <<"SMART_ID">>,
               authentication_type => <<"Smart-Id">>},
         links =>
             #{scaStatus =>
                   <<"/{version}/payments/{payment-product}/123abc/authorisations/123auth567">>},
         psu_message =>
             <<"Approve payment of ${amount} ${currency} to ${receiver} by using your ${scaMethod} device">>,
         sca_status => started
        },
       decode_authentication_challenge(
         #{<<"_links">> =>
               #{<<"scaStatus">> =>
                     #{<<"href">> => <<"/{version}/payments/{payment-product}/123abc/authorisations/123auth567">>}
                },
           <<"challengeData">> => #{ <<"code">> => "3748" },
           <<"chosenScaMethod">> =>
               #{<<"authenticationMethodId">> => <<"SMART_ID">>,
                 <<"authenticationType">> => <<"Smart-Id">>
                },
           <<"psuMessage">> => <<"Approve payment of ${amount} ${currency} to ${receiver} by using your ${scaMethod} device">>,
           <<"scaStatus">> => <<"started">>
          }
        )).

-endif.
