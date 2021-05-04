% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Swedbank PSD2 utilities.

-module(swedbank_utils).

-export([date_to_iso_binary/1,
         iso_binary_to_date/1,
         date_90_days_from_now/0,
         date_90_days_from/1,
         date_x_days_from/2,
         local_date/0
        ]).


%%%%%%%%%
%% API %%
%%%%%%%%%

-spec date_to_iso_binary(calendar:date()) -> binary().
date_to_iso_binary({Year,Month,Day}) ->
    iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year,Month,Day])).

-spec iso_binary_to_date(binary()) -> calendar:date().
iso_binary_to_date(Date) ->
    [{Year,_},{Month,_},{Day,_}] =
        lists:map(fun string:to_integer/1, string:split(Date, "-", all)),
    {Year,Month,Day}.

-spec date_90_days_from_now() -> calendar:date().
date_90_days_from_now() ->
    date_90_days_from(local_date()).

-spec date_90_days_from(calendar:date()) -> calendar:date().
date_90_days_from(Date) ->
    date_x_days_from(90, Date).

-spec date_x_days_from(integer(), calendar:date()) -> calendar:date().
date_x_days_from(X, Date) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + X).

-spec local_date() -> calendar:date().
local_date() ->
    {Date,_} = calendar:local_time(),
    Date.


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

date_to_iso_binary_test() ->
    ?assertEqual(<<"2021-05-01">>, date_to_iso_binary({2021,5,1})).

iso_binary_to_date_test() ->
    ?assertEqual({2021,5,1}, iso_binary_to_date(<<"2021-05-01">>)).

date_90_days_from_test() ->
    ?assertEqual({2021,7,29}, date_90_days_from({2021,4,30})).

-endif.
