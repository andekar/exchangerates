-module(exchangerates_server).

-behaviour(gen_server).

-export([start_link/0, rates/0, rate/1, rate/2, countries/0, country/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(INTERVAL, 60000 * 60 * 24). % One day

-define(EXCHANGE_URL, "http://openexchangerates.org/api/latest.json?app_id=").
-define(COUNTRIES_URL, "http://openexchangerates.org/api/currencies.json?app_id=").

-define(RATES, <<"rates">>).
-define(USD, <<"USD">>).
-define(BASE, <<"base">>).
-define(RATE_DB, rate_db).
-define(COUNTRY_DB, country_db).
-define(ID, id).

rates() ->
    gen_server:call(?MODULE, rates).

rate(CountryCode) ->
    gen_server:call(?MODULE, {rate, CountryCode}).

rate(FromCode, ToCode) ->
    gen_server:call(?MODULE, {rate, FromCode, ToCode}).

countries() ->
    gen_server:call(?MODULE, countries).

country(CountryCode) ->
    gen_server:call(?MODULE, {country, CountryCode}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Id = case application:get_env(exchangerates, id) of
             undefined ->
                 undefined; %crash
             {ok, Val} ->
                 Val
         end,
    %start by calling and then start interval running
    fetch_exchangerates(Id),
    fetch_countries(Id),
    timer:apply_interval(?INTERVAL, ?MODULE, fun fetch_exchangerates/1, [Id]),
    timer:apply_interval(?INTERVAL, ?MODULE, fun fetch_countries/1, [Id]),
    {_, Rates} = dets:open_file("../../exchange_rates.dets",[{type, set}]), %% {rate, amount}
    {_, Countries} = dets:open_file("../../exchange_countries.dets",[{type, set}]), %% {rate, amount}
    {ok, [{?RATE_DB, Rates}, {?COUNTRY_DB, Countries}]}.

handle_call(rates, _From, State) ->
    RatesDB = proplists:get_value(?RATE_DB, State),
    Rates = dets:foldl(fun(Rate, Acc) ->
                               [Rate|Acc] end, [], RatesDB),
    {reply, Rates, State};

handle_call({rate, CountryCode}, _From, State) ->
    RatesDB = proplists:get_value(?RATE_DB, State),
    [{CountryCode, Rate}] = dets:lookup(RatesDB, CountryCode),
    {reply, {CountryCode, Rate}, State};

handle_call({rate, FromCode, ToCode}, _From, State) ->
    RatesDB = proplists:get_value(?RATE_DB, State),
    [{FromCode, FromRate}] = dets:lookup(RatesDB, FromCode),
    [{ToCode, ToRate}] = dets:lookup(RatesDB, ToCode),
    {reply, {res, FromRate / ToRate}, State};

handle_call({country, CountryCode}, _From, State) ->
    CountryDB = proplists:get_value(?COUNTRY_DB, State),
    [{CountryCode, CountryName}] = dets:lookup(CountryDB, CountryCode),
    {reply, {CountryCode, CountryName}, State};

handle_call(countries, _From, State) ->
    CountryDB = proplists:get_value(?COUNTRY_DB, State),
    Countries = dets:foldl(fun(Rate, Acc) ->
                                   [Rate|Acc] end, [], CountryDB),
    {reply, Countries, State};

handle_call(Request, _From, State) ->
    lager:alert("Handle unknown call ~p", [Request]),
    {reply, ok, State}.

handle_cast({update_rates, Rates}, State) ->
    RatesDB = proplists:get_value(?RATE_DB, State),
    lists:foreach(fun(Rate) ->
                   dets:insert(RatesDB, Rate) end, Rates),
    {noreply, State};

handle_cast({update_countries, Countries}, State) ->
    CountryDB = proplists:get_value(?COUNTRY_DB, State),
    lists:foreach(fun(Country) ->
                   dets:insert(CountryDB, Country) end, Countries),
    {noreply, State};

handle_cast(Request, State) ->
    lager:alert("Handle unknown cast ~p", [Request]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    lager:emergency("TERMINATING ~p", [Reason]),
    ok.

code_change(OldVsn, State, Extra) ->
    lager:debug("UPGRADING VERSION ~n~p~n~p~n~p~n",[OldVsn, State, Extra]),
    {ok, State}.

fetch_exchangerates(Id) ->
    Method = get,
    URL = ?EXCHANGE_URL ++ Id,
    Header = [],
    HTTPOptions = [],
    Options = [],
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}}
        = httpc:request(Method, {URL, Header}, HTTPOptions, Options),
    Res = destructify(mochijson2:decode(Body)),
    lager:info("updated exchangerates"),
    ?USD = proplists:get_value(?BASE, Res), %make sure we have the expected base
    case proplists:get_value(?RATES, Res) of
        undefined -> do_nothing;
        Rates -> gen_server:cast(?MODULE, {update_rates, Rates})
    end.

fetch_countries(Id) ->
    Method = get,
    URL = ?COUNTRIES_URL ++ Id,
    Header = [],
    HTTPOptions = [],
    Options = [],
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}}
        = httpc:request(Method, {URL, Header}, HTTPOptions, Options),
    Res = destructify(mochijson2:decode(Body)),
    lager:info("updated country list"),
    true = proplists:is_defined(?USD, Res),
    gen_server:cast(?MODULE, {update_countries, Res}).

destructify(List) when is_list(List)->
    lists:map(fun destructify/1, List);
destructify({struct, Val}) ->
    destructify(Val);
destructify({Key, PossibleList}) ->
    {Key, destructify(PossibleList)};
destructify(Other) ->
    Other.
