-module(beruang).

-export([
    start_link/0,
    get_ets/2,
    get_ets/3
]).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_ets(atom(), list()) -> {ok, ets:tab()}.
get_ets(TabName, Options) ->
    get_ets(self(), TabName, Options).

-spec get_ets(pid(), atom(), list()) -> {ok, ets:tab()}.
get_ets(Pid, TabName, Options) ->
    gen_server:call(?SERVER, {get_ets, Pid, TabName, Options}).

%% gen_server callbacks

init([]) ->
    {ok, ets:new(?MODULE, [])}.

handle_call({get_ets, Pid, TabName, Options}, _From, State) ->
    Result = get_ets_internal(Pid, TabName, Options, State),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', Tab, _FromPid, _GiftData}, State) ->
    ok = handle_ets_transfer(Tab, State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

get_ets_internal(OwnerPid, EtsName, EtsOptions, State) ->
    try get_or_create_ets(EtsName, EtsOptions, State) of
        Tab ->
            true = ets:give_away(Tab, OwnerPid, undefined),
            {ok, Tab}
    catch
        _:Error ->
            {error, Error}
    end.

get_or_create_ets(EtsName, EtsOptions, State) ->
    case ets:lookup(State, EtsName) of
        [] ->
            ValidOptions = lists:dropwhile(fun invalid_option/1, EtsOptions),
            ets:new(EtsName, [{heir, self(), undefined} | ValidOptions]);
        [{EtsName, Tab}] ->
            ets:delete(State, EtsName),
            Tab
    end.


handle_ets_transfer(Tab, State) ->
    case ets:info(Tab, name) of
        undefined ->
            {error, ets_doesnt_exist};
        EtsName ->
            ets:insert(State, {EtsName, Tab}),
            ok
    end.

valid_option(set)                        -> true;
valid_option(bag)                        -> true;
valid_option(ordered_set)                -> true;
valid_option(duplicate_bag)              -> true;
valid_option(public)                     -> true;
valid_option(private)                    -> true;
valid_option(protected)                  -> true;
valid_option(compressed)                 -> true;
valid_option(named_table)                -> true;
valid_option({write_concurrency, true})  -> true;
valid_option({write_concurrency, false}) -> true;
valid_option({read_concurrency, true})   -> true;
valid_option({read_concurrency, false})  -> true;
valid_option({keypos, Pos}) when is_integer(Pos) -> true;
% valid_option({heir, _, _}) -> false;
% valid_option({heir, none}) -> false;
valid_option(_) -> false.

invalid_option(Option) ->
    not valid_option(Option).
