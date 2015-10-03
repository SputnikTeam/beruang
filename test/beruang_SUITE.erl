-module(beruang_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [
        retrieve_ets_after_death
    ].

init_per_suite(Config) ->
    ok = application:start(beruang),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(beruang).

retrieve_ets_after_death(_Config) ->
    Pid = spawn(fun() -> create_ets_and_put_data(self()) end),
    ok = receive
        ready -> ok
    after 1000 ->
        timeout
    end,
    false = is_process_alive(Pid),
    {ok, Tab} = beruang:get_ets(very_important_ets, []),
    [{important_data_key, important_data}] = ets:lookup(Tab, important_data_key).

create_ets_and_put_data(Pid) ->
    {ok, Tab} = beruang:get_ets(very_important_ets, []),
    true = ets:insert(Tab, {important_data_key, important_data}),
    Pid ! ready.
