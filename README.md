# Beruang

[![Build Status](https://travis-ci.org/SputnikTeam/beruang.svg)](https://travis-ci.org/SputnikTeam/beruang)

![Beruang](beruang.png)

Retrieve your ets after restart. Beruang will keep process ets tables when supervisor will restart it.

## Usage

```
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.3  (abort with ^G)
1> application:start(beruang).
ok
2> Tab = beruang:get_ets(subscribers, [protected, set]).
20497
3> ets:insert(Tab, {foo, <<"bar">>}).
true
4> exit(self(), kill).
** exception exit: killed
5> Tab = beruang:get_ets(subscribers, [protected, set]).
20497
6> ets:lookup(Tab, foo).
[{foo,<<"bar">>}]
7>
```

## API

Just replace `ets:new` with `beruang:get_ets`

```erlang
beruang:get_ets(Name, Options) -> ets:tab()
```

* `Name` - must be unique. Beruang uses it as a key
* `Options` - can be any `ets:new/2` options except `heir`. Beruang will overide it. Invalid options will be ignored. When ets is already exists options are ignored as well.
