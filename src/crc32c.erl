-module(crc32c).

-export([nif/1, nif/2]).

-on_load(init/0).


-spec init() -> ok.
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    NumScheds = erlang:system_info(schedulers),
    SoPath = filename:join([PrivDir, ?MODULE]),
    erlang:load_nif(SoPath, NumScheds).


-spec nif(iodata()) -> non_neg_integer().
nif(IoData) ->
    nif(0, IoData).


-spec nif(integer(), iodata()) -> non_neg_integer().
nif(_Acc, _IoData) ->
    erlang:nif_error(crc32c_nif_not_loaded).



-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [
   {"0", fun() -> ?assertEqual(0, nif(<<>>)) end},
   {"1-9", fun() -> ?assertEqual(16#e3069283, nif("123456789")) end},
   {"a", fun() -> ?assertEqual(16#c1d04330, nif("a")) end}
  ].

-endif.
