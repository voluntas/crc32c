-module(crc32c).

-export([nif/1, nif/2]).

-on_load(init/0).


-spec init() -> ok.
init() ->
    _ = erlang:load_nif(so_path(), 0),
    ok.


-spec nif(iodata()) -> non_neg_integer().
nif(IoData) ->
    nif(0, IoData).


-spec nif(integer(), iodata()) -> non_neg_integer().
nif(_Acc, _IoData) ->
    erlang:nif_error({crc32c_nif_not_loaded, so_path()}).


-spec so_path() -> string().
so_path() ->
    PrivDir =
        case code:priv_dir(crc32) of
            {error, bad_name} ->
                {ok, Cwd} = file:get_cwd(),
                Priv = filename:join([Cwd, "..", "priv"]),
                case filelib:is_dir(Priv) of
                    true  ->
                        Priv;
                    false ->
                        filename:join(Cwd, "priv")
                end;
            Dir ->
                Dir
        end,
    filename:join([PrivDir, "crc32c"]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [
   {"0", fun() -> ?assertEqual(0, nif(<<>>)) end},
   {"1-9", fun() -> ?assertEqual(16#e3069283, nif("123456789")) end},
   {"a", fun() -> ?assertEqual(16#c1d04330, nif("a")) end},
   {"license", fun() -> ?assertEqual(license_crc(), nif(license_txt())) end},
   {"acc",
     fun() ->
         Bytes = license_txt(),
         Crc = lists:foldl(fun(B, Acc) -> nif(Acc, [B]) end, 0, Bytes),
         ?assertEqual(license_crc(), Crc)
     end}
  ].

-endif.
