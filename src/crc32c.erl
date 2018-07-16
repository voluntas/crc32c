-module(crc32c).

-export([nif/1, nif/2]).

-on_load(init/0).

-ifndef(APPLICATION).
-define(APPLICATION, crc32c).
-endif.

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
    case code:priv_dir(?APPLICATION) of
      {error, bad_name} ->
        {ok, Cwd} = file:get_cwd(),
        Priv = filename:join([Cwd, "..", "priv"]),
        case filelib:is_dir(Priv) of
          true  -> Priv;
          false -> filename:join(Cwd, "priv")
        end;
      Dir ->
        Dir
    end,
  filename:join([PrivDir, "crc32c"]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [ {"0", fun() -> ?assertEqual(0, nif(<<>>)) end}
  , {"1-9", fun() -> ?assertEqual(16#e3069283, nif("123456789")) end}
  , {"a", fun() -> ?assertEqual(16#c1d04330, nif("a")) end}
  , {"license", fun() -> ?assertEqual(license_crc(), nif(license_txt())) end}
  , {"acc",
     fun() ->
         Bytes = license_txt(),
         Crc = lists:foldl(fun(B, Acc) -> nif(Acc, [B]) end, 0, Bytes),
         ?assertEqual(license_crc(), Crc)
     end}
  ].

license_crc() ->
  16#7dcde113.

license_txt() ->
"  This software is provided 'as-is', without any express or implied\n"
"  warranty.  In no event will the author be held liable for any damages\n"
"  arising from the use of this software.\n"
"\n"
"  Permission is granted to anyone to use this software for any purpose,\n"
"  including commercial applications, and to alter it and redistribute it\n"
"  freely, subject to the following restrictions:\n"
"\n"
"  1. The origin of this software must not be misrepresented; you must not\n"
"     claim that you wrote the original software. If you use this software\n"
"     in a product, an acknowledgment in the product documentation would be\n"
"     appreciated but is not required.\n"
"  2. Altered source versions must be plainly marked as such, and must not be\n"
"     misrepresented as being the original software.\n"
"  3. This notice may not be removed or altered from any source distribution.".

-endif. % TEST


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
