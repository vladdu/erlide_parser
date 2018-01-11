-module(sourcer_documents_tests).

-include_lib("eunit/include/eunit.hrl").

text() ->
    "
-module(foo).
bar() ->
    quz(1).
quz(1) ->
    ok;
quz(_) ->
    ok.
    ".

parse_file_test_() ->
    Text = text(),
    [
    ].

chunk(_, [], R) ->
    lists:reverse(R);
chunk(Text, [{Ofs, Len, _}|Lines], R) ->
    Text0 = string:slice(Text, Ofs, Len),
    chunk(Text, Lines, [Text0|R]).
