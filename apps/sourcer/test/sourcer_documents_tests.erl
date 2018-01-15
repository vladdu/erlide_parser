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

parse_file_test_1() ->
    Text = text(),
    {ok, AST, Refs, Lines} = sourcer_documents:parse_file("foo1", Text),
    Chunks = chunk(Text, Lines, []),
    TextLines = sourcer_scan_util:split(Text),
    [
        ?_assertMatch([
                        {attribute,
                            {{1,1,1},13},
                            module,foo,"foo",undefined},
                        {function,
                            {{2,3,15},20},
                            bar,0,[],[],[],
                            {{2,15},3},
                            false},
                        {function,
                            {{4,5,36},17},
                            quz,1,[],undefined,
                            [
                            {clause,
                                {{4,5,36},17},quz,[<<"1">>],<<"(1)">>,{{4,36},3}},
                            {clause,
                                {{6,7,54},17},quz,[<<"_">>],<<"(_)">>,{{6,54},3}}
                            ],
                            {{4,36},3},
                            false}
                        ],
                        AST),
        ?_assertMatch([
                        {ref,{module_def,"foo"},1,13,module,-3,[],false},
                        {ref,{local_call,quz,1},28,3,bar,0,[],false},
                        {ref,{function_def,bar,0},15,3,bar,0,[],false},
                        {ref,{var_def,'_'},58,1,quz,1,"(_)",true},
                        {ref,{function_def,quz,1},36,3,quz,1,[],false}
                    ], Refs),
        ?_assertMatch([{0,1,0},{1,14,1},{15,9,2},{24,12,3},
                        {36,10,4},{46,8,5},{54,10,6},{64,8,7},{72,4,8}
                    ], Lines),
        ?_assertEqual(TextLines, Chunks)
    ].

get_element_test_() ->
    Text = text(),
    Model = sourcer_documents:parse_file("foo1", Text),
    Open = [{<<"foo">>, Text, {Model, []}}],
    [
        ?_assertMatch([],
                        sourcer_documents:get_element(Open, <<"foo">>, #{line=>0, character=>0})
                    ),
        ?_assertMatch({[{module,foo}],{{1,9},{1,12}},none,#{}},
                        sourcer_documents:get_element(Open, <<"foo">>, #{line=>1, character=>9})
                    ),
        ?_assertMatch({[{module,foo},{function,bar,0}],{{2,1},{2,4}},{{2,1},{3,11}},#{}},
                        sourcer_documents:get_element(Open, <<"foo">>, #{line=>2, character=>1})
                    ),
        ?_assertMatch({[{module,foo},{function,quz,1}],{{4,1},{4,4}},{{4,1},{7,7}},#{}},
                        sourcer_documents:get_element(Open, <<"foo">>, #{line=>4, character=>1})
                    )
    ].

chunk(_, [], R) ->
    lists:reverse(R);
chunk(Text, [{Ofs, Len, _}|Lines], R) ->
    Text0 = string:slice(Text, Ofs, Len),
    chunk(Text, Lines, [Text0|R]).
