-module(sourcer_util_tests).

-include_lib("eunit/include/eunit.hrl").

range_test_() ->
  [
    ?_assertEqual(#{start=>#{line=>1,character=>2},
                    'end'=>#{line=>3,character=>4}}, sourcer_util:range({{1,3},{3,5}}))
  ].

print_name_test_() ->
  [
    ?_assertEqual(<<"fff/333">>, sourcer_util:print_name({function, mmm, fff, 333})),
    ?_assertEqual(<<"?fff/333">>, sourcer_util:print_name({macro, mmm, fff, 333})),
    ?_assertEqual(<<"?fff">>, sourcer_util:print_name({macro, mmm, fff, -1})),
    ?_assertEqual(<<"vvv">>, sourcer_util:print_name({var, mmm, vvv})),
    ?_assertEqual(<<"{bar,333}">>, sourcer_util:print_name({bar, 333}))
  ].

kind_test_() ->
  [
    ?_assertEqual(12, sourcer_util:kind(function)),
    ?_assertEqual(2, sourcer_util:kind(something))
  ].
