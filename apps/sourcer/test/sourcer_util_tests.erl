-module(sourcer_util_tests).

-include_lib("eunit/include/eunit.hrl").

take_right_test_() ->
  [
    ?_assertEqual([], sourcer_util:take_right([], 2)),
    ?_assertEqual([3 ,4], sourcer_util:take_right([1, 2, 3, 4], 2)),
    ?_assertEqual([1, 2, 3 ,4], sourcer_util:take_right([1, 2, 3, 4], 5))
  ].

binary_join_test_() ->
  [
    ?_assertEqual(<<"a1,a2,a3">>, sourcer_util:binary_join([<<"a1">>, <<"a2">>, <<"a3">>], <<",">>)),
    ?_assertEqual(<<"a1">>, sourcer_util:binary_join([<<"a1">>], <<",">>)),
    ?_assertEqual(<<>>, sourcer_util:binary_join([], <<",">>))
  ]. 
