-module(sourcer_util).

-export([
    pack/1,
    unpack/1,
    join/2,
    get_pos/1,
    reverse2/1,
    get_auto_imported/1,
    add_auto_imported/1,
    get_hover/1,
    convert_refs/2,
    find_def/2,
    range/1,
    print_name/1,
    kind/1
]).

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_db.hrl").

-define(SEP, ";").

unpack(F) ->
    string:tokens(F, ?SEP).

pack(L) ->
    join(L, ?SEP).

reverse2(L) when is_list(L) ->
    lists:reverse([lists:reverse(A) || A <- L]).

join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

add_auto_imported(Imports) ->
    [{erlang, get_auto_imported("")} | Imports].

get_auto_imported(Prefix) when is_list(Prefix) ->
    case catch erlang:module_info(exports) of
        Val when is_list(Val) ->
            lists:filter(fun({N, A}) ->
                                 lists:prefix(Prefix, atom_to_list(N)) andalso
                                     erl_internal:bif(N, A)
                         end, Val);
        _Error ->
            ?D(_Error),
            error
    end.

find_def(Src, []) ->
    null;
find_def(Src, L) ->
    hd(L).

default_answer(completion) ->
    null;
default_answer(completion_resolve) ->
    null;
default_answer(hover) ->
    null;
default_answer(signature_help) ->
    null;
default_answer(_) ->
    [].

range({P1,P2}) ->
    range(P1, P2).

range({L1,C1},{L2,C2}) ->
    #{
        start=>#{line=>L1, character=>C1-1},
        'end'=>#{line=>L2, character=>C2-1}
    }.

convert_refs(Model, URI) ->
    #model{refs=Refs, defs=Defs} = Model,
    ?D(Refs),
    [
        #{
            name=>print_name(Key),
            kind=>kind(element(1, lists:last(Key))),
            location=> #{
                uri=>URI,
                range=>range(Pos)
                }
        } || {Key,Pos} <- Refs
    ] ++
    [
        #{
            name=>print_name(Key),
            kind=>kind(element(1, lists:last(Key))),
            location=>#{
                uri=>URI,
                range=>range(Pos)
                }
        } || {Key,Pos,_,_} <- Defs
    ].

print_name(Data) ->
    case Data of
        {function, _, F, A} ->
            iolist_to_binary(io_lib:format("~s/~w", [F, A]));
        {macro, _, M, A} ->
            case A of
                -1 ->
                    iolist_to_binary(io_lib:format("?~s", [M]));
                _ ->
                    iolist_to_binary(io_lib:format("?~s/~w", [M, A]))
            end;
        {var, _, N} ->
            iolist_to_binary(io_lib:format("~s", [N]));
        _ ->
            iolist_to_binary(io_lib:format("~p", [Data]))
    end.

kind(E) ->
    case lists:keyfind(E, 1, lsp_data:get_data(symbol)) of
        false ->
            2;
        {_, N} ->
            N
    end.

encode_file_changes(Changes) ->
    [encode_file_change(X) || X<-Changes].

encode_file_change(#{type:=1}=Change) ->
    Change#{type=>created};
encode_file_change(#{type:=2}=Change) ->
    Change#{type=>changed};
encode_file_change(#{type:=3}=Change) ->
    Change#{type=>deleted}.

get_pos(#{line:=L, character:=C}) ->
    {L, C+1}.

get_hover({Def,Ref}) ->
    get_def_hover(Def)++get_ref_hover(Ref).

get_def_hover([]) ->
    [];
get_def_hover([{K,_,_,D}]) ->
    Cmt = case maps:is_key(comments, D) of
            true ->
                maps:get(comments, D);
            _ ->
                []
        end,
    ["    <i>",Cmt,"</i>\n\n"].

get_ref_hover([]) ->
    [];
get_ref_hover([R]) ->
    io_lib:format("## ff~n~n*Ref*: ~n```erlang~n~30p~n```~n~n", [R]).
