-module(sourcer_documents).

-export([
    get_sync_value/0,
    open_file/3,
    update_file/3,
    get_text/2,
    get_model/2,
    process_file/2,
    process_watched/2,
    get_line_at_offset/2,
    get_line_info/2
]).

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_db.hrl").

get_sync_value() ->
    1.

open_file(Open, URI, Text) ->
    NewOpen = [{URI, Text, process_file(URI, Text)}|Open],
    ?D({open, URI}),
    NewOpen.

update_file(Open, URI, [#{text:=Text}]) ->
    NewOpen = lists:keyreplace(URI, 1, Open, {URI, Text, process_file(URI, Text)}),
    ?D({change, URI}),
    NewOpen;
update_file(Open, _URI, _Changes) ->
    Open.

get_text(State, URI) ->
    {URI, Text, _Data} = lists:keyfind(URI, 1, State),
    Text.

get_model(State, URI) ->
    case lists:keyfind(URI, 1, State) of
        {URI, _Text, Data} ->
            Data;
        false ->
            []
    end.

process_file(URI, Text) ->
    {sourcer_db:analyse_text(Text), get_line_info(Text)}.

process_watched(#{uri:=URI, type:=1}, List) ->
    %% TODO: start parsing & processing
    [URI|List];
process_watched(#{uri:=_URI, type:=2}, List) ->
    %% TODO: start parsing & processing
    List;
process_watched(#{uri:=URI, type:=3}, List) ->
    lists:delete(URI, List).

get_line_info(Text) ->
    ?D(Text),
    %% we hope there are no mixed newlines
    Lines = binary:split(Text, [<<"\r\n">>, <<"\r">>, <<"\n">>], [global]),
    lists:mapfoldl(
        fun(X, {A, I}) -> L=size(X), {{A, L, I}, {A+L, I+1}} end, 
        {0, 0}, 
        Lines
    ).

get_line_at_offset(Offset, Lines) ->
    L = lists:dropwhile(fun({O,_,_})-> O<Offset end, Lines),
    case L of
        [{_,_,I}|_] -> I;
        _ -> none
    end.

get_line_info(none, _) ->
    undefined;
get_line_info(Index, Lines) ->
    lists:nth(Index+1, Lines).

