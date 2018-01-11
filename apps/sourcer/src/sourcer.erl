-module(sourcer).

-export([
        'initialize'/1,
        'initialized'/2,

        'workspace/didChangeConfiguration'/2,
        'workspace/didChangeWatchedFiles'/2,
        'workspace/didChangeWorkspaceFolders'/2,
        'workspace/symbol'/3,
        'workspace/executeCommand'/3,

        'textDocument/didChange'/3,
        'textDocument/didOpen'/2,
        'textDocument/didClose'/2,
        'textDocument/willSave'/2,
        'textDocument/willSaveWaitUntil'/3,
        'textDocument/didSave'/2,

        'textDocument/completion'/3,
        'completionItem/resolve'/3,
        'textDocument/hover'/3,
        'textDocument/signatureHelp'/3,
        'textDocument/references'/3,
        'textDocument/documentHighlight'/3,
        'textDocument/documentSymbol'/3,
        'textDocument/formatting'/3,
        'textDocument/rangeFormatting'/3,
        'textDocument/onTypeFormatting'/3,
        'textDocument/definition'/3,
        'textDocument/codeAction'/3,
        'textDocument/codeLens'/3,
        'codeLens/resolve'/3,
        'textDocument/documentLink'/3,
        'documentLink/resolve'/3,
        'textDocument/rename'/3
]).

-record(state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        watched_files = [],
        open_files = [],
        initialized = false
        }).

-record(small_state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        open_file
        }).

-define(DEBUG, true).
-include("debug.hrl").

-include("sourcer_db.hrl").

'initialize'(InitializeParams) ->
    %% TODO: should use internal format, to be converted by LSP layer
    Capabilities = #{
        textDocumentSync => sourcer_documents:get_sync_value(),
        hoverProvider => true,
        completionProvider => #{
            resolveProvider => true,
            triggerCharacters => [<<":">>, <<"?">>, <<"#">>]
            },
        signatureHelpProvider => #{
            triggerCharacters => [?LPAR]
            },
        definitionProvider => true,
        referencesProvider => true,
        documentHighlightProvider => true,
        documentSymbolProvider => true,
        workspaceSymbolProvider => true,
        codeActionProvider => true,
        codeLensProvider => #{
            resolveProvider => true
            },
        documentFormattingProvider => true,
        documentRangeFormattingProvider => true,
        documentOnTypeFormattingProvider => #{
            firstTriggerCharacter => ?RCURL,
            moreTriggerCharacters => [?RCURL,<<";">>,<<".">>]
            },
        renameProvider => true,
        documentLinkProvider => #{resolveProvider => false},
        executeCommandProvider => #{commands => []},
        experimental => [],
        workspace => #{workspaceFolders => #{
                        supported => true,
                        changeNotifications => true}
                    }
        },
    Server = #{capabilities => Capabilities},
    {Server, #state{client_capabilities=InitializeParams, server_capabilities=Server}}.

'initialized'(State, #{}) ->
    State#state{initialized=true}.

'workspace/didChangeConfiguration'(State, #{settings:=Settings}) ->
    %lsp_client:workspaceFolders(),
    %% TODO: recompute code path (for db)
    %% TODO: start reparsing & processing
    %% TODO: start compile
    State#state{configuration=Settings}.

'workspace/didChangeWatchedFiles'(State, _Changes) ->
    Watched = State#state.watched_files,
    NewWatched = Watched,
    % TODO: lists:foldl(fun sourcer_documents:process_watched/2, [], Watched),
    %% TODO: start compile
    State#state{watched_files=NewWatched}.

'workspace/didChangeWorkspaceFolders'(State, _Changes) ->
    State.

'textDocument/didOpen'(State, #{textDocument:=Item}) ->
    ?D({open, Item}),
    #{uri:=URI, text:=Text}=Item,
    Open = State#state.open_files,
    NewOpen = sourcer_documents:open_file(State#state.open_files, URI, Text),
    State#state{open_files=NewOpen}.

%% TODO: this is for full sync, handle incremental changes too
'textDocument/didChange'(State, #{textDocument:=Item}, Changes) ->
    ?D({change, Item, Changes}),
    #{uri:=URI} = Item,
    %% TODO: start parsing & processing
    NewOpen = sourcer_documents:update_file(State#state.open_files, URI, Changes),
    %% TODO: start compile
    State#state{open_files=NewOpen}.

'textDocument/didSave'(State, #{textDocument:=#{uri:=_URI}}) ->
    State.

'textDocument/willSave'(State, #{textDocument:=#{uri:=_URI}}) ->
    State.

'textDocument/willSaveWaitUntil'(State,  #{textDocument:=#{uri:=_URI}}, Reporter) ->
    Reporter([]),
    State.

'textDocument/didClose'(State,  #{textDocument:=#{uri:=URI}}) ->
    Open = State#state.open_files,
    NewOpen = lists:keydelete(URI, 1, Open),
    State#state{open_files=NewOpen}.

'workspace/symbol'(_State, _Query, Reporter) ->
    %% symbol = #{name, kind, location, containerName?}}
    Res = [],
    Reporter({value, Res}).

'workspace/executeCommand'(_State, _Query, Reporter) ->
    %% symbol = #{name, kind, location, containerName?}}
    Res = [],
    Reporter({value, Res}).

%% completion_item() :: label, kind?, detail?, documentation?, sortText?, filterText?,
%% insertText?, textEdit? additionalTextEdits?, command? data?

'textDocument/completion'(_State, #{textDocument:=#{uri:=URI}, position:=Position}, Reporter) ->
    Res = #{
        isIncomplete => false,
        items => []
    },
    Reporter({value, Res}).

'completionItem/resolve'(_State, Item, Reporter) ->
    Res = Item,
    Reporter({value, Res}).

'textDocument/hover'(State, #{textDocument:=#{uri:=URI}, position:=Position}, Reporter) ->
    Pos = sourcer_util:get_pos(Position),
    {Model, _} = sourcer_documents:get_model(State#state.open_files, URI),
    Source = sourcer_db:get_element_at_pos(Model, Pos),
    %% [markedstring()]:: String (=markdown)
    Hover = sourcer_util:get_hover(Source),
    Res = #{
        contents => unicode:characters_to_binary(Hover)
        %%, range => lsp_utils:range(_Position, _Position)
    },
    Reporter({value, Res}).

'textDocument/references'(State, #{textDocument:=#{uri:=URI}, position:=Position, context:=Context}, Reporter) ->
    Pos = sourcer_util:get_pos(Position),
    {Model, _} = sourcer_documents:get_model(State#state.open_files, URI),
    Source = sourcer_db:get_element_at_pos(Model, Pos),
    Res = [], % TODO convert_refs(Refs, URI),
    Reporter({value, Res}).

'textDocument/documentHighlight'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/documentSymbol'(State, #{textDocument:=#{uri:=URI}}, Reporter) ->
    ?D(State),
    XX = sourcer_documents:get_model(State#state.open_files, URI),
    ?D({sym, URI, XX}),
    {Refs, _} = XX,
    Res = sourcer_util:convert_refs(Refs, URI),
    Reporter({value, Res}).

'textDocument/definition'(State, #{textDocument:=#{uri:=URI}, position:=Position}, Reporter) ->
    Pos = sourcer_util:get_pos(Position),
    {Model, _} = sourcer_documents:get_model(State#state.open_files, URI),
    {_, Source} = sourcer_db:get_element_at_pos(Model, Pos),
    ?D(Source),
    Res = case Source of
        [Item] ->
            case sourcer_util:find_def(Source, Model#model.defs) of
                {_,P,_,_} ->
                    #{uri=>URI, range=>sourcer_util:range(P)};
                _ ->
                    []
            end;
        [] ->
            []
    end,
    Reporter({value, Res}).

'textDocument/signatureHelp'(_State, _Args, Reporter) ->
    Res = #{
        signatures => [],
        activeSignature => null,
        activeParameter => null
    },
    Reporter({value, Res}).

'textDocument/rename'(_State, _Args, Reporter) ->
    %% #{URI: [edits]}
    Res = #{changes => []},
    Reporter({value, Res}).

'textDocument/formatting'(_State, #{textDocument:=#{uri:=_URI}}, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/rangeFormatting'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/onTypeFormatting'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/codeAction'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'textDocument/codeLens'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'codeLens/resolve'(_State, _Args, Reporter) ->
    Res = #{},
    Reporter({value, Res}).

'textDocument/documentLink'(_State, _Args, Reporter) ->
    Res = [],
    Reporter({value, Res}).

'documentLink/resolve'(_State, _Args, Reporter) ->
    Res = #{},
    Reporter({value, Res}).
