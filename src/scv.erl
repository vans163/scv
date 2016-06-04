-module(scv).

-export([start/0, start/1]). 
-export([good_to_go/1, recurse_all_folders/1, hotload_file/1]).
-export([remote_watcher/1]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.

good_to_go(Folder) -> supervisor:start_child({global, scv_sup}, [Folder]).


start()-> start(#{}).
start(#{}=Opts) ->
    SrcPaths = maps:get(source_paths, Opts, ["./src", "./lib"]),
    BeamPath = maps:get(beam_path, Opts, "./ebin"),
    RemoteCompile = maps:get(remote_compile, Opts, true),
    RemoteNodes = maps:get(remote_nodes, Opts, []),

    spawn(scv, remote_watcher, [RemoteNodes]),

    FoldersToMonitor = recurse_all_folders(SrcPaths),

    scv_sup:start_link(),
    spawn_initial(lists:merge(SrcPaths,FoldersToMonitor))
    .

remote_watcher([]) -> pass;
remote_watcher(Nodes) ->
    ConnectedNodes = nodes(),
    lists:foreach(fun(Node) ->
            case lists:member(Node, ConnectedNodes) of
                true -> pass;
                false -> 
                    Ping = net_adm:ping(Node),
                    ?PRINT({"Pinging", Node, Ping}),
                    case Ping of
                        pang -> pass;
                        pong ->
                            %Node joined our cluster, hotload all beams
                            BeamFiles = filelib:wildcard("./**/*.beam"),
                            lists:foreach(fun(BeamPath) ->
                                    [_, {module, Module}, _] = beam_lib:info(BeamPath),
                                    {_, Binary, Filename} = code:get_object_code(Module),
                                    {_, []} = rpc:multicall(code, load_binary, [Module, Filename, Binary]),
                                    ?PRINT({"Netloaded:", Filename, Module})
                                end,
                                BeamFiles
                            )

                    end
            end
        end,
        Nodes
    ),
    timer:sleep(2000),
    remote_watcher(Nodes)
    .


hotload_file(FullPath) ->
    case filename:extension(FullPath) of
        ".erl" -> 
            ?PRINT({"Got erlang file", FullPath}),
            case compile:file(FullPath, {outdir, "./ebin"}) of
                {ok, Mod} ->
                    {_, Binary, Filename} = code:get_object_code(Mod),
                    {_, []} = rpc:multicall(code, load_binary, [Mod, Filename, Binary]);
                _ -> pass
            end;

        ".ex" -> 
            ?PRINT({"Got elixir file", FullPath}),
            %{EExTest.Compiled, <<70, 79, 82, 49, ...>>}
            try
                %Elixir.Code.load_file crashes if it fails to compile, ignore it
                ModList = apply('Elixir.Code', 'load_file', [unicode:characters_to_binary(FullPath)]),
                lists:foreach(fun({Module, Bin}) ->
                    {_, []} = rpc:multicall(code, load_binary, [Module, FullPath, Bin])
                end, ModList)
            of
                _ -> pass
            catch
                error:E -> ?PRINT({"Compile error", E})
            end;

        _ -> pass
    end
    .

spawn_initial(Folders) ->
    lists:foreach(fun(Folder) ->
            good_to_go(Folder)
        end,
        Folders
    ).

recurse_all_folders(PathList) when is_list(PathList) ->
    lists:foldl(fun(Path, Acc) ->
            case filelib:is_dir(Path) of
                true -> recurse_all_folders(Path, Acc);
                false -> Acc
            end
        end,
        [],
        PathList
    ).
recurse_all_folders(Folder, FolderList) ->
    {ok, Files} = file:list_dir(Folder),
    lists:foldl(fun(File, Acc) ->
            FullPath = filename:join([Folder, File]),
            case filelib:is_dir(FullPath) of
                true -> 
                    NewAcc = recurse_all_folders(FullPath, Acc),
                    [FullPath|NewAcc];
                false -> Acc
            end
        end,
        FolderList,
        Files
    )
    .

