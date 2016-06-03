-module(scv_gen).
-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.

start_link(Folder) -> gen_server:start(?MODULE, Folder, []).

init(Folder) ->
    ?PRINT({"Scv good to go", Folder}),
    random:seed(os:timestamp()),
    process_flag(trap_exit, true),

    {ok, Wd} = inotify:init(),
    {ok, Fd} = inotify:add_watch(Wd, Folder, 0),

    self() ! tick,
    {ok, #{wd=> Wd, fd=> Fd, dirPath=> Folder}}.


handle_call(Message, From, S) -> {reply, ok, S}.
handle_cast(Message, S) -> {noreply, S}.

handle_info({inotify, Fd, Events, Cookie, FileName}, S=#{dirPath:= DirPath}) ->
    IsDir = lists:member(isdir, Events),
    Create = lists:member(create, Events),
    Delete = lists:member(delete, Events),

    %MovedFrom = lists:member(moved_from, Events),
    %MovedTo = lists:member(moved_to, Events),

    CloseWrite = lists:member(close_write, Events),

    %?PRINT({FileName, Events}),

    case CloseWrite of
        false -> pass;
        true -> 
            ?PRINT({"Wrote to file", FileName}),
            FullPath = filename:join(DirPath, binary_to_list(FileName)),
            scv:hotload_file(FullPath)
    end,

    case {IsDir, Create} of
        {true, true} -> scv:good_to_go(FileName);
        _ -> pass
    end,

    %case {IsDir, Delete} of
    %    {true, true} -> ?PRINT({"Deleted a folder", FileName, Events});
    %    _ -> pass
    %end,

    %We can ignore this apparently moving a folder does not change the descriptor id
    %case {IsDir, MovedFrom} of
    %    {true, true} -> 
    %        ?PRINT({"MovedFrom a folder", FileName, Events, Fd});
    %    _ -> pass
    %end,
    %case {IsDir, MovedTo} of
    %    {true, true} -> 
    %        ?PRINT({"MovedTo a folder", FileName, Events, Fd});
    %    _ -> pass
    %end,

    {noreply, S};

handle_info(tick, S=#{wd:= Wd}) ->
    case inotify:read(Wd) of
        {error, 11} -> 'EAGAIN';
        {ok, Events} ->
            %Reverse the order
            lists:foldr(fun(Event, _) ->
                    self() ! Event
                end,
                [],
                Events
            )
    end,

    erlang:send_after(100, self(), tick),
    {noreply, S};

handle_info(Message, S) -> {noreply, S}.



terminate(_Reason, S) -> 
    ?PRINT({"Terminated", _Reason, S}).

code_change(_OldVersion, S, _Extra) -> {ok, S}. 