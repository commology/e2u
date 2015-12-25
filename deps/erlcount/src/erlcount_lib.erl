-module(erlcount_lib).

-export([find_erl/1, valid_regex/1, regex_count/2]).

-include_lib("kernel/include/file.hrl").

%% implementation

%% find all files ending in .erl
find_erl(Directory) ->
    find_erl(Directory, queue:new()).


%% dispatch based on file type.
find_erl(Name, Queue) ->
    {ok, F = #file_info{}} = file:read_file_info(Name),
    case F#file_info.type of
        directory -> handle_directory(Name, Queue);
        regular -> handle_regular_file(Name, Queue);
        _Other -> dequeue_and_run(Queue)
    end.

%% open directories and enqueues files in there
handle_directory(Dir, Queue) ->
    case file:list_dir(Dir) of
        {ok, []} ->
            dequeue_and_run(Queue);
        {ok, Files} ->
            dequeue_and_run(enqueue_many(Dir, Files, Queue))
    end.

%% pop an item from the queue and run it
dequeue_and_run(Queue) ->
    case queue:out(Queue) of
        {empty, _} -> done;
        {{value, File}, NewQueue} -> find_erl(File, NewQueue)
    end.

%% add a bunch of items to the queue
enqueue_many(Path, Files, Queue) ->
    F = fun(File, Q) ->
            queue:in(filename:join(Path, File), Q)
        end,
    lists:foldl(F, Queue, Files).

%% check if the file finishes in .erl
handle_regular_file(Name, Queue) ->
    case filename:extension(Name) of
        ".erl" ->
            {continue, Name, fun() -> dequeue_and_run(Queue) end};
        _NonErl ->
            dequeue_and_run(Queue)
    end.


valid_regex(Re) ->
    try re:run("", Re) of
        _ -> true
    catch
        error:badarg -> false
    end.

regex_count(Re, Str) ->
    case re:run(Str, Re, [global]) of
        nomatch -> 0;
        {match, List} -> length(List)
    end.


