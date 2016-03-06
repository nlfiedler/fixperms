%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015-2016 Nathan Fiedler
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% Fix permissions of files and directories.
%%
-module(fixperms).

-export([main/1]).

-include_lib("kernel/include/file.hrl").

main(Args) ->
    OptSpecList = [
        {help,    $h, "help",    boolean,         "display usage"},
        {version, $v, "version", boolean,         "display version information"},
        {file,    $f, "file",    {string, "644"}, "permissions for files"},
        {dir,     $d, "dir",     {string, "755"}, "permissions for directories"},
        {exclude, $x, "exclude", {string, ""},    "entries to ignore, comma separated"}
    ],
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            maybe_help(proplists:get_bool(help, Options), OptSpecList, Options, NonOptArgs);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, "fixperms")
    end,
    ok.

% Handle the --help optional command-line flag.
maybe_help(true, OptSpecList, _Options, _NonOptArgs) ->
    getopt:usage(OptSpecList, "fixperms <path> ...");
maybe_help(false, OptSpecList, Options, NonOptArgs) ->
    maybe_version(proplists:get_bool(version, Options), OptSpecList, Options, NonOptArgs).

% Handle the --version optional command-line flag.
maybe_version(true, _OptSpecList, _Options, _NonOptArgs) ->
    ok = application:load(fixperms),
    {ok, Keys} = application:get_all_key(fixperms),
    Version = proplists:get_value(vsn, Keys),
    io:format("fixperms version ~p~n", [Version]);
maybe_version(false, _OptSpecList, Options, NonOptArgs) ->
    % Do the real work of this script.
    case length(NonOptArgs) of
        0 ->
            io:format("Missing required path(s), see help.~n");
        _ ->
            ValidOpts = validate_args(Options),
            FilePerm = proplists:get_value(file, ValidOpts),
            DirPerm = proplists:get_value(dir, ValidOpts),
            Excludes = proplists:get_value(exclude, ValidOpts),
            ProcessPath = fun(Path) ->
                case filelib:is_dir(Path) of
                    true  ->
                        walk_tree(Path, Excludes, DirPerm, FilePerm);
                    false ->
                        io:format("Path does not exist: ~s~n", [Path])
                end
            end,
            lists:foreach(ProcessPath, NonOptArgs)
    end.

% Ensure the parsed command line arguments are valid, return a proplist
% with validated values.
validate_args(Options) ->
    FilePermStr = proplists:get_value(file, Options),
    FilePerm = (catch list_to_integer(FilePermStr, 8)),
    if not is_integer(FilePerm); FilePerm > 511; FilePerm < 256 ->
            io:format("Invalid file permissions: ~s~n", [FilePermStr]),
            erlang:halt();
        true -> ok
    end,
    DirPermStr = proplists:get_value(dir, Options),
    DirPerm = (catch list_to_integer(DirPermStr, 8)),
    if not is_integer(DirPerm); DirPerm > 511; DirPerm < 256 ->
            io:format("Invalid directory permissions: ~s~n", [DirPermStr]),
            erlang:halt();
        true -> ok
    end,
    ExcludeStr = proplists:get_value(exclude, Options),
    Excludes = re:split(ExcludeStr, ",", [{return, list}]),
    [{file, FilePerm}, {dir, DirPerm}, {exclude, Excludes}].

% Walk the given directory tree, looking for entires to correct.
walk_tree(Path, Excludes, DirPerm, FilePerm) ->
    {ok, Filenames} = file:list_dir(Path),
    Included = lists:filter(fun(F) -> not lists:member(F, Excludes) end, Filenames),
    Filepaths = [filename:join(Path, F) || F <- Included],
    Files = lists:filter(fun filelib:is_regular/1, Filepaths),
    Dirs = lists:filter(fun filelib:is_dir/1, Filepaths),
    lists:foreach(fun (F) -> fix_perms(F, DirPerm, FilePerm) end, Files ++ Dirs),
    lists:foreach(fun (D) -> walk_tree(D, Excludes, DirPerm, FilePerm) end, Dirs).

% Change the permissions on the given file or directory.
fix_perms(F, DirPerm, FilePerm) ->
    case file:read_file_info(F) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            if Mode =/= DirPerm -> file:change_mode(F, DirPerm);
                true -> ok
            end;
        {ok, #file_info{type=regular, mode=Mode}} ->
            if Mode =/= FilePerm -> file:change_mode(F, FilePerm);
                true -> ok
            end
    end,
    ok.
