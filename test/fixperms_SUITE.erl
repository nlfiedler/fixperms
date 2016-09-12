%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Nathan Fiedler
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
%% The test suite.
%%
-module(fixperms_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [
        path_validation_test,
        perms_validation_test,
        fixperms_test,
        fixperms_nondefault_perms_test
    ].


% Test the path arguments validation logic.
path_validation_test(_Config) ->
    BinDir = os:getenv("PWD") ++ "/_build/test/bin/",
    MissingPaths = lists:flatten(io_lib:format("~s/fixperms", [BinDir])),
    ?assertCmdOutput("Missing required path(s), see help.\n", MissingPaths),
    WrongPath = lists:flatten(io_lib:format("~s/fixperms foobar", [BinDir])),
    ?assertCmdOutput("Path does not exist: foobar\n", WrongPath),
    ok.

% Test the permissions validation logic.
perms_validation_test(_Config) ->
    BinDir = os:getenv("PWD") ++ "/_build/test/bin/",
    BadFilePerms1 = lists:flatten(io_lib:format("~s/fixperms --file a foobar", [BinDir])),
    ?assertCmdOutput("Invalid file permissions: a\n", BadFilePerms1),
    BadFilePerms2 = lists:flatten(io_lib:format("~s/fixperms --file 111 foobar", [BinDir])),
    ?assertCmdOutput("Invalid file permissions: 111\n", BadFilePerms2),
    BadFilePerms3 = lists:flatten(io_lib:format("~s/fixperms --file 888 foobar", [BinDir])),
    ?assertCmdOutput("Invalid file permissions: 888\n", BadFilePerms3),
    BadDirPerms1 = lists:flatten(io_lib:format("~s/fixperms --dir a foobar", [BinDir])),
    ?assertCmdOutput("Invalid directory permissions: a\n", BadDirPerms1),
    BadDirPerms2 = lists:flatten(io_lib:format("~s/fixperms --dir 111 foobar", [BinDir])),
    ?assertCmdOutput("Invalid directory permissions: 111\n", BadDirPerms2),
    BadDirPerms3 = lists:flatten(io_lib:format("~s/fixperms --dir 888 foobar", [BinDir])),
    ?assertCmdOutput("Invalid directory permissions: 888\n", BadDirPerms3),
    ok.

% Test the permissions fixing logic.
fixperms_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    % make several directories with some files in each
    mkdir(PrivDir, "foo", ["a1", "a2", "a3"]),
    mkdir(PrivDir, "bar", ["b1", "b2", "b3"]),
    mkdir(PrivDir, "notouch", ["n1", "n2", "n3"]),
    % set their permissions to be in need of change
    file:change_mode(filename:join(PrivDir, "foo"), 8#700),
    file:change_mode(filename:join(PrivDir, "bar"), 8#750),
    file:change_mode(filename:join(PrivDir, "notouch"), 8#700),
    file:change_mode(filename:join([PrivDir, "foo", "a1"]), 8#600),
    file:change_mode(filename:join([PrivDir, "foo", "a2"]), 8#640),
    file:change_mode(filename:join([PrivDir, "foo", "a3"]), 8#750),
    file:change_mode(filename:join([PrivDir, "bar", "b1"]), 8#600),
    file:change_mode(filename:join([PrivDir, "bar", "b2"]), 8#640),
    file:change_mode(filename:join([PrivDir, "bar", "b3"]), 8#750),
    file:change_mode(filename:join([PrivDir, "notouch", "n1"]), 8#600),
    file:change_mode(filename:join([PrivDir, "notouch", "n2"]), 8#640),
    file:change_mode(filename:join([PrivDir, "notouch", "n3"]), 8#750),
    % fix the permissions
    BinDir = os:getenv("PWD") ++ "/_build/test/bin/",
    FixCmd = lists:flatten(io_lib:format("~s/fixperms --exclude notouch ~s", [BinDir, PrivDir])),
    ?assertCmd(FixCmd),
    % verify the permissions have been fixed
    {ok, FooInfo} = file:read_file_info(filename:join(PrivDir, "foo")),
    ?assertEqual(directory, FooInfo#file_info.type),
    ?assertEqual(8#40755, FooInfo#file_info.mode),
    {ok, A1Info} = file:read_file_info(filename:join([PrivDir, "foo", "a1"])),
    ?assertEqual(regular, A1Info#file_info.type),
    ?assertEqual(8#100644, A1Info#file_info.mode),
    {ok, A2Info} = file:read_file_info(filename:join([PrivDir, "foo", "a2"])),
    ?assertEqual(regular, A2Info#file_info.type),
    ?assertEqual(8#100644, A2Info#file_info.mode),
    {ok, A3Info} = file:read_file_info(filename:join([PrivDir, "foo", "a3"])),
    ?assertEqual(regular, A3Info#file_info.type),
    ?assertEqual(8#100644, A3Info#file_info.mode),
    {ok, BarInfo} = file:read_file_info(filename:join(PrivDir, "bar")),
    ?assertEqual(directory, BarInfo#file_info.type),
    ?assertEqual(8#40755, BarInfo#file_info.mode),
    {ok, B1Info} = file:read_file_info(filename:join([PrivDir, "bar", "b1"])),
    ?assertEqual(regular, B1Info#file_info.type),
    ?assertEqual(8#100644, B1Info#file_info.mode),
    {ok, B2Info} = file:read_file_info(filename:join([PrivDir, "bar", "b2"])),
    ?assertEqual(regular, B2Info#file_info.type),
    ?assertEqual(8#100644, B2Info#file_info.mode),
    {ok, B3Info} = file:read_file_info(filename:join([PrivDir, "bar", "b3"])),
    ?assertEqual(regular, B3Info#file_info.type),
    ?assertEqual(8#100644, B3Info#file_info.mode),
    % this directory and its contents should be unchanged
    {ok, NotouchInfo} = file:read_file_info(filename:join(PrivDir, "notouch")),
    ?assertEqual(directory, NotouchInfo#file_info.type),
    ?assertEqual(8#40700, NotouchInfo#file_info.mode),
    {ok, N1Info} = file:read_file_info(filename:join([PrivDir, "notouch", "n1"])),
    ?assertEqual(regular, N1Info#file_info.type),
    ?assertEqual(8#100600, N1Info#file_info.mode),
    {ok, N2Info} = file:read_file_info(filename:join([PrivDir, "notouch", "n2"])),
    ?assertEqual(regular, N2Info#file_info.type),
    ?assertEqual(8#100640, N2Info#file_info.mode),
    {ok, N3Info} = file:read_file_info(filename:join([PrivDir, "notouch", "n3"])),
    ?assertEqual(regular, N3Info#file_info.type),
    ?assertEqual(8#100750, N3Info#file_info.mode),
    ok.

% Test the permissions fixing logic using non-default permissions.
fixperms_nondefault_perms_test(Config) ->
    PrivDir = ?config(priv_dir, Config),
    % make several directories with some files in each
    mkdir(PrivDir, "fubar", ["a1", "a2", "a3"]),
    % set their permissions to be in need of change
    file:change_mode(filename:join(PrivDir, "fubar"), 8#700),
    file:change_mode(filename:join([PrivDir, "fubar", "a1"]), 8#600),
    file:change_mode(filename:join([PrivDir, "fubar", "a2"]), 8#640),
    file:change_mode(filename:join([PrivDir, "fubar", "a3"]), 8#750),
    % fix the permissions
    BinDir = os:getenv("PWD") ++ "/_build/test/bin/",
    FixCmd = lists:flatten(io_lib:format("~s/fixperms -f 664 -d 775 ~s", [BinDir, PrivDir])),
    ?assertCmd(FixCmd),
    % verify the permissions have been fixed
    {ok, FooInfo} = file:read_file_info(filename:join(PrivDir, "fubar")),
    ?assertEqual(directory, FooInfo#file_info.type),
    ?assertEqual(8#40775, FooInfo#file_info.mode),
    {ok, A1Info} = file:read_file_info(filename:join([PrivDir, "fubar", "a1"])),
    ?assertEqual(regular, A1Info#file_info.type),
    ?assertEqual(8#100664, A1Info#file_info.mode),
    {ok, A2Info} = file:read_file_info(filename:join([PrivDir, "fubar", "a2"])),
    ?assertEqual(regular, A2Info#file_info.type),
    ?assertEqual(8#100664, A2Info#file_info.mode),
    {ok, A3Info} = file:read_file_info(filename:join([PrivDir, "fubar", "a3"])),
    ?assertEqual(regular, A3Info#file_info.type),
    ?assertEqual(8#100664, A3Info#file_info.mode),
    ok.

% Make a directory and a set of files, all within the base directory.
mkdir(Base, Dir, Files) ->
    Dirpath = filename:join(Base, Dir),
    ok = file:make_dir(Dirpath),
    MakeFile = fun(Name) ->
        Filename = filename:join(Dirpath, Name),
        file:write_file(Filename, <<"The Mellow Scarves?">>)
    end,
    lists:foreach(MakeFile, Files).
