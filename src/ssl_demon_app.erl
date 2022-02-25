%%%-------------------------------------------------------------------
%%% @author yangcancai

%%% Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
   
%%% @doc
%%%
%%% @end
%%% Created : 2022-02-11T13:26:34+00:00
%%%-------------------------------------------------------------------

-module(ssl_demon_app).

-author("yangcancai").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang:spawn(fun() -> ssl_demon:start_usr_server(), ssl_demon:recv() end),
    erlang:spawn(fun() -> ssl_demon:start_server(), ssl_demon:recv() end),
    ssl_demon_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
