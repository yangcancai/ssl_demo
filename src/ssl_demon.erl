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
%%% ssl 自签名证书 测试
%%% 自签名证书流程： https://jamielinux.com/docs/openssl-certificate-authority/index.html
%%% @end
%%% Created : 2022-02-11T13:26:34+00:00
%%%-------------------------------------------------------------------

-module(ssl_demon).

-author("yangcancai").

-export([start_server/0, start_client/0,
  start_usr_server/0,start_usr_cert/0,
  recv/0]).
%% 服务器私钥和签名证书
-define(SERVER_CERT, "priv/www.example.com.cert.pem").
-define(SERVER_KEY, "priv/www.example.com.key.pem").
%% 客户端认证私钥和签名证书
-define(USR_CERT, "priv/www.example.com.usr.cert.pem").
-define(USR_KEY, "priv/www.example.com.usr.key.pem").
%% 证书链
-define(CA_CHAIN_CERT, "priv/ca-chain.cert.pem").
%% 根证书
-define(CA_CERT, "priv/ca.cert.pem").
start_server() ->
    Port = 11029,
        LOpts = [
             {cacertfile, ?CA_CHAIN_CERT},
             {certfile, ?SERVER_CERT},
             {keyfile, ?SERVER_KEY},
             {password, "123456"},
             {reuseaddr, true},
             {versions, ['tlsv1.2','tlsv1.3']},
             {session_tickets, stateless}
            ],
        {ok, LSock} = ssl:listen(Port, LOpts),
        %% Accept first connection
        {ok, CSock0} = ssl:transport_accept(LSock),
        {ok, _} = ssl:handshake(CSock0),
        ssl:stop().
        %% Accept second connection
%%        {ok, CSock1} = ssl:transport_accept(LSock),
%%        {ok, Sock} = ssl:handshake(CSock1),
%%        Sock.
start_client() ->
  application:load(ssl),
	{ok, _} = application:ensure_all_started(ssl),
  erlang:spawn(fun() -> ssl_demon:start_server(), ssl_demon:recv() end),
	Port = 11029,
	COpts1 = [
    {cacertfile, ?CA_CERT},
%%  {log_level, debug},
    {verify, verify_peer},
	    {versions, ['tlsv1.2', 'tlsv1.3']},
		  {session_tickets, auto}
  ],
        {ok, Sock} = ssl:connect("www.example.com", Port, COpts1),
	Sock.
%% 双向认证
start_usr_server() ->
  Port = 11030,
  LOpts = [
    {cacertfile, ?CA_CHAIN_CERT},
    {certfile, ?SERVER_CERT},
    {keyfile, ?SERVER_KEY},
    {password, "123456"},
    %% 认证客户端
    {verify, verify_peer},
    %% 要求客户端发送证书
    {fail_if_no_peer_cert, true},
    {reuseaddr, true},
    {versions, ['tlsv1.2','tlsv1.3']}
%%    {session_tickets, stateless}
  ],
  {ok, LSock} = ssl:listen(Port, LOpts),
  %% Accept first connection
  {ok, CSock0} = ssl:transport_accept(LSock),
  {ok, _} = ssl:handshake(CSock0),
  ssl:stop().
start_usr_cert() ->
  application:load(ssl),
  {ok, _} = application:ensure_all_started(ssl),
  erlang:spawn(fun() -> ssl_demon:start_usr_server(), ssl_demon:recv() end),
  Port = 11030,
  COpts1 = [
  %% 信任的根证书
  {cacertfile, ?CA_CERT},
  %% 客户端证书
  {certfile, ?USR_CERT},
  %% 客户端密钥
  {keyfile, ?USR_KEY},
  %% 开启证书认证
  {verify, verify_peer},
  {password, "123456"},
  {versions, ['tlsv1.2', 'tlsv1.3']}
    ],
%%  {session_tickets, auto}],
  {ok, Sock} = ssl:connect("www.example.com", Port, COpts1),
  Sock.

recv() ->
  receive
   Info ->
     io:format("server recv: ~p~n",[Info]),
     recv()
  end.