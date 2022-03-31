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
%%% Created : 2022-03-31T09:22:39+00:00
%%%-------------------------------------------------------------------
-module(ssl_demon_upgrade).

-author("yangcancai").

-export([run/0, start_server/0, start_client/0]).
-define(SERVER_CERT, "priv/www.example.com.cert.pem").
-define(SERVER_KEY, "priv/www.example.com.key.pem").
run() ->
    erlang:spawn_link(fun() -> start_server() end),
    erlang:spawn_link(fun() -> start_client() end),
    ok.
start_client() ->
    {ok, Socket} = gen_tcp:connect("localhost", 9999,  [], infinity),
     ok = gen_tcp:send(Socket, <<"tls">>),
     put(id, client),
     loop(Socket).
start_server() ->
    {ok, ListenSocket} = gen_tcp:listen(9999, [{reuseaddr, true}]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    put(id, server),
    loop(Socket).
loop(Socket) ->
     receive
         {tcp_closed, _} ->
             log(close, tcp_closed),
             ok;
         {tcp_error, _} ->
             log(close, tcp_error),
             ok;
         {ssl_error, _} ->
             log(close, ssl_error),
             ok;
         {ssl_closed, _} ->
             log(close, ssl_closed),
             ok;
         {ssl, _, "hello tls resp"} ->
             log(ssl, <<"hello tls resp">>),
             ok;
         {ssl, _, "hello tls"} ->
             log(ssl, <<"hello tls">>),
             ok = ssl:send(Socket, <<"hello tls resp">>),
             loop(Socket);
         {tcp, _, "tls_processed"} ->
             log(tcp, <<"tls_processed">>),
             {ok, TlsSocket} = ssl:connect(Socket, []),
             ok = ssl:send(TlsSocket, <<"hello tls">>),
             loop(TlsSocket);
         {tcp, _, "tls"} ->
             log(tcp, "tls"),
             ok = gen_tcp:send(Socket, <<"tls_processed">>),
             case ssl:handshake(Socket, [
                 {certfile, ?SERVER_CERT}, {keyfile, ?SERVER_KEY},
                  {password, "123456"}
                  ]) of
                  {ok, TLSSocket} ->
                      log(tcp, <<"handshake">>),
                      loop(TLSSocket);
                  E->
                      log(tcp, E)
              end
     end.
log(TcpOrTls, Data) ->
    io:format("~p => ~p~n",[get(id),{TcpOrTls, Data}]).