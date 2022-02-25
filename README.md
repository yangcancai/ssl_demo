ssl_demon
----

![CI](https://github.com/yangcancai/ssl_demon/actions/workflows/ci.yml/badge.svg)

Required
-----
	$ rebar3 -v
	rebar 3.14.4 on Erlang/OTP 22 Erts 10.7.2.1

Shell
-----
### add www.example.com to host
    $ sudo vim /etc/hosts
      127.0.0.1 www.example.com
### rebar3 shell
    $ make shell
```erlang
#./tool.sh replace_config
./rebar3 as test shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling ssl_demon
Erlang/OTP 22 [erts-10.7.2.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.7.2.1  (abort with ^G)
(ssl_demon@127.0.0.1)1> ===> Booted ssl_demon

(ssl_demon@127.0.0.1)1> ssl_demon:start_usr_cert().
{sslsocket,{gen_tcp,#Port<0.20>,tls_connection,undefined},
[<0.273.0>,<0.271.0>]}
(ssl_demon@127.0.0.1)2> Accept: client=<<"usr1.example.com">>

(ssl_demon@127.0.0.1)2> ssl_demon:start_usr2_cert().
{sslsocket,{gen_tcp,#Port<0.22>,tls_connection,undefined},
[<0.279.0>,<0.277.0>]}
(ssl_demon@127.0.0.1)3> Accept: client=<<"usr2.example.com">>

(ssl_demon@127.0.0.1)3> ssl:send(v(1),<<"I'am user one">>).
ok
(ssl_demon@127.0.0.1)4> server recv: client=<<"usr1.example.com">>,req={ssl,
{sslsocket,
{gen_tcp,#Port<0.21>,
tls_connection,
[{option_tracker,<0.268.0>},
{session_tickets_tracker,
disabled}]},
[<0.272.0>,<0.270.0>]},
"I'am user one"}

(ssl_demon@127.0.0.1)4> ssl:send(v(2),<<"I'am user two">>).
ok
(ssl_demon@127.0.0.1)5> server recv: client=<<"usr2.example.com">>,req={ssl,
{sslsocket,
{gen_tcp,#Port<0.23>,
tls_connection,
[{option_tracker,<0.268.0>},
{session_tickets_tracker,
disabled}]},
[<0.280.0>,<0.278.0>]},
"I'am user two"}

(ssl_demon@127.0.0.1)5> ssl_demon:start_client().
{sslsocket,{gen_tcp,#Port<0.24>,tls_connection,undefined},
[<0.286.0>,<0.285.0>]}
(ssl_demon@127.0.0.1)6>
(ssl_demon@127.0.0.1)6> ssl:send(v(5),"I'am ...").
ok
(ssl_demon@127.0.0.1)7> server recv: client=<<"unknown">>,req={ssl,
{sslsocket,
{gen_tcp,#Port<0.25>,tls_connection,
[{option_tracker,<0.267.0>},
{session_tickets_tracker,
<0.269.0>}]},
[<0.288.0>,<0.287.0>]},
"I'am ..."}

```
Reference

[openssl-certificate-authority](https://jamielinux.com/docs/openssl-certificate-authority/index.html)