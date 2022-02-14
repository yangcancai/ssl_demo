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
(ssl_demon@127.0.0.1)1> ssl_demon:start_client().
{sslsocket,{gen_tcp,#Port<0.19>,tls_connection,undefined},
           [<0.254.0>,<0.252.0>]}
(ssl_demon@127.0.0.1)2> ssl_demon:start_usr_cert().
{sslsocket,{gen_tcp,#Port<0.21>,tls_connection,undefined},
           [<0.259.0>,<0.257.0>]}
```
Reference

[openssl-certificate-authority](https://jamielinux.com/docs/openssl-certificate-authority/index.html)