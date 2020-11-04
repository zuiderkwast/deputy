-module(deputy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile(
			   [
			    {"localhost", [{'_', cowboy_static, []}]},
			    {'_', [{'_', proxy_handler, #{}}]}]),
	{ok, _} = cowboy:start_clear(deputy_http_listener,
				     [{port, 8080}],
				     #{env => #{dispatch => Dispatch}}),
	Dir = code:priv_dir(deputy),
	{ok, _} = cowboy:start_tls(deputy_https_listener,
				   [{port, 8443},
				    {certfile, filename:join(Dir, "localhost.crt")},
				    {keyfile, filename:join(Dir, "localhost.key")}
				   ],
				   #{env => #{dispatch => Dispatch}}),
	deputy_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(deputy_http_listener),
	ok = cowboy:stop_listener(deputy_https_listener).
