-module(deputy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Routes =
		[
		 %% If we proxy localhost, we create a proxy loop.
		 {"localhost",
		  [{"/", cowboy_static,
		    {priv_file, deputy, "static/example.html"}},
		   {"/[...]", cowboy_static,
		    {priv_dir, deputy, "static"}}
		  ]},
		 {'_',
		  [{'_', proxy_handler, #{}}]}
		],
	Dispatch = cowboy_router:compile(Routes),
	{ok, _} = cowboy:start_clear(deputy_http_listener,
				     [{port, 8080}],
				     #{env => #{dispatch => Dispatch}}),
	Dir = code:priv_dir(deputy),
	Certfile = filename:join([Dir, "certs", "localhost.crt"]),
	Keyfile = filename:join([Dir, "certs", "localhost.key"]),
	{ok, _} = cowboy:start_tls(deputy_https_listener,
				   [{port, 8443},
				    {certfile, Certfile},
				    {keyfile, Keyfile}],
				   #{env => #{dispatch => Dispatch}}),
	deputy_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(deputy_http_listener),
	ok = cowboy:stop_listener(deputy_https_listener).
