{application, 'deputy', [
	{description, "Web proxy build on cowboy and gun"},
	{vsn, "0.1.0"},
	{modules, ['deputy_app','deputy_sup','proxy_handler']},
	{registered, [deputy_sup]},
	{applications, [kernel,stdlib,ranch,cowlib,cowboy,gun]},
	{mod, {deputy_app, []}},
	{env, []}
]}.