PROJECT = deputy
PROJECT_DESCRIPTION = Web proxy build on cowboy and gun
PROJECT_VERSION = 0.1.0

DEPS = ranch cowlib cowboy gun
dep_ranch = git https://github.com/ninenines/ranch master
dep_cowlib = git https://github.com/ninenines/cowlib master
dep_cowboy = git https://github.com/zuiderkwast/cowboy connect
dep_gun = git https://github.com/ninenines/gun master

include erlang.mk

# Create self-signed certificate for localhost.
# (This needs to be added to your browser to use TLS.)
keyfile = priv/localhost.key
certfile = priv/localhost.crt

all:: $(keyfile) $(certfile)

$(keyfile) $(certfile): | priv
	openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 -nodes \
	  -keyout $(keyfile) -out $(certfile) -subj "/CN=localhost" \
	  -addext "subjectAltName=DNS:localhost,DNS:example.com,IP:127.0.0.1"

priv:
	mkdir priv
