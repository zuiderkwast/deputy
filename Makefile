PROJECT = deputy
PROJECT_DESCRIPTION = Web proxy build on cowboy and gun
PROJECT_VERSION = 0.1.0

DEPS = ranch cowlib cowboy gun
dep_ranch = git https://github.com/ninenines/ranch master
dep_cowlib = git https://github.com/ninenines/cowlib master
dep_cowboy = git https://github.com/zuiderkwast/cowboy connect
dep_gun = git https://github.com/ninenines/gun master

include erlang.mk

# Certificates
# ------------

# 1. Create a CA private key and a cert.
# 2. Create a server private key and a CSR.
# 3. The CA signs the CSR to produce the server cert.

# Inspired by
# https://stackoverflow.com/questions/49553138/how-to-make-browser-trust-localhost-ssl-certificate

org = localhost-ca
ca = localhost-ca
domain = localhost

certdir = priv/certs
cakey = $(certdir)/$(ca).key
cacert = $(certdir)/$(ca).crt
key = $(certdir)/$(domain).key
csr = $(certdir)/$(domain).csr
cert = $(certdir)/$(domain).crt
extfile = $(certdir)/extfile-$(domain)

all:: $(cacert) $(cakey) $(cert) $(key)

clean::
	rm -rf $(certdir)

$(cakey): | $(certdir)
	openssl genpkey -algorithm RSA -out $@

$(cacert): $(cakey) | $(certdir)
	openssl req -x509 -key $(cakey) -out $(cacert) \
	    -subj "/CN=$(org)/O=$(org)"

$(key): | $(certdir)
	openssl genpkey -algorithm RSA -out $@

$(csr): $(key) | $(certdir)
	openssl req -new -key $(key) -out $@ \
	    -subj "/CN=$(domain)/O=$(org)"

$(cert): $(csr) $(cakey) $(cacert) $(extfile) | $(certdir)
	openssl x509 -req -in $(csr) -days 3650 -out $@ \
	    -CA $(cacert) -CAkey $(cakey) -CAcreateserial \
	    -extfile $(extfile)

$(extfile): | $(certdir)
	printf "%s\n" \
	 'basicConstraints = CA:FALSE' \
	 'subjectKeyIdentifier = hash' \
	 'authorityKeyIdentifier = keyid,issuer' \
	 'subjectAltName = DNS:$(domain)' \
	 > $@

$(certdir):
	mkdir -p $@
