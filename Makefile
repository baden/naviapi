# See LICENSE for licensing information.

PROJECT = naviapi

# Options.
# -Werror
ERLC_OPTS ?= +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec \
	+'{parse_transform, lager_transform}' \
	-Dmaps_support
# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config
PLT_APPS = crypto public_key

# Dependencies.

# bear not got automaticaly by folsom, bug?
DEPS = lager cowboy oauth2 jsx jsxn navidb
# TEST_DEPS = ct_helper gun
TEST_DEPS = gun
# dep_ct_helper = git https://github.com/extend/ct_helper.git master

#dep_cowboy = git git://github.com/ninenines/cowboy.git 2.0.0-pre.1
dep_cowboy = git git://github.com/baden/cowboy.git master
dep_jsx = git git://github.com/baden/jsx.git develop
dep_jsxn = git git://github.com/talentdeficit/jsxn.git v2.1.1
# dep_folsom = git https://github.com/boundary/folsom.git master
dep_oauth2 = git https://github.com/kivra/oauth2.git 0.4.1
dep_navidb = git git://github.com/baden/navidb.git master

include erlang.mk

# Also dialyze the tests.
# DIALYZER_OPTS += --src -r test
test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s naviapi -config test/test.config
