# See LICENSE for licensing information.
.SILENT:

PROJECT = naviapi

# Options.
# -Werror
# ERLC_OPTS ?= +debug_info +warn_export_all +warn_export_vars \
# 	+warn_shadow_vars +warn_obsolete_guard +warn_missing_spec \
# 	+'{parse_transform, lager_transform}' \
# 	-Dmaps_support
# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars
ERLC_OPTS += +warn_unused_import +warn_unused_function +warn_bif_clash
ERLC_OPTS += +warn_unused_record +warn_deprecated_function +warn_obsolete_guard
ERLC_OPTS += +strict_validation +warn_export_vars +warn_exported_vars
ERLC_OPTS += +warn_missing_spec +warn_untyped_record +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config
PLT_APPS = crypto public_key ssl lager

# Dependencies.

# bear not got automaticaly by folsom, bug?
DEPS = lager cowboy oauth2 jsx jsxn navidb
# TEST_DEPS = ct_helper gun
TEST_DEPS = gun xref_runner
# dep_ct_helper = git https://github.com/extend/ct_helper.git master

#dep_cowboy = git git://github.com/ninenines/cowboy.git 2.0.0-pre.1
dep_cowboy = git git://github.com/baden/cowboy.git master
dep_jsx = git git://github.com/baden/jsx.git develop
dep_jsxn = git git://github.com/talentdeficit/jsxn.git v2.1.1
# dep_folsom = git https://github.com/boundary/folsom.git master
dep_oauth2 = git https://github.com/kivra/oauth2.git 0.4.1
dep_navidb = git git://github.com/baden/navidb.git refresh2017

BUILD_DEPS = elvis_mk
DEP_PLUGINS = elvis_mk

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

EDOC_DIRS := ["src"]
EDOC_OPTS := {preprocess, true}, {source_path, ${EDOC_DIRS}}, nopackages, {subpackages, true}

include erlang.mk

# Also dialyze the tests.
# DIALYZER_OPTS += --src -r test
test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s naviapi -config test/test.config
