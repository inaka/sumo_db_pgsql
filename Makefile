PROJECT = sumo_db_pgsql

CONFIG ?= test/test.config

DEPS = lager sumo_db epgsql

dep_lager = git https://github.com/basho/lager.git 3.0.1
dep_sumo_db = git https://github.com/inaka/sumo_db.git e99d05e
dep_epgsql = git https://github.com/epgsql/epgsql 2.0.0

TEST_DEPS = katana_test mixer

dep_katana_test = git https://github.com/inaka/katana-test.git 5491fba
dep_mixer = git git://github.com/inaka/mixer.git 0.1.5

BUILD_DEPS = inaka_mk

dep_inaka_mk = git https://github.com/inaka/inaka.mk.git 1.0.0

DEP_PLUGINS = inaka_mk

LOCAL_DEPS := tools compiler syntax_tools common_test inets test_server dialyzer wx

include erlang.mk

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Common Test config
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' +debug_info
CT_OPTS = -cover test/sumo_db_pgsql.coverspec -vvv -erl_args -boot start_sasl -config ${CONFIG}

erldocs:
	erldocs . -o docs

changelog:
	github_changelog_generator --token ${TOKEN}

EDOC_OPTS += todo, report_missing_types
