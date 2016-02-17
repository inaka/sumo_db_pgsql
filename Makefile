PROJECT = sumo_db_pgsql

DEPS = lager sumo_db epgsql

dep_lager = git https://github.com/basho/lager.git 3.0.1
dep_sumo_db = git https://github.com/inaka/sumo_db.git 0.3.13
dep_epgsql = git https://github.com/epgsql/epgsql 2.0.0

include erlang.mk

ERLC_OPTS := +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

erldocs:
	erldocs . -o docs

changelog:
	github_changelog_generator --token ${TOKEN}

EDOC_OPTS += todo, report_missing_types
