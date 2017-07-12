PROJECT = unbound
PROJECT_DESCRIPTION = Unbound wrapper
PROJECT_VERSION = 0.1.0
PROJECT_REGISTERED = unbound
PROJECT_ENV = [{server_defaults, [{trust_anchor, auto}]}]
LOCAL_DEPS = crypto
LDLIBS = -lunbound
C_SRC_OUTPUT = $(CURDIR)/priv/unbound_drv
SHELL_OPTS = -eval 'unbound:start()'

include erlang.mk

$(PROJECT).d:: include/params.hrl

ifdef NO_TA_AUTR
ERLC_OPTS += -DNO_TA_AUTR
CFLAGS += -DNO_TA_AUTR
TEST_ERLC_OPTS += -DNO_TA_AUTR
endif

clean::
	$(gen_verbose) rm -f include/params.hrl

include/params.hrl: support/dns-parameters-4.csv ./support/gen_params.escript
	$(gen_verbose) ./support/gen_params.escript $@
