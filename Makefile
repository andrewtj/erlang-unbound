PROJECT = unbound
PROJECT_DESCRIPTION = Unbound wrapper
PROJECT_VERSION = 0.1.0
PROJECT_REGISTERED = unbound
LOCAL_DEPS = crypto
LDLIBS = -lunbound
C_SRC_OUTPUT = $(CURDIR)/priv/unbound_drv
SHELL_OPTS = -eval 'unbound:start()'
include erlang.mk
