PROJECT = unbound
PROJECT_DESCRIPTION = Unbound wrapper
PROJECT_VERSION = 0.1.0
LDLIBS = -lunbound
C_SRC_OUTPUT = $(CURDIR)/priv/unbound_drv
SHELL_OPTS = -eval 'unbound:do()'
include erlang.mk
