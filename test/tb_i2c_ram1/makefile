# makefile for tb_i2c_ram1

REPO_ROOT:=$(shell git rev-parse --show-toplevel)
ifeq ($(OS),Windows_NT)
REPO_ROOT:=$(shell cygpath -m $(REPO_ROOT))
endif
MAKE_FPGA:=$(REPO_ROOT)/submodules/make-fpga/make-fpga.mak
SRC:=$(REPO_ROOT)/src

SIMULATOR:=ghdl nvc vsim xsim

SIM_TOP:=tb_i2c_ram1
SIM_SRC:=\
    $(SRC)/common/tyto_types_pkg.vhd \
    $(SRC)/common/i2c/i2c_ram1.vhd \
    $(SRC)/common/i2c/test/tb_i2c_ram1.vhd
SIM_RUN=$(SIM_TOP)

VSCODE_SRC:=$(SIM_SRC)
V4P_TOP:=$(SIM_TOP)
V4P_LIB_SRC:=

include $(MAKE_FPGA)
