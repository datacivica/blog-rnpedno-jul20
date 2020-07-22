#
# Author: OE
# Maintainer(s): OE, MS, AF
# License: (c) Data Cívica 2020
#
# ------------------------------------
#
# blog-rnpedno-jul20/import/Makefile
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

all: output/cenapi.rds \
	 output/nombre-entidad.rds \
	 output/rnpdno.rds

clean:
	rm output/*

INPUTS := $(shell find $(HERE)/import/input -name '*.csv')

OUTPUTS := output/cenapi.rds \
	 output/nombre-entidad.rds \
	 output/rnpdno.rds

$(OUTPUTS): src/import.R $(INPUTS)
	Rscript --vanilla $<

# done.