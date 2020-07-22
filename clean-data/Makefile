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
	 output/rnpdno.rds

clean:
	rm output/*

output/cenapi.rds output/rnpdno.rds: \
		src/clean.R \
		$(HERE)/import/output/cenapi.rds \
		$(HERE)/import/output/nombre-entidad.rds \
		$(HERE)/import/output/rnpdno.rds 
	Rscript --vanilla $<

# done.