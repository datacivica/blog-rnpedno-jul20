#
# Author: OE
# Maintainer(s): OE, MS, AF
# License: (c) Data Cívica 2020
#
# ----------------------------------------
# blog-rnpedno-jul20/descriptives/Makefile
#

HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')

.PHONY: all clean

all: output/heatmap-diff.png \
	 output/heatmap-diff.svg \
	 output/fiebre-estatus.png \
	 output/fiebre-estatus.svg \
	 output/fiebre-ent.png \
	 output/fiebre-ent.svg

clean:
	rm output/*

OUTPUTS := output/heatmap-diff.png \
			output/heatmap-diff.svg \
			output/fiebre-estatus.png \
			output/fiebre-estatus.svg \
			output/fiebre-ent.png \
			output/fiebre-ent.svg

INPUTS := src/descriptives.R \
		$(HERE)/clean-data/output/cenapi.rds \
		$(HERE)/clean-data/output/rnpdno.rds

$(OUTPUTS): $(INPUTS) 
	Rscript --vanilla $<
	rm Rplots.pdf

# done.