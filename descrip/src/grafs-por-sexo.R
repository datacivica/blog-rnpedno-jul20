##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Mariana S
# Maintainer(s): AF, MS
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-rnpedno-jul20/descript/src/grafs-por-sexo

pacman::p_load(tidyverse, janitor,stringi, here, writexl, extrafont, scales,
               viridis, gginnards)

tema <- theme_minimal() +
        theme(plot.title = element_text(size = 20, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(size = 18, family = "Barlow Condensed", hjust = 0.5),
              plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
              axis.text = element_text(size = 12, family = "Barlow Condensed"),
              axis.title = element_text(size = 14, family = "Barlow Condensed"),
              legend.title = element_text(size = 14, family = "Barlow Condensed", hjust = 0.5),
              legend.text = element_text(size = 14, family = "Barlow Condensed", hjust = 0.5),
              strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"))

files <- list(cenapi = here("/clean-data/output/clean-cenapi.rds"),
              rnpdno = here("/clean-data/output/rnpedno.rds"),
              g_1 = here("/clean-data/output/g_1.png"),
              g_2 = here("/clean-data/output/g_2.png"),
              g_3 = here("/clean-data/output/g_3.png"),
              g_4 = here("/clean-data/output/g_4.png"))

cenapi <- readRDS(files$cenapi)
rnpedno <- readRDS(files$rnpedno)



