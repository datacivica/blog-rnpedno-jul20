##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Alicia Franco
# Maintainer(s): Alicia Franco, Mariana Solano
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# /clean-data/src/clean_rnpedno.R

# Cargamos paquetes
require(pacman)
p_load(tidyverse, janitor, here)

# Archivos

files_input = list(
  lcv = here("import/output/lcv.rds"),
  lsv = here("import/output/lsv.rds"),
  d = here("import/output/d.rds"),
  ent_nom = here("import/output/nom_ent.rds")
  )

files_output = list(
  datos = here("clean-data/output/rnpedno.rds")
)

# Abrir y juntar todas lcv, lsv, d

lcv <- readRDS(files_input$lcv)
lsv <- readRDS(files_input$lsv)
d <- readRDS(files_input$d)

datos <- rbind(lcv,lsv,d) %>% rename(year = category) %>% 
  mutate(year = ifelse(year == "1.CIFRA SIN  AÑO DE REFERENCIA", 
                       "no determinado", year))
rm(lcv, lsv, d)

# Cruzar con nombres
nom <- readRDS(files_input$ent_nom) %>% rename(inegi = cve_ent)
datos <- left_join(datos, nom, by = c("inegi"))
rm (nom)


# Guardar base 

saveRDS(datos, files_output$datos)
