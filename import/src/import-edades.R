##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Mariana S
# Maintainer(s): Alicia Franco, Mariana Solano
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# rnpdno/import_rnpdno/src/import-edades.R

require(pacman)
p_load(tidyverse, janitor, data.table, here, readxl)

#Inputs y outputs generales
files_output = list(edades_lcv = here("import/output/edades_lcv.rds"),
                    edades_lsv = here("import/output/edades_lsv.rds"),
                    edades_d = here("import/output/edades_d.rds"))


#RNPEDNO
#Códigos del inegi para abrir archivos
cod_inegi <- c(1:32) %>% formatC(width = 2, flag = "0", format = "d")
cod_inegi <- append(cod_inegi,"99")

#Vectores vacios para generar listas de archivos
input_lcv <- vector(mode = "list")
input_lsv <- vector(mode = "list")
input_d <- vector(mode = "list")


# Files de localizados con vida (lcv)
for (i in cod_inegi) {
  input_lcv[[i]] <- here(paste0("import/input/edades/lcv/lcv_",i,".csv"))
} 

#Abrir todas lcv y pegarlas
lcv_data <- data.frame()
for (i in cod_inegi) {
  data <- read.csv(input_lcv[[i]]) %>% clean_names() %>% 
    mutate(inegi = i, status = "localizado con vida")
  lcv_data <- rbind(lcv_data, data) 
} 
rm(input_lcv)


# Files de localizados sin vida (lsv)
for (i in cod_inegi) {
  input_lsv[[i]] <- here(paste0("import/input/edades/lsv/lsv_",i,".csv"))
} 

#Abrir todas lsv y pegarlas
lsv_data <- data.frame()
for (i in cod_inegi) {
  data <- read.csv(input_lsv[[i]]) %>% clean_names() %>% 
    mutate(inegi = i, status = "localizado sin vida")
  lsv_data <- rbind(lsv_data, data)
} 
rm(input_lsv)
#Sale un warning de lsv_18 Nayarit pero sí la está leyendo bien


# Files de desaparecidos y no localizados (d)
for (i in cod_inegi) {
  input_d[[i]] <- here(paste0("import/input/edades/d/d_",i,".csv"))
} 

#Abrir todas las d_ y pegarlas
d_data <- data.frame()
for (i in cod_inegi) {
  data <- read.csv(input_d[[i]]) %>% clean_names() %>% 
    mutate(inegi = i, status = "desaparecidos")
  d_data <- rbind(d_data, data)
} 
rm(input_d)


#Guardar todo como rds

saveRDS(lcv_data, files_output$edades_lcv)
saveRDS(lsv_data, files_output$edades_lsv)
saveRDS(d_data, files_output$edades_d)
