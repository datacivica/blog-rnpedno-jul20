##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Alicia Franco
# Maintainer(s): Alicia Franco, Mariana Solano
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# rnpdno/import_rnpdno/src/import.R

require(pacman)
p_load(tidyverse, janitor, data.table, here, purrr)

#Inputs y outputs generales
files_input = list(
  cenapi = here("import/input/cenapi.csv"),
  entidades = here("import/input/entidades_federativas.csv"))

files_output = list(
  lcv =  here("import/output/lcv.rds"),
  lsv =  here("import/output/lsv.rds"),
  d =  here("import/output/d.rds"),
  cenapi = here("import/output/cenapi.rds"),
  nom_ent = here("import/output/nom_ent.rds")
)

#CENAPI

cenapi <- fread(files_input$cenapi) %>% clean_names()
saveRDS(cenapi, files_output$cenapi)
rm(cenapi)


#Nombres entidades
nom <- read.csv(files_input$entidades) %>% clean_names()
saveRDS(nom, files_output$nom_ent)
rm(nom)

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
  input_lcv[[i]] <- here(paste0("import/input/lcv/lcv_",i,".csv"))
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
  input_lsv[[i]] <- here(paste0("import/input/lsv/lsv_",i,".csv"))
} 

#Abrir todas lsv y pegarlas
lsv_data <- data.frame()
for (i in cod_inegi) {
  data <- read.csv(input_lsv[[i]]) %>% clean_names() %>% 
    mutate(inegi = i, status = "localizado sin vida")
  lsv_data <- rbind(lsv_data, data)
} 
rm(input_lsv)
#Salen 3 warnings de lsv_04, lsv_08 y lsv_99 pero sí las está leyendo bien


# Files de desaparecidos y no localizados (d)
for (i in cod_inegi) {
  input_d[[i]] <- here(paste0("import/input/d/d_",i,".csv"))
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

saveRDS(lcv_data, files_output$lcv)
saveRDS(lsv_data, files_output$lsv)
saveRDS(d_data, files_output$d)
