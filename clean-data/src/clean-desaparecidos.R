##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Alicia Franco
# Maintainer(s): Alicia Franco, Mariana Solano
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# /clean-data/src/clean.R

# Cargamos paquetes
require(pacman)
p_load(tidyverse, janitor, data.table, here)

# Listas de directorios
files_input = list(
  cenapi = here("clean-data/output/clean-cenapi.rds"),
  rnpedno = here("clean-data/output/rnpedno.rds")
)

files_output = list (
  clean_desaparecidos = here("clean-data/output/desaparecidos.rds")
)

# Abrimos y homogeneizamos CENAPI con RNPEDNO
cenapi <- readRDS(files_input$cenapi) %>% 
  rename(status = vivo_muerto, year = year_evento, inegi = clave_estado, #estados comparables
         nom_ent = nom_ent_evento) %>% 
  mutate(n = 1, 
         status = case_when(status == 'Aun sin localizar' ~ 'desaparecidos',
                            status == "Localizado con vida" ~ 'localizado con vida',
                            T ~ "localizado sin vida"),
         fuente = "CENAPI") %>% 
  group_by(year, sexo, inegi, status, nom_ent, fuente) %>% 
  summarise(cuenta = sum (n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = sexo, values_from = cuenta) %>% 
  select(year, Hombre, Mujer, inegi, status, nom_ent, fuente) %>% 
  mutate(indeterminado = 0) %>% # No registran "indeterminados" pero queremos rbindear
  clean_names()

# Abrimos y delimitamos años del RNPEDNO
rnpedno <- readRDS(files_input$rnpedno) %>% 
  mutate(fuente = "RNPEDNO") %>% 
  filter(year >= 2000 & year <= 2018) #CENAPI tiene desde el 2000 y hasta el 2018, queremos comparar

# Juntamos todo
base <- rbind(rnpedno, cenapi) %>% 
  pivot_longer(hombre:indeterminado, names_to = "sexo", values_to = "cuenta")

#Guardamos base final
saveRDS(base, files_output$clean_desaparecidos)
