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
  rnpedno = here("clean-data/output/rnpedno.rds"),
  pob = here("clean-data/input/pob-mit-poryecciones.rds"),
  abrev = here("import/output/nom_ent.rds")
)

files_output = list (
  clean_desaparecidos = here("clean-data/output/desaparecidos.rds")
)

# Abrimos y homogeneizamos CENAPI con RNPEDNO 
abrev <- readRDS(files_input$abrev) %>% 
  rename(inegi = cve_ent)

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
  mutate(cuenta = round(ifelse(year == 2018, cuenta*3, cuenta))) %>%  #Anualizamos 2018
  ungroup() %>% 
  pivot_wider(names_from = sexo, values_from = cuenta) %>% 
  select(year, Hombre, Mujer, inegi, status, nom_ent, fuente) %>% 
  clean_names() %>% 
  left_join(., abrev)

# Abrimos y delimitamos años del RNPEDNO
rnpedno <- readRDS(files_input$rnpedno) %>% 
  mutate(fuente = "RNPEDNO") %>% 
  filter(year >= 2000 & year <= 2018) %>% #CENAPI tiene desde el 2000 y hasta el 2018, queremos comparar
  select(-c(indeterminado))

# Abrimos y limpiamos pob

pob <- readRDS(files_input$pob) %>% 
  rename(nom_ent = entidad, inegi = cve_ent) %>% 
  group_by(year, nom_ent, inegi, sexo) %>% 
  summarise(pob = sum(poblacion)) %>% 
  ungroup() %>% 
  filter(year >= 2000) %>% 
  mutate(year = as.character(year),
         sexo = case_when(sexo == "Hombres" ~ "hombre",
                          sexo == "Mujeres" ~ "mujer"))
  
  
# Juntamos todo
base <- rbind(rnpedno, cenapi) %>% 
  mutate(nom_ent = case_when(nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz", 
                             nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
                             nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
                             T ~ nom_ent)) %>% 
  pivot_longer(hombre:mujer, names_to = "sexo", values_to = "cuenta")  %>% 
  left_join(., pob) 

#Guardamos base final
saveRDS(base, files_output$clean_desaparecidos)
