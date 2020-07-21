##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Mariana Solano
# Maintainer(s): Alicia Franco, Mariana Solano
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# /clean-data/src/clean-edades.R

rm(list=ls())

require(pacman)
p_load(tidyverse, janitor, data.table, here)

files_input = list(d = here("import/output/edades_d.rds"),
                   lcv = here("import/output/edades_lcv.rds"),
                   lsv = here("import/output/edades_lsv.rds"),
                   pob = here("clean-data/input/pob-mit-poryecciones.rds"),
                   cenapi = here("clean-data/output/clean-cenapi.rds"),
                   abrev = here("import/output/nom_ent.rds"),
                   ent_nom = here("import/output/nom_ent.rds"))

files_output = list(datos = here("clean-data/output/rnpdno-edades.rds"),
                    base_junta = here("clean-data/output/cenapi-rnpdno-edades.rds"))

# Abrir y juntar todas lcv, lsv, d
lcv <- readRDS(files_input$lcv)
lsv <- readRDS(files_input$lsv)
d <- readRDS(files_input$d)

datos <- rbind(lcv,lsv,d) %>% 
         rename(gpo_edad = category) %>% 
         mutate(gpo_edad = case_when(gpo_edad == "0-4" ~ "De 0 a 4 años", 
                                     gpo_edad == "5-9"~ "De 5 a 9 años",
                                     gpo_edad == "10-14" ~ "De 10 a 14 años",
                                     gpo_edad == "15-19" ~ "De 15 a 19 años",
                                     gpo_edad == "20-24" ~ "De 20 a 24 años",
                                     gpo_edad == "25-29" ~ "De 25 a 29 años",
                                     gpo_edad == "30-34" ~ "De 30 a 34 años",
                                     gpo_edad == "35-39"  ~ "De 35 a 39 años",
                                     gpo_edad == "40-44" ~ "De 40 a 44 años",
                                     gpo_edad == "45-49" ~ "De 45 a 49 años",
                                     gpo_edad == "50-54" ~ "De 50 a 54 años",
                                     gpo_edad == "55-59"  ~ "De 55 a 59 años",
                                     gpo_edad == "60-64" ~ "De 60 a 64 años",
                                     gpo_edad == "65-69" ~ "De 65 a 69 años",
                                     gpo_edad == "70-74" ~ "De 70 a 74 años",
                                     gpo_edad == "75-79"  ~ "De 75 a 79 años",
                                     gpo_edad == "MÁS DE 80" ~ "De 80 años en adelante",
                                     gpo_edad == "CIFRA SIN EDAD DE REFERENCIA" ~ "Sin especificar"))

rm(lcv, lsv, d)

# Cruzar con nombres
nom <- readRDS(files_input$ent_nom) %>% 
       rename(inegi = cve_ent)

datos <- left_join(datos, nom, by = c("inegi"))

rm(nom)


# Guardamos base de RNPDNO con edades
saveRDS(datos, files_output$datos)


##====================== Juntamos CENAPI ======================##

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
                 fuente = "CENAPI",
                 gpo_edad = case_when(edad < 4 ~ "De 0 a 4 años", 
                                      edad >= 5 & edad < 10 ~ "De 5 a 9 años",
                                      edad >= 10 & edad < 15 ~ "De 10 a 14 años",
                                      edad >= 15 & edad < 20 ~ "De 15 a 19 años",
                                      edad >= 20 & edad < 25 ~ "De 20 a 24 años",
                                      edad >= 25 & edad < 30 ~ "De 25 a 29 años",
                                      edad >= 30 & edad < 35 ~ "De 30 a 34 años",
                                      edad >= 35 & edad < 40 ~ "De 35 a 39 años",
                                      edad >= 40 & edad < 45 ~ "De 40 a 44 años",
                                      edad >= 45 & edad < 50 ~ "De 45 a 49 años",
                                      edad >= 50 & edad < 55 ~ "De 50 a 54 años",
                                      edad >= 55 & edad < 60 ~ "De 55 a 59 años",
                                      edad >= 60 & edad < 65 ~ "De 60 a 64 años",
                                      edad >= 65 & edad < 70 ~ "De 65 a 69 años",
                                      edad >= 70 & edad < 75 ~ "De 70 a 74 años",
                                      edad >= 75 & edad < 80 ~ "De 75 a 79 años",
                                      edad >= 80  ~ "De 80 años en adelante")) %>% 
          group_by(sexo, gpo_edad, inegi, status, nom_ent, fuente) %>% 
          summarise(cuenta = sum(n)) %>% 
          ungroup() %>% 
          pivot_wider(names_from = sexo, values_from = cuenta) %>% 
          select(gpo_edad, Hombre, Mujer, inegi, status, nom_ent, fuente) %>% 
          clean_names() %>% 
          left_join(., abrev) %>% 
          select(nom_ent, nom_ent2, inegi, gpo_edad, hombre, mujer, status, fuente)

# Acomodamos RNPDNO
rnpdno <- datos %>% 
           mutate(fuente = "RNPDNO") %>% 
           select(nom_ent, nom_ent2, inegi, gpo_edad, hombre, mujer, status, fuente)


# Abrimos y limpiamos pob
pob <- readRDS(files_input$pob) %>% 
       rename(nom_ent = entidad, inegi = cve_ent) %>%
       mutate(gpo_edad = case_when(edad < 4 ~ "De 0 a 4 años", 
                                   edad >= 5 & edad < 10 ~ "De 5 a 9 años",
                                   edad >= 10 & edad < 15 ~ "De 10 a 14 años",
                                   edad >= 15 & edad < 20 ~ "De 15 a 19 años",
                                   edad >= 20 & edad < 25 ~ "De 20 a 24 años",
                                   edad >= 25 & edad < 30 ~ "De 25 a 29 años",
                                   edad >= 30 & edad < 35 ~ "De 30 a 34 años",
                                   edad >= 35 & edad < 40 ~ "De 35 a 39 años",
                                   edad >= 40 & edad < 45 ~ "De 40 a 44 años",
                                   edad >= 45 & edad < 50 ~ "De 45 a 49 años",
                                   edad >= 50 & edad < 55 ~ "De 50 a 54 años",
                                   edad >= 55 & edad < 60 ~ "De 55 a 59 años",
                                   edad >= 60 & edad < 65 ~ "De 60 a 64 años",
                                   edad >= 65 & edad < 70 ~ "De 65 a 69 años",
                                   edad >= 70 & edad < 75 ~ "De 70 a 74 años",
                                   edad >= 75 & edad < 80 ~ "De 75 a 79 años",
                                   edad >= 80  ~ "De 80 años en adelante")) %>% 
       group_by(gpo_edad, nom_ent, inegi, sexo) %>% 
       summarise(pob = sum(poblacion)) %>% 
       ungroup() %>% 
       mutate(sexo = case_when(sexo == "Hombres" ~ "hombre",
                               sexo == "Mujeres" ~ "mujer"))


# Juntamos todo
base_junta <- rbind(rnpdno, cenapi) %>% 
              mutate(nom_ent = case_when(nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz", 
                                         nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
                                         nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
                                         T ~ nom_ent)) %>% 
              pivot_longer(hombre:mujer, names_to = "sexo", values_to = "cuenta")  %>% 
              left_join(., pob) 

# Guardamos base final
saveRDS(base_junta, files_output$base_junta)
