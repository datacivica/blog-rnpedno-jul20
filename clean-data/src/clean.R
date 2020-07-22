#
# Author: Mariana S
# Maintainer(s): AF, MS, OE
# License: (c) Data Cívica 2020
#
# ------------------------------------------
# blog-rnpedno-jul20/clean-data/src/clean.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, stringr, here)

# === File paths === #
files <- list(inp_cenapi = here("import/output/cenapi.rds"),
              inp_noment = here("import/output/nombre-entidad.rds"),
              inp_rnpdno = here("import/output/rnpdno.rds"),
              out_cenapi = here("clean-data/output/cenapi.rds"),
              out_rnpdno = here("clean-data/output/rnpdno.rds")
              )

noment <- readRDS(files$inp_noment)

keep_years <- 2000:2018

# === CENAPI === #
cenapi <- readRDS(files$inp_cenapi) %>%
  mutate(fecha_evento = word(fecha_evento, 1),
         sexo = case_when(sexo=="MASCULINO" ~ "Hombres",
                          T ~ "Mujeres"),
         cve_ent = case_when(nom_ent == "NO ESPECIFICADO"  ~ "99",
                             nom_ent == "ZACATECAS" ~ "32",
                             nom_ent == "MICHOACAN" ~ "16",
                             nom_ent == "TAMAULIPAS"~ "28",
                             cve_ent == "NA" ~ "99",
                             T ~ cve_ent),
         year = year(as.Date(fecha_evento)),
         mes = month(as.Date(fecha_evento)),
         dia = day(as.Date(fecha_evento)),
         estatus = case_when(estatus == "AUN SIN LOCALIZAR" ~ "Aún sin localizar",
                             estatus == "VIVO" ~ "Localizado con vida",
                             estatus == "MUERTO" ~ "Localizado sin vida"),
         edad = as.integer(edad),
         edad = case_when(edad %in% 1:118 ~ edad,
                          T ~ NA_integer_),
         grupo_edad = case_when(edad < 12 ~ "Menos de 12 años",
                                edad %in% 12:17  ~ "De 12 a 17 años",
                                edad %in% 18:23 ~ "De 18 a 23 años",
                                edad %in% 24:29 ~ "De 24 a 29 años",
                                edad %in% 30:44 ~ "De 30 a 44 años",
                                edad %in% 45:59 ~ "De 45 a 59 años",
                                edad>=60 & !is.na(edad)  ~ "60 años o más",
                                is.na(edad) ~ "No especificado")) %>% 
  filter(year %in% keep_years) %>% 
  select(-fecha_evento, -nom_ent) %>% 
  left_join(noment)

saveRDS(cenapi, files$out_cenapi)


# === RNPDNO ENTIDAD === #
rnpdno <- readRDS(files$inp_rnpdno) %>% 
  mutate(year = as.integer(year),
         year = case_when(is.na(year) ~ as.integer(9999),
                          T ~ year)) %>% 
  rename(Hombres = hombre, 
         Mujeres = mujer,
         Indeterminados = indeterminado) %>% 
  pivot_longer(cols = -c(year, cve_ent, estatus),
               names_to = "sexo",
               values_to = "tot_desp") %>% 
  left_join(noment)

saveRDS(rnpdno, files$out_rnpdno)

# done.
