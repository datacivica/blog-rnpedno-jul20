##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Mariana S
# Maintainer(s): AF, MS
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-rnpedno-jul20/clean-data/src/clean-cenapi

require(pacman)
p_load(tidyverse, janitor, here)

# Archivos

files <- list(cenapi = here("import/output/cenapi.rds"),
              names_ent = here("import/output/nom_ent.rds"),
              out_cenapi = here("clean-data/output/clean-cenapi.rds"))

# Abrimos bases

df_cenapi <- readRDS(files$cenapi)
df_names <- readRDS(files$names_ent)
df_nombres2 <- readRDS(files$input_nombres2)

# Limpiamos

clean_cenapi <-rename(df_cenapi,
                      situacion = situacion_de_la_persona_en_el_registro_nacional,
                      vivo_muerto = vivo_o_muerto,
                      fecha_localizacion = fecha_de_localizacion) %>% 
  mutate(across(c("clave_estado", "clave_estado_localizado"),
                formatC,width = 2, flag = 0, format = "d")) %>% 
  mutate(across(c("clave_municipio", "clave_municipio_localizado"),
                formatC, width =3, flag = 0, format = "d")) %>% 
  mutate(across(c("fecha_evento", "fecha_localizacion"),
                str_replace, "00:00:00", "")) %>% 
  mutate(across(c("fecha_evento", "fecha_localizacion"),
                str_trim, side = "both")) %>% 
  separate(fecha_evento, into = c("dia_evento", "mes_evento", "year_evento"),
           sep = "/", remove = F) %>% 
  separate(fecha_localizacion, into = c("dia_loc", "mes_loc", "year_loc"),
           sep = "/", remove = F) %>% 
  mutate(inegi_evento = paste0(clave_estado, clave_municipio),
         inegi_localizado = paste0(clave_estado_localizado,
                                   clave_municipio_localizado),
         sexo=ifelse(sexo == "FEMENINO", "Mujer","Hombre"),
         vivo_muerto = case_when(
           vivo_muerto == "AUN SIN LOCALIZAR" ~ "Aun sin localizar",
           vivo_muerto == "VIVO" ~ "Localizado con vida",
           vivo_muerto == "MUERTO" ~ "Localizado sin vida"),
         edad = str_trim(as.character(edad), 
                         side = c("both")), 
         edad = as.numeric(edad),
         grupo_edad = case_when(
           edad < 12 ~ "Menos de 12 años",
           edad >= 12 & edad < 18  ~ "De 12 a 17 años",
           edad >= 18 & edad < 24  ~ "De 18 a 23 años",
           edad >= 24 & edad < 30  ~ "De 24 a 29 años",
           edad >= 30 & edad < 45  ~ "De 30 a 44 años",
           edad >= 45 & edad < 60  ~ "De 45 a 59 años",
           edad >= 60 & edad < 120 ~ "60 años o más",
           edad > 120 | is.na(edad) ~ "No especificado"),
         fecha_evento = lubridate::dmy(fecha_evento),
         fecha_localizacion = lubridate::dmy(fecha_localizacion),
         tiempo_busqueda = lubridate::interval(fecha_evento, fecha_localizacion),
         tiempo_busqueda = lubridate::time_length(tiempo_busqueda, unit = "month"),
         tiempo_busqueda = ifelse(tiempo_busqueda < 0, NA, tiempo_busqueda),
         tiempo_busqueda = round(tiempo_busqueda, digits = 2),
         clave_estado = case_when(estado == "NO ESPECIFICADO"  ~ "00",
                                  estado == "ZACATECAS" ~ "32",
                                  estado == "MICHOACAN" ~ "16",
                                  estado == "TAMAULIPAS"~ "28",
                                  clave_estado == "NA" ~ "33",
                                  T ~ clave_estado)) %>% 
  filter(year_evento >= 2000 & year_evento <= 2018,
         as.numeric(clave_estado) >= 1 &as.numeric(clave_estado) <= 32) %>% 
  select(vivo_muerto, fecha_evento:year_evento, clave_estado, clave_municipio, 
         inegi_evento, sexo, edad, grupo_edad, 
         clave_estado_localizado, clave_municipio_localizado, inegi_localizado, 
         fecha_localizacion:year_loc, tiempo_busqueda)


# Cruzamos con nombres

clean_cenapi <- left_join(clean_cenapi, df_nombres1, by = c("clave_estado" = "cve_ent")) %>%
  rename(nom_ent_evento = nom_ent) %>%
  left_join(., df_nombres1, by = c("clave_estado_localizado" = "cve_ent")) %>%
  rename(nom_ent_localizado = nom_ent) %>%
  left_join(., select(df_nombres2, inegi, nom_mun),
            by = c("inegi_evento" = "inegi")) %>%
  rename(nom_mun_evento = nom_mun) %>%
  left_join(., select(df_nombres2, inegi, nom_mun),
            by = c("inegi_localizado" = "inegi")) %>%
  rename(nom_mun_localizado= nom_mun) %>% 
  select(vivo_muerto, fecha_evento:year_evento, clave_estado,
         nom_ent_evento, clave_municipio, inegi_evento,
         nom_mun_evento, sexo:grupo_edad, clave_estado_localizado,
         nom_ent_localizado, clave_municipio_localizado,
         inegi_localizado, nom_mun_localizado, 
         fecha_localizacion:year_loc, tiempo_busqueda)

saveRDS(clean_cenapi, files$output_cenapi)
