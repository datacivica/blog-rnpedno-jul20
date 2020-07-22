#
# Author: Alicia Franco, Mariana Solano
# Maintainer(s): Alicia Franco, Mariana Solano, Oscar Elton
# License: (c) Data Cívica 2020
#
# ---------------------------------------------------------
# blog-rnpedno-jul20/import/src/import.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, janitor, data.table, readxl, stringr, here)

# === File paths === #
file_paths = list(input = here("import/input/"),
                  cenapi = here("import/output/cenapi.rds"),
                  nom_ent = here("import/output/nombre-entidad.rds"),
                  pob = here("import/output/poblacion.rds"),
                  rnpdno_tot = here("import/output/rnpdno-ent.rds"),
                  rnpdno_edad = here("import/output/rnpdno-edad.rds")
                  )


# === CENAPI === #
print("working on CENAPI")
cenapi <- fread(paste0(file_paths$input, "cenapi/cenapi.csv")) %>% 
  clean_names() %>% 
  select(fecha_evento, estado, clave_estado, sexo, edad, vivo_o_muerto) %>%
  rename(nom_ent = estado,
         cve_ent = clave_estado,
         estatus = vivo_o_muerto) %>% 
  mutate(cve_ent = formatC(cve_ent, width = 2, flag = 0, format = "d"))

saveRDS(cenapi, file_paths$cenapi)


# === NOMBRES DE ENTIDADES === #
print("working on nombres entidades")
nom_ent <- fread(paste0(file_paths$input, "nombres-entidades.csv")) %>%
  clean_names() %>% 
  rename(cve_ent = clave_de_entidad,
         nom_ent = nombre_de_la_entidad_federativa) %>% 
  mutate(cve_ent = formatC(cve_ent, width = 2, flag = 0, format = "d"))
saveRDS(nom_ent, file_paths$nom_ent)


# === POBLACION === #
print("working on poblacion")
pob <- fread(paste0(file_paths$input, "pob-mit-proyecciones.csv")) %>%
  clean_names() %>% 
  rename(year = ano,
         nom_ent = entidad,
         cve_ent = cve_geo) %>% 
  mutate(cve_ent = formatC(cve_ent, width = 2, flag = 0, format = "d"))
saveRDS(pob, file_paths$pob)


# === RNPDNO POR ENTIDAD === #
print("working on rnpdno por entidad")
rnpdno_tot_path <- paste0(file_paths$input, "rnpdno/totales/")
rnpdno_tot_files <- dir(rnpdno_tot_path)

rnpdno_tot <- data.frame()

pb <- txtProgressBar(min=1, max=length(rnpdno_tot_files), style=3)
for (i in 1:length(rnpdno_tot_files)) {
  tempo <- fread(paste0(rnpdno_tot_path, rnpdno_tot_files[i])) %>% 
    clean_names() %>% 
    mutate(cve_ent = parse_number(rnpdno_tot_files[i]),
           cve_ent = formatC(cve_ent, width = 2, flag = 0, format = "d"),
           estatus = str_extract(rnpdno_tot_files[i], "[^_]+"),
           estatus = case_when(estatus=="d" ~ "Aún sin localizar",
                               estatus=="lcv" ~ "Localizado con vida",
                               estatus=="lsv" ~ "Localizado sin vida"),
           category = as.character(category)) %>% 
    rename(year = category)
  rnpdno_tot <- bind_rows(rnpdno_tot, tempo)
  rm(tempo)
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb, i)

saveRDS(rnpdno_tot, file_paths$rnpdno_tot)


# === RNPDNO POR EDADES === #
print("working on rnpdno por edades")
rnpdno_age_path <- paste0(file_paths$input, "rnpdno/rango-edad/")
rnpdno_age_files <- dir(rnpdno_age_path)

rnpdno_edad <- data.frame()

pb <- txtProgressBar(min=1, max=length(rnpdno_tot_files), style=3)
for (i in 1:length(rnpdno_tot_files)) {
  
  tempo <- fread(paste0(rnpdno_age_path, rnpdno_age_files[i])) %>% 
    clean_names() %>% 
    mutate(cve_ent = parse_number(rnpdno_tot_files[i]),
           cve_ent = formatC(cve_ent, width = 2, flag = 0, format = "d"),
           estatus = str_extract(rnpdno_tot_files[i], "[^_]+"),
           estatus = case_when(estatus=="d" ~ "Aún sin localizar",
                               estatus=="lcv" ~ "Localizado con vida",
                               estatus=="lsv" ~ "Localizado sin vida"),
           category = as.character(category)) %>% 
    rename(rango_edad = category)
  rnpdno_edad <- bind_rows(rnpdno_edad, tempo)
  rm(tempo)
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb, i)

saveRDS(rnpdno_edad, file_paths$rnpdno_edad)

# done.