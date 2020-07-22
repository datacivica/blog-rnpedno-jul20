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
                  rnpdno = here("import/output/rnpdno.rds")
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


# === RNPDNO === #
print("working on rnpdno")
rnpdno_path <- paste0(file_paths$input, "rnpdno/")
rnpdno_files <- dir(rnpdno_path)

rnpdno <- data.frame()

pb <- txtProgressBar(min=1, max=length(rnpdno_files), style=3)
for (i in 1:length(rnpdno_files)) {
  tempo <- fread(paste0(rnpdno_path, rnpdno_files[i])) %>% 
    clean_names() %>% 
    mutate(cve_ent = parse_number(rnpdno_files[i]),
           cve_ent = formatC(cve_ent, width = 2, flag = 0, format = "d"),
           estatus = str_extract(rnpdno_files[i], "[^_]+"),
           estatus = case_when(estatus=="d" ~ "Aún sin localizar",
                               estatus=="lcv" ~ "Localizado con vida",
                               estatus=="lsv" ~ "Localizado sin vida"),
           category = as.character(category)) %>% 
    rename(year = category)
  rnpdno <- bind_rows(rnpdno, tempo)
  rm(tempo)
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb, i)

saveRDS(rnpdno, file_paths$rnpdno)

# done.