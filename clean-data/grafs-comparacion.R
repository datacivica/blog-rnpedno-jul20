##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Alicia Franco 
# Maintainer(s): Mariana Solano, Alicia Franco
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# descrip/src/heatmaps.R

  
# Archivos

require(pacman)
p_load(tidyverse, janitor, data.table, here, ggplot2, treemapify, egg, ggridges, 
       ggmosaic, svglite)

inp <- "C:\\Users\\oarev\\Documents\\alicia\\"

cenapi <- readRDS(paste0(inp, "clean-cenapi.rds")) %>% 
  rename(status = vivo_muerto, year = year_evento, inegi = clave_estado,#estados comparados
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

rnpedno <- readRDS(paste0(inp, "rnpedno.rds")) %>% 
  mutate(fuente = "RNPEDNO") %>% 
  filter(year >= 2000 & year <= 2018) #CENAPI tiene desde el 2000 y hasta el 2018, queremos comparar

base <- rbind(rnpedno, cenapi) %>% 
  pivot_longer(hombre:indeterminado, names_to = "sexo", values_to = "cuenta")

rm (cenapi, rnpedno, inp)

# Funciones utiles

prcnt <- function(x,y) {
  round((x/y)*100, digits = 2)
}

# Grafs

# I. Heatmap

# i. base vs. year: sin facet_wrap

tempo <- base %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  group_by(fuente, year) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct()
  

ggplot(tempo, aes(x = fuente, y = year, fill = porc)) +
  geom_tile(color= "black") +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(porc,"%")))

# ii. base vs year: segun sexo

tempo <- base %>%
  group_by(sexo) %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fuente, year, sexo) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter (sexo == "hombre" | sexo == "mujer")

ggplot(tempo, aes(x = fuente, y = year, fill = porc)) +
  facet_wrap(~ sexo) +
  geom_tile(color= "black") +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(porc,"%")))

# iii. base vs year: segun status

tempo <- base %>%
  group_by(status) %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fuente, year, status) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct()

ggplot(tempo, aes(x = fuente, y = year, fill = porc)) +
  facet_wrap(~ status) +
  geom_tile(color= "black") +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(porc,"%")))

# iv. base vs. entidades: sin facet_wrap
tempo <- base %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  group_by(fuente, nom_ent) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct()

ggplot(tempo, aes(x = fuente, y = nom_ent, fill = porc)) +
  geom_tile(color= "black") +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(porc,"%")))

# v. base vs. entidades: sexo

tempo <- base %>%
  group_by(sexo) %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fuente, nom_ent, sexo) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter (sexo == "hombre" | sexo == "mujer")

ggplot(tempo, aes(x = fuente, y = nom_ent, fill = porc)) +
  facet_wrap(~ sexo) +
  geom_tile(color= "black") +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(porc,"%")))

# v. base vs. entidades: status

tempo <- base %>%
  group_by(status) %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fuente, nom_ent, status) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct() 

ggplot(tempo, aes(x = fuente, y = nom_ent, fill = porc)) +
  facet_wrap(~ status) +
  geom_tile(color= "black") +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(porc,"%")))


# II. Scatters
