##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Alicia Franco 
# Maintainer(s): Mariana Solano, Alicia Franco
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# descrip/src/grafs-comparacion.R

  
# Paquetes

require(pacman)
p_load(tidyverse, janitor, data.table, here, ggplot2, treemapify, 
       egg, ggridges,ggmosaic, svglite, viridis)

# Inputs y outputs

base <- readRDS(here("clean-data/output/desaparecidos.rds"))

grafs_output = list(
  graf1 = here("descrip/output/.svg"),
  graf2 = here("descrip/output/.svg")
)

# Funciones útiles

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

#i. viendo los años
tempo <- base %>% 
  group_by(fuente, year,inegi) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tot) %>% 
  mutate(dif=RNPEDNO-CENAPI)

ggplot(tempo, aes(x=CENAPI, y = RNPEDNO)) +
  geom_point(aes(color=year)) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  scale_color_viridis(discrete = TRUE, option = "D")



#ii. viendo sexo
tempo <- base %>% 
  group_by(fuente, year,inegi, sexo) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tot) %>% 
  filter(sexo != "indeterminado")

ggplot(tempo, aes(x=CENAPI, y = RNPEDNO)) +
  geom_point(aes(color=sexo)) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  scale_color_viridis(discrete = TRUE, option = "D")

#iii. viendo status
tempo <- base %>% 
  group_by(fuente, year, inegi, status) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tot) 

ggplot(tempo, aes(x=CENAPI, y = RNPEDNO)) +
  geom_point(aes(color=status)) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  scale_color_viridis(discrete = TRUE, option = "D")


