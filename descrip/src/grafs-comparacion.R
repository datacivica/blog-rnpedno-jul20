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
p_load(tidyverse,  yaml, grid, janitor, data.table, here, ggplot2, treemapify, 
       egg, ggridges,ggmosaic, svglite, viridis)


# Inputs y outputs

base <- readRDS(here("clean-data/output/desaparecidos.rds"))

grafs_output = list(
  graf1 = here("descrip/output/1.heat_anual_nominal.svg"),
  graf2 = here("descrip/output/2.heat_anual_porc.svg"),
  graf3 = here("descrip/output/3.scatter_anual.svg"),
  graf4 = here("descrip/output/4.scatter_sexo.svg"),
  graf5 = here("descrip/output/5.scatter_status.svg"),
  graf6 = here("descrip/output/6.heat_entidad.svg"),
  graf7 = here("descrip/output/7.heat_entidad_sexo.svg"),
  graf8 = here("descrip/output/8.scatter_entidad.svg")
  
)

# Tema


tema <- theme_minimal() +
  theme(plot.title = element_text(size = 16, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 10, family = "Barlow Condensed"),
        axis.title = element_text(size = 12, family = "Barlow Condensed"),
        legend.text = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"))



# Funciones útiles

prcnt <- function(x,y) {
  round((x/y)*100, digits = 2)
}

# Grafs

# I. Heatmap

# i. base vs. year: sin facet_wrap

#nominal
tempo <- base %>% 
  group_by(fuente, year) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tot)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  mutate(cut = as.character(cut(diferencia, breaks = c(-Inf,0,Inf)))) %>% 
  pivot_longer(CENAPI:diferencia, names_to = "fuente", values_to = "tot")  

ggplot(tempo, aes(x = factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = year, fill = cut)) +
  geom_tile() +
  scale_fill_manual(values = col) + 
  labs(title = "Personas desaparecidas registradas por fuente y 
       la diferencia entre las fuentes",
       subtitle = "Total anual a nivel nacional",
       x = "Fuente", y = "",
       caption = "Fuente: CENAPI y RNPEDNO") +
  guides(fill = guide_legend(title="")) +
  tema +
  theme(axis.text.x = element_text(color = ifelse(tempo$cut == "República Mexicana", 
                                          "red", "darkgray")))
ggsave(grafs_output$graf1, width = 12, height = 12)

# porcentaje
tempo <- base %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  group_by(fuente, year) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot, gran_tot)) %>% 
  ungroup () %>% 
  distinct() %>%
  select(-c(tot)) %>% 
  pivot_wider(names_from = fuente, values_from = porc)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  pivot_longer(CENAPI:diferencia, names_to = "fuente", values_to = "porc")  

ggplot(tempo, aes(x = factor(fuente, levels = c("CENAPI", "RNPEDNO", "diferencia")), 
                  y = year, fill = porc)) +
  geom_tile() +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") + 
  tema +
  labs(title = "Desaparecidos anuales por fuente y 
       la diferencia entre las fuentes",
       subtitle = "Porcentaje anual a nivel nacional",
       x = "fuente", y = "",
       caption = "Fuente: CENAPI y RNPEDNO") +
  theme(legend.position = "none") +
  tema +
  geom_text(aes(label = paste0(round(porc, digits = 2),"%"))) 

ggsave(grafs_output$graf2, width = 12, height = 12)

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
  group_by(fuente, nom_ent) %>% 
  summarise(tot = sum (cuenta, na.rm = T), pob = sum (pob), tasa = (tot/pob)*100000) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tasa)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI,
         cut = as.character(cut(diferencia, breaks = c(-Inf,0,Inf)))) %>% #Siempre tiene más RNPEDNO
  pivot_longer(CENAPI:diferencia, names_to = "fuente", values_to = "tot") %>% 
  filter(nom_ent != "No Determinado")

ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO", "diferencia")), 
                  y = reorder(nom_ent, desc(tot)), fill = tot)) +
  geom_tile(color= "black") +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs(title = "Personas desaparecidas registradas por fuente y 
       la diferencia entre las fuentes",
       subtitle = "Total anual a nivel nacional",
       x = "Fuente", y = "",
       caption = "Fuente: CENAPI y RNPEDNO") +
  tema +
  theme(axis.text.y = element_text(color = ifelse(tempo$cut == "(-Inf,0]", "red", "darkgray"))) 

a <- readRDS("/Users/aliciafranco/Desktop/pob-mit-poryecciones.rds")
ggsave(grafs_output$graf6, width = 12, height = 12)

# v. base vs. entidades: sexo

tempo <- base %>%
  group_by(sexo) %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fuente, nom_ent, sexo) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter (sexo == "hombre" | sexo == "mujer") %>% 
  select(-c(tot)) %>% 
  pivot_wider(names_from = fuente, values_from = porc)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  pivot_longer(CENAPI:diferencia, names_to = "fuente", values_to = "porc")  

ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO", "diferencia")), 
                  y = reorder(nom_ent, desc(porc)), fill = porc)) +
  facet_wrap(~ sexo) +
  geom_tile() +
  tema +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(round(porc, digits = 2),"%"))) 


ggsave(grafs_output$graf7, width = 12, height = 12)
# v. base vs. entidades: status

tempo <- base %>%
  group_by(status) %>% 
  mutate(gran_tot = sum(cuenta, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fuente, nom_ent, status) %>% 
  summarise(tot = sum (cuenta, na.rm = T), porc = prcnt(tot,gran_tot)) %>% 
  ungroup () %>% 
  distinct() %>% 
  select(-c(tot)) %>% 
  pivot_wider(names_from = fuente, values_from = porc)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  pivot_longer(CENAPI:diferencia, names_to = "fuente", values_to = "porc") %>% 
  filter(nom_ent != "No Determinado")

ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO", "diferencia")), 
                  y = reorder(nom_ent, desc(porc)), fill = porc)) +
  geom_tile() +
  facet_wrap(~ status) +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  geom_text(aes(label = paste0(round(porc, digits = 2),"%")))


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
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1 )

ggsave(grafs_output$graf3, height = 12, width = 12)


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

ggsave(grafs_output$graf4, height = 12, width = 12)
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
ggsave(grafs_output$graf5, height = 12, width = 12)

#iv. viendo estados

tempo <- base %>% 
  group_by(fuente, nom_ent, inegi) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tot) 

ggplot(tempo, aes(x=CENAPI, y = RNPEDNO)) +
  geom_point(aes(color=nom_ent)) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  scale_color_viridis(discrete = TRUE, option = "D")

ggsave(grafs_output$graf8, height = 12, width = 12)
