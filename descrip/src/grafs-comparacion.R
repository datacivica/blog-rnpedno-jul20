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
       egg, ggridges,ggmosaic, svglite, viridis, scales, ggrepel)


# Inputs y outputs

base <- readRDS(here("clean-data/output/desaparecidos.rds"))

g_output = list(
  g_1 = here("descrip/output/g_1.svg"),
  g_2 = here("descrip/output/g_2.svg"),
  g_3 = here("descrip/output/g_3.svg"),
  g_4 = here("descrip/output/g_4.svg"),
  g_5 = here("descrip/output/g_5.svg"),
  g_6 = here("descrip/output/g_6.svg"),
  g_7 = here("descrip/output/g_7.svg"),
  g_8 = here("descrip/output/g_8.svg")
  
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
# i. Fuente y diferencia entre estados, sin facet

tempo <- base %>% 
  group_by(fuente, nom_ent) %>% 
  summarise(tot = sum (cuenta, na.rm = T), pob = sum (pob), tasa = (tot/pob)*100000) %>% 
  ungroup () %>% 
  distinct() %>% 
  select(-c(tot, pob)) %>% 
  filter(nom_ent != "No Determinado")  %>% 
  pivot_wider(names_from = fuente, values_from = tasa)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI,
         cut = ifelse(diferencia<=0, 1, 0)) %>% 
  pivot_longer(CENAPI:RNPEDNO, names_to = "fuente", values_to = "tasa") %>% 
  arrange(desc(nom_ent))

tempo2 <- tempo %>% select(nom_ent, diferencia) %>% 
  distinct(nom_ent, .keep_all = T)


ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = nom_ent, fill = tasa)) +
  geom_tile() +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs(title = " Tasa de personas desaparecidas registradas por fuente y entidad",
       subtitle = "Por cada 100,000 habitantes de 2000 a 2018",
       x = "Fuente", y = "",
       caption = "Fuente: CENAPI y RNPEDNO \n En los estados resaltados de rojo se registró mayor tasa de personas desaparecidas en el CENAPI que en el RNPEDNO") +
  tema +
  geom_text(aes(label = as.character(round(tasa, digits = 2)))) +
  theme(axis.text.y = element_text(color = ifelse(tempo2$diferencia <= 0, "red", "black"))) 

ggsave(g_output$g_1, width = 8, height = 12)

#Otra opción

g1 <- ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = nom_ent, fill = tasa)) +
  geom_tile() +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs(title = " Tasa de personas desaparecidas registradas por fuente y entidad",
       subtitle = "Por cada 100,000 habitantes de 2000 a 2018",
       x = "Fuente", y = "",
       caption = "Fuente: CENAPI y RNPEDNO ") +
  tema +
  geom_text(aes(label = as.character(round(tasa, digits = 2)))) 


tempo2 <- tempo %>% 
  select(nom_ent, fuente, tasa) %>% 
  pivot_wider(names_from = fuente, values_from = tasa)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  select(nom_ent, diferencia) %>% 
  group_by(nom_ent) %>% 
  summarise(diferencia = sum(diferencia)) %>% 
  ungroup() %>% 
  mutate(fuente = "diferencia")

g2 <- ggplot(tempo2, aes(x = fuente, y = nom_ent, fill = diferencia, width =0.4)) +
  geom_tile() +
  tema +
  scale_fill_gradientn(colours = c("red","white","green"), 
                       values = rescale(c(-1.7,0,9)),
                       guide = "colorbar", limits=c(-1.7,9)) +
  geom_text(aes(label = round(diferencia, digits = 2))) +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,-0.4,0.2,0.2), "cm")) +
  labs(y = "", x = "")

g3 <- egg::ggarrange(g1, g2, ncol = 2)

ggsave(g_output$g_3, width = 8, height = 12)


# i. Fuente y diferencia entre estados, por sexo
tempo <- base %>%
  group_by(fuente, nom_ent, sexo) %>% 
  summarise(tot = sum (cuenta, na.rm = T), pob = sum(pob), tasa = (tot/pob)*100000) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter (sexo == "hombre" | sexo == "mujer") %>% 
  filter(nom_ent != "No Determinado")


tempo2 <- tempo %>% 
  select(nom_ent, fuente, tasa, sexo) %>% 
  pivot_wider(names_from = fuente, values_from = tasa)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  select(nom_ent, sexo, diferencia) %>% 
  group_by(nom_ent) %>% 
  summarise(diferencia = sum(diferencia)) %>% 
  ungroup() %>% 
  mutate(fuente = "diferencia")

g1 <- ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = nom_ent, fill = tasa)) +
  facet_wrap(~ sexo) +
  geom_tile() +
  tema +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs (title = "Tasa de personas desaparecidas registradas por fuente, sexo y entidad",
              subtitle = "Por 100,000 de 2000 a 2018",
        y = "", x = "Fuente", caption = "Fuente: CENAPI y RNPEDNO") +
  geom_text(aes(label = round(tasa, digits = 2))) +
  theme(legend.position = "none")
  

g2 <- ggplot(tempo2, aes(x = fuente, y = nom_ent, fill = diferencia, width =0.4)) +
  geom_tile() +
  tema +
  scale_fill_gradientn(colours = c("red","white","green"), 
                     values = rescale(c(-1.7,0,9)),
                     guide = "colorbar", limits=c(-1.7,9)) +
  geom_text(aes(label = round(diferencia, digits = 2))) +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,-0.4,0.2,0.2), "cm")) +
  labs(y = "", x = "")

g3 <- egg::ggarrange(g1, g2, ncol = 2)

ggsave(g_output$g_2, width = 8, height = 12)

# iii. Fuente y diferencia entre estados, por status

tempo <- base %>%
  group_by(fuente, nom_ent, status) %>% 
  summarise(tot = sum (cuenta, na.rm = T), pob = sum(pob), tasa = (tot/pob)*100000) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter(nom_ent != "No Determinado")

tempo2 <- tempo %>%
  select(nom_ent, fuente, tasa, status) %>% 
  pivot_wider(names_from = fuente, values_from = tasa)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  select(nom_ent, status, diferencia) %>% 
  group_by(nom_ent) %>% 
  summarise(diferencia = sum(diferencia)) %>% 
  ungroup() %>% 
  mutate(fuente = "diferencia")

g1 <- ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = nom_ent, fill = tasa)) +
  facet_wrap(~ status) +
  geom_tile() +
  tema +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs (title = "Tasa de personas desaparecidas registradas por fuente, status y entidad",
        subtitle = "Por 100,000 de 2000 a 2018",
        y = "", x = "Fuente", caption = "Fuente: CENAPI y RNPEDNO ") +
  geom_text(aes(label = round(tasa, digits = 2))) +
  theme(legend.position = "none")

g2 <- ggplot(tempo2, aes(x = fuente, y = nom_ent, fill = diferencia, width =0.4)) +
  geom_tile() +
  tema +
  scale_fill_gradientn(colours = c("red","white","green"), 
                       values = rescale(c(-2,0,13)),
                       guide = "colorbar", limits=c(-2,13)) +
  geom_text(aes(label = round(diferencia, digits = 2))) +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,-0.4,0.2,0.2), "cm")) +
  labs(y = "", x = "")
g3 <- egg::ggarrange(g1, g2, ncol = 2)

ggsave(g_output$g_4, width = 8, height = 12)

# Scatter

#iv. viendo estados

tempo <- base %>% 
  group_by(nom_ent2) %>% 
  mutate(pob = sum(pob)) %>% 
  ungroup () %>% 
  group_by(fuente, nom_ent2, pob) %>% 
  summarise(tasa = (sum(cuenta,na.rm = T)/pob)*100000) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tasa) 

ggplot(tempo, aes(x=CENAPI, y = RNPEDNO)) +
  geom_point(shape = 21) +
  geom_label_repel(aes(label = nom_ent2), show.legend = F) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs (title = "Tasa de personas desaparecidas registradas por fuente y entidad",
        subtitle = "Por cada 100,000 habitantes de 2000 a 2018",
        caption = "Fuente: CENAPI y RNPEDNO") +
  tema

ggsave(g_output$g_8, height = 12, width = 12)

# Fiebres


