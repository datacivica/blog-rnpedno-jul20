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
p_load(tidyverse,  yaml, grid, janitor, here, ggplot2, 
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
  g_8 = here("descrip/output/g_8.svg"),
  g_9 = here("descrip/output/g_9.svg")
  
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


# Grafs

# I. Heatmap
# i. Fuente y diferencia entre estados, sin facet

tempo <- base %>% 
  group_by(fuente, nom_ent) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter(nom_ent != "No Determinado")  %>% 
  pivot_wider(names_from = fuente, values_from = tot)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI,
         cut = ifelse(diferencia<=0, 1, 0)) %>% 
  pivot_longer(CENAPI:RNPEDNO, names_to = "fuente", values_to = "tot") %>% 
  arrange(desc(nom_ent))

g1 <- ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = nom_ent, fill = tot)) +
  geom_tile() +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs(title = "Personas desaparecidas, localizadas con o sin vida registradas",
       subtitle = "Por fuente y entidad (2000 - 2018)",
       x = "Fuente", y = "",
       caption = "Fuente: CENAPI y RNPDNO ") +
  tema +
  geom_text(aes(label = as.character(round(tot, digits = 2))))  +
  theme(legend.position = "none")


tempo2 <- tempo %>% 
  select(nom_ent, fuente, tot) %>% 
  pivot_wider(names_from = fuente, values_from = tot)  %>% 
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
                       values = rescale(c(-1400,0,5000)),
                       guide = "colorbar", limits=c(-1400,5000)) +
  geom_text(aes(label = round(diferencia, digits = 2))) +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,-0.4,0.2,0.2), "cm")) +
  labs(y = "", x = "")

g3 <- egg::ggarrange(g1, g2, ncol = 2)

ggsave(g_output$g_3, g3, width = 8, height = 12)


# i. Fuente y diferencia entre estados, por sexo
tempo <- base %>%
  group_by(fuente, nom_ent, sexo) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter (sexo == "hombre" | sexo == "mujer") %>% 
  filter(nom_ent != "No Determinado")


tempo2 <- tempo %>% 
  select(nom_ent, fuente, tot, sexo) %>% 
  pivot_wider(names_from = fuente, values_from = tot)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  select(nom_ent, sexo, diferencia) %>% 
  group_by(nom_ent) %>% 
  summarise(diferencia = sum(diferencia)) %>% 
  ungroup() %>% 
  mutate(fuente = "diferencia")

g1 <- ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = nom_ent, fill = tot)) +
  facet_wrap(~ sexo) +
  geom_tile() +
  tema +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs (title = "Personas desaparecidas, localizadas con o sin vida registradas",
        subtitle = "Por sexo, fuente y entidad (2000 - 2018)",
        y = "", x = "Fuente", caption = "Fuente: CENAPI y RNPDNO") +
  geom_text(aes(label = tot)) +
  theme(legend.position = "none")
  

g2 <- ggplot(tempo2, aes(x = fuente, y = nom_ent, fill = diferencia, width = 0.4)) +
  geom_tile() +
  tema +
  scale_fill_gradientn(colours = c("red","white","green"), 
                     values = rescale(c(-1400,0,5000)),
                     guide = "colorbar", limits=c(-1400,5000)) +
  geom_text(aes(label = round(diferencia, digits = 2))) +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,-0.4,0.2,0.2), "cm")) +
  labs(y = "", x = "")

g3 <- egg::ggarrange(g1, g2, ncol = 2)

ggsave(g_output$g_2, g3,width = 18, height = 12)

# iii. Fuente y diferencia entre estados, por status

tempo <- base %>%
  group_by(fuente, nom_ent, status) %>% 
  summarise(tot = sum (cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  filter(nom_ent != "No Determinado")

tempo2 <- tempo %>%
  select(nom_ent, fuente, tot, status) %>% 
  pivot_wider(names_from = fuente, values_from = tot)  %>% 
  mutate(diferencia = RNPEDNO-CENAPI) %>% 
  select(nom_ent, status, diferencia) %>% 
  group_by(nom_ent) %>% 
  summarise(diferencia = sum(diferencia)) %>% 
  ungroup() %>% 
  mutate(fuente = "diferencia")

g1 <- ggplot(tempo, aes(x =  factor(fuente, levels = c("CENAPI", "RNPEDNO")), 
                  y = nom_ent, fill = tot)) +
  facet_wrap(~ status) +
  geom_tile() +
  tema +
  scale_fill_continuous(low = "aliceblue", high = "midnightblue") +
  labs (title = "Personas desaparecidas, localizadas con o sin vida registradas",
        subtitle = "Por status, fuente y entidad (2000 - 2018)",
        y = "", x = "Fuente", caption = "Fuente: CENAPI y RNPEDNO ") +
  geom_text(aes(label = tot)) +
  theme(legend.position = "none")

g2 <- ggplot(tempo2, aes(x = fuente, y = nom_ent, fill = diferencia, width =0.4)) +
  geom_tile() +
  tema +
  scale_fill_gradientn(colours = c("red","white","green"), 
                       values = rescale(c(-1400,0,4600)),
                       guide = "colorbar", limits=c(-1400,4600)) +
  geom_text(aes(label = round(diferencia, digits = 2))) +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,-0.4,0.2,0.2), "cm")) +
  labs(y = "", x = "")
g3 <- egg::ggarrange(g1, g2, ncol = 2)

ggsave(g_output$g_4, g3,width = 18, height = 12)

# Scatter

tempo <- base %>% 
  filter(nom_ent!= "No Determinado") %>% 
  group_by(fuente, nom_ent2) %>% 
  summarise(tot = sum(cuenta, na.rm = T)) %>% 
  ungroup () %>% 
  distinct() %>% 
  pivot_wider(names_from = fuente, values_from = tot) 

ggplot(tempo, aes(x=CENAPI, y = RNPEDNO)) +
  geom_point(shape = 21) +
  geom_label_repel(aes(label = nom_ent2), show.legend = F) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs (title = "Personas desaparecidas, localizadas con o sin vida registradas",
        subtitle = "Por fuente y entidad (2000 - 2018)",
        caption = "Fuente: CENAPI y RNPDNO") +
  tema

ggsave(g_output$g_5, height = 12, width = 12)

# Fiebres

tempo <- base %>% 
  group_by(year,fuente) %>% 
  summarise(tot = sum(cuenta, na.rm = T)) %>% 
  ungroup() %>% 
  distinct () %>% 
  pivot_wider(names_from = fuente, values_from = tot) 

 ggplot(tempo, aes(x = as.numeric(year), y = RNPEDNO)) +
  geom_line(aes(y = RNPEDNO,  color = "RNPDNO")) +
  geom_line(aes(y = CENAPI, color = "CENAPI")) +
  scale_color_manual(values = c('RNPDNO' = 'midnightblue','CENAPI' = 'purple')) +
  labs (title = "Personas desaparecidas, localizadas con o sin vida registradas",
        subtitle = "Por fuente  (2000 - 2018)",
        caption = "Fuente: CENAPI y RNPDNO",
        x = "", y = "", color = "Fuente") +
  geom_ribbon(aes(ymin=CENAPI, ymax=RNPEDNO), fill = "lightyellow" , alpha = .5) +
  tema 
  
 
 ggsave(g_output$g_6, height = 12, width = 12)

 #facetwrapeamos por sexo
 
 tempo <- base %>% 
   group_by(year, fuente, sexo) %>% 
   summarise(tot = sum(cuenta, na.rm = T)) %>% 
   ungroup() %>% 
   distinct () %>% 
   pivot_wider(names_from = fuente, values_from = tot) 
 
 ggplot(tempo, aes(x = as.numeric(year), y = RNPEDNO)) +
   geom_line(aes(y = RNPEDNO,  color = "RNPDNO")) +
   geom_line(aes(y = CENAPI, color = "CENAPI")) +
   facet_wrap(~sexo) +
   scale_color_manual(values = c('RNPDNO' = 'midnightblue','CENAPI' = 'purple')) +
   labs (title = "Personas desaparecidas, localizadas con o sin vida registradas",
         subtitle = "Por sexo y fuente  (2000 - 2018)",
         caption = "Fuente: CENAPI y RNPDNO",
         x = "", y = "", color = "Fuente") +
   geom_ribbon(aes(ymin=CENAPI, ymax=RNPEDNO), fill = "lightyellow" , alpha = .5) +
   tema
 
 ggsave(g_output$g_7, height = 12, width = 12)
 
 
 #facetwrapeamos por status
 
 tempo <- base %>% 
   group_by(year, fuente, status) %>% 
   summarise(tot = sum(cuenta, na.rm = T)) %>% 
   ungroup() %>% 
   distinct () %>% 
   pivot_wider(names_from = fuente, values_from = tot) 
 
 ggplot(tempo, aes(x = as.numeric(year), y = RNPEDNO)) +
   geom_line(aes(y = RNPEDNO,  color = "RNPDNO")) +
   geom_line(aes(y = CENAPI, color = "CENAPI")) +
   facet_wrap(~status) +
   scale_color_manual(values = c('RNPDNO' = 'midnightblue','CENAPI' = 'purple')) +
   labs (title = "Personas desaparecidas, localizadas con o sin vida registradas",
         subtitle = "Por status y fuente  (2000 - 2018)",
         caption = "Fuente: CENAPI y RNPDNO",
         x = "", y = "", color = "Fuente") +
   geom_ribbon(aes(ymin=CENAPI, ymax=RNPEDNO), fill = "lightyellow" , alpha = .5) +
   tema
 ggsave(g_output$g_8, height = 12, width = 12)

 #facetwrapeamos por entidad
 
 tempo <- base  %>% 
   filter(nom_ent != "No Determinado") %>% 
   group_by(year, fuente, nom_ent2) %>% 
   summarise(tot = sum(cuenta, na.rm = T)) %>% 
   ungroup() %>% 
   distinct () %>% 
   pivot_wider(names_from = fuente, values_from = tot) 
 
 ggplot(tempo, aes(x = as.numeric(year), y = RNPEDNO)) +
   geom_line(aes(y = RNPEDNO,  color = "RNPDNO")) +
   geom_line(aes(y = CENAPI, color = "CENAPI")) +
   facet_wrap(~nom_ent2, nrow = 4) +
   labs (title = "Personas desaparecidas, localizadas con o sin vida registradas",
         subtitle = "Por sexo y fuente  (2000 - 2018)",
         caption = "Fuente: CENAPI y RNPDNO",
         x = "", y = "", color = "Fuente") +
   scale_color_manual(values = c('RNPDNO' = 'midnightblue','CENAPI' = 'purple')) +
   geom_ribbon(aes(ymin=CENAPI, ymax=RNPEDNO), fill = "lightyellow" , alpha = .5) +
   tema +
   theme(axis.text.x = element_text(angle = 90))
 
 ggsave(g_output$g_9, height = 12, width = 12)
 
 