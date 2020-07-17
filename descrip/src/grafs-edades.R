##!/usr/bin/env Rscript --vanilla
# set expandtab tabstop=4 shiftwidth=4 ai fileencoding=utf-8
#
# Author: Mariana S
# Maintainer(s): AF, MS
# License: (c) Data Cívica 2020
#
# ------------------------------------------------------------------------------------
# blog-rnpedno-jul20/descrip/src/grafs-edades

rm(list=ls())

pacman::p_load(tidyverse, ggrepel, sf, lwgeom, yaml, here, ggmosaic, treemapify)


tema <- theme_minimal() +
        theme(plot.title = element_text(size = 20, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(size = 18, family = "Barlow Condensed", hjust = 0.5),
              plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
              axis.text = element_text(size = 12, family = "Barlow Condensed"),
              axis.title = element_text(size = 14, family = "Barlow Condensed"),
              legend.title = element_text(size = 14, family = "Barlow Condensed", hjust = 0.5),
              legend.text = element_text(size = 14, family = "Barlow Condensed", hjust = 0.5),
              strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"))

fill_dos <-  c("#7fbf7b", "#af8dc3")
fill_varios <-  c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99",
                  "#e31a1c","#fdbf6f")
fill_muchos <-  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", 
                  "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")


files <- list(datos = here("/clean-data/output/cenapi-rnpdno-edades.rds"),
              g_e1 = here("/descrip/output/g_e1.png"),
              g_e2 = here("/descrip/output/g_e2.png"),
              g_e3 = here("/descrip/output/g_e3.png"),
              g_e4 = here("/descrip/output/g_e4.png"))

datos <- readRDS(files$datos)

#Algunos ajustes
datos$gpo_edad <- ifelse(is.na(datos$gpo_edad), "Sin especificar", datos$gpo_edad)

datos <- datos %>% 
         mutate(gpo_edad = factor(gpo_edad, levels = c("De 0 a 4 años", "De 5 a 9 años", 
                                                       "De 10 a 14 años", "De 15 a 19 años", 
                                                       "De 20 a 24 años", "De 25 a 29 años", 
                                                       "De 30 a 34 años","De 35 a 39 años",
                                                       "De 40 a 44 años", "De 45 a 49 años", 
                                                       "De 50 a 54 años", "De 55 a 59 años", 
                                                       "De 60 a 64 años", "De 65 a 69 años", 
                                                       "De 70 a 74 años","De 75 a 79 años",
                                                       "De 80 años en adelante", "Sin especificar")))

##================== Gráficas ==================##
tempo <- datos %>% 
         group_by(gpo_edad, sexo, fuente, status) %>% 
         summarize(total = sum(cuenta, na.rm = T)) %>% 
         mutate(denomin = sum(total), 
                porcent = round(total / denomin * 100, 1)) 

ggplot(tempo,aes(x = gpo_edad, y = porcent, fill = status))+
       geom_bar(position = "stack", stat = "identity") + 
       facet_wrap(sexo~fuente) +
       scale_fill_manual(values = fill_varios) + 
       coord_flip() +
       geom_label(aes(label = paste0(porcent, "%")), position = position_stack(vjust = 0.5),
                  family = "Barlow Condensed", color = "black", angle = 0, size = 3) +
       labs(title = "Estatus de las personas desaparecidas, no localizadas y localizadas en México", 
            subtitle = "por edad y fuente de registro", 
            caption = "Fuente: Elaboración propia con datos del RNPDNO y CENAPI", 
            fill = "", x = "", y = "") +
       tema 

ggsave(files$g_e1, width = 18, height = 15)

tempo <- datos %>% 
         group_by(fuente, sexo, gpo_edad) %>% 
         summarize(total = sum(cuenta, na.rm = T)) %>% 
         mutate(denomin = sum(total), 
                porcent = round(total / denomin * 100, 1)) 

ggplot(tempo, aes(area = total, fill = gpo_edad, label = paste0(porcent,"%\n", total))) +
       geom_treemap() + 
       facet_wrap(fuente~sexo) +
       geom_treemap_text(place = "centre", grow = F, color ="black") +
       labs(title = "Porcentajes por edad de las personas desaparecidas, no localizadas y localizadas en México", 
            subtitle = "por fuente de registro", 
            caption = "Fuente: Elaboración propia con datos del RNPDNO y CENAPI", fill = "") +
       tema + 
       coord_fixed()
ggsave(files$g_e2, width = 18, height = 15)

ggplot(tempo,aes(x = gpo_edad, y = porcent, fill = sexo))+
       geom_bar(position = "stack", stat = "identity") + 
       facet_wrap(~fuente) +
       scale_fill_manual(values = fill_varios) + 
       coord_flip() +
       geom_text(aes(label = paste0(porcent, "%")), position = position_stack(vjust = 0.5),
                  family = "Barlow Condensed", color = "black", angle = 0, size = 3) +
       labs(title = "Porcentaje de las personas desaparecidas, no localizadas y localizadas en México", 
            subtitle = "por grupo de edad y fuente de registro", 
            caption = "Fuente: Elaboración propia con datos del RNPDNO y CENAPI", 
            fill = "", x = "", y = "") +
  tema 

ggsave(files$g_e3, width = 18, height = 15)




















