#
# Author: Alicia Franco 
# Maintainer(s): Mariana Solano, Alicia Franco, Oscar Elton
# License: (c) Data Cívica 2020
#
# -----------------------------------------------------------
# blog-rnpedno-jul20/descriptives/src/descriptives.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, stringr,
               svglite, egg, grid, scales, extrafont, here)
extrafont::loadfonts(quiet = T)

# === Files === #
files = list(cenapi = here("clean-data/output/cenapi.rds"),
             rnpdno = here("clean-data/output/rnpdno.rds"),
             plot1_png = here("descriptives/output/heatmap-diff.png"),
             plot1_svg = here("descriptives/output/heatmap-diff.svg"),
             plot2_png = here("descriptives/output/fiebre-estatus.png"),
             plot2_svg = here("descriptives/output/fiebre-estatus.svg"),
             plot3_png = here("descriptives/output/fiebre-ent.png"),
             plot3_svg = here("descriptives/output/fiebre-ent.svg")
             )

cenapi <- readRDS(files$cenapi)
rnpdno <- readRDS(files$rnpdno)

tema <- theme_minimal() +
  theme(plot.title = element_text(size = 16, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 10, family = "Barlow Condensed"),
        axis.title = element_text(size = 12, family = "Barlow Condensed"),
        legend.text = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"))

# === HEATMAP === #

tempo <- cenapi %>% 
  filter(year!=9999 & cve_ent!=99) %>% 
  group_by(year, nom_ent, cve_ent) %>% 
  summarise(tot_desp = n()) %>% 
  ungroup() %>% 
  mutate(tot_desp = ifelse(year==2018, tot_desp*3, tot_desp)) %>% 
  group_by(nom_ent, cve_ent) %>% 
  summarise(tot_desp = sum(tot_desp)) %>% 
  ungroup() %>% 
  mutate(source = "CENAPI") %>% 
  bind_rows(rnpdno %>% 
              filter(year!=9999 & cve_ent!=99) %>% 
              group_by(nom_ent, cve_ent) %>% 
              summarise(tot_desp = sum(tot_desp)) %>% 
              ungroup() %>% 
              mutate(source = "RNPEDNO")) %>% 
  pivot_wider(names_from = source, values_from = tot_desp) %>% 
  mutate(diff = RNPEDNO-CENAPI,
         cut = ifelse(diff<=0, 1, 0)) %>% 
  pivot_longer(CENAPI:RNPEDNO, names_to = "source", values_to = "tot_desp") %>% 
  mutate(nom_ent = fct_reorder(nom_ent, -diff))
  
plot1 <- ggplot(tempo, aes(x = source, y = nom_ent, fill = tot_desp)) +
  geom_tile() +
  scale_fill_continuous(low = "#edf8fb", high = "#88419d") +
  labs(title = "Personas desaparecidas y localizadas con o sin vida",
       subtitle = "Por fuente y entidad (2000 - 2018)",
       x = "Fuente", y = "",
       caption = "Elaboración propia con datos del CENAPI y RNPEDNO.\nCENAPI 2018 anualizado.") +
  tema +
  geom_text(aes(label = as.character(tot_desp)), family = "Barlow Condensed")  +
  theme(legend.position = "none")

plot2 <- ggplot(tempo %>% select(nom_ent, diff) %>% unique, 
             aes(x = "RNPEDNO - CENAPI", y = nom_ent, fill = diff, width =0.5)) +
  geom_tile() +
  tema +
  scale_fill_gradientn(colours = c("#ca0020", "white", "#0571b0"), 
                       values = rescale(c(-1350,0,10500)),
                       guide = "colorbar", limits=c(-1350,10500)) +
  geom_text(aes(label = diff), family = "Barlow Condensed") +
  theme(axis.text.y = element_blank(),
        plot.margin = unit(c(0.2,-0.4,0.2,0.2), "cm"),
        legend.position = "none") +
  labs(title = "Diferencia", y = "", x = "")

heatmap <- egg::ggarrange(plot1, plot2, ncol = 2)

ggsave(files$plot1_png, heatmap, width = 8, height = 12)
ggsave(files$plot1_svg, heatmap, width = 8, height = 12)


# === FIEBRE POR ESTATUS === #
tempo <- cenapi %>% 
  group_by(year, estatus) %>% 
  summarise(tot_desp = n()) %>% 
  ungroup() %>% 
  mutate(tot_desp = ifelse(year==2018, tot_desp*3, tot_desp),
         source = "CENAPI") %>% 
  bind_rows(rnpdno %>% 
              group_by(year, estatus) %>% 
              summarise(tot_desp = sum(tot_desp)) %>% 
              ungroup() %>% 
              mutate(source = "RNPEDNO")) %>% 
  pivot_wider(names_from = source, values_from = tot_desp)

 ggplot(tempo, aes(x = year)) +
   geom_ribbon(aes(ymin=CENAPI, ymax=RNPEDNO), fill = "#969696" , alpha = .5) +
   geom_line(aes(y = RNPEDNO, color = "RNPEDNO"), size = 2) +
   geom_line(aes(y = CENAPI, color = "CENAPI"), size = 2) +
   geom_point(aes(y = RNPEDNO, color = "RNPEDNO"), size = 2) +
   geom_point(aes(y = CENAPI, color = "CENAPI"), size = 2) +
   facet_wrap(~estatus, nrow = 3, scales = "free") +
   scale_color_manual(values = c('RNPEDNO' = '#ca0020','CENAPI' = '#0571b0')) +
   scale_x_continuous(breaks = seq(from=min(tempo$year), to=max(tempo$year), by=2)) + 
   labs(title = "Tendencias por fuente y estatus",
         subtitle = "2000 - 2018",
         caption = "Fuente: Elaboración propia con datos del CENAPI y RNPEDNO.\nCENAPI 2018 anualizado.",
         x = "", y = "", color = "Fuente") +
   tema +
   theme(legend.position = "top",
         axis.text.x = element_text(size = 12, face = "bold"))
 
 ggsave(files$plot2_png, height = 14, width = 14)
 ggsave(files$plot2_svg, height = 14, width = 14)
 

 # === FIEBRE POR ENTIDAD === #
 tempo <- cenapi %>% 
   filter(cve_ent!=99) %>% 
   group_by(year, abreviacion, cve_ent) %>% 
   summarise(tot_desp = n()) %>% 
   ungroup() %>% 
   mutate(tot_desp = ifelse(year==2018, tot_desp*3, tot_desp),
          source = "CENAPI") %>% 
   bind_rows(rnpdno %>% 
               filter(year!=9999 & cve_ent!=99) %>% 
               group_by(year, abreviacion, cve_ent) %>% 
               summarise(tot_desp = sum(tot_desp)) %>% 
               ungroup() %>% 
               mutate(source = "RNPEDNO"))
 
 order_var <- tempo %>% 
   group_by(cve_ent, source) %>% 
   summarise(total_periodo = sum(tot_desp)) %>% 
   ungroup() %>%
   pivot_wider(names_from = source, values_from = total_periodo) %>% 
   mutate(diff = RNPEDNO-CENAPI) %>% 
   select(cve_ent, diff)
 
 tempo <- tempo %>% 
   left_join(order_var) %>% 
   mutate(abreviacion = fct_reorder(abreviacion, -diff)) %>% 
   pivot_wider(names_from = source, values_from = tot_desp)
 
 ggplot(tempo, aes(x = year)) +
   geom_ribbon(aes(ymin=CENAPI, ymax=RNPEDNO), fill = "#969696" , alpha = .5) +
   geom_line(aes(y = RNPEDNO, color = "RNPEDNO"), size = 1) +
   geom_line(aes(y = CENAPI, color = "CENAPI"), size = 1) +
   geom_point(aes(y = RNPEDNO, color = "RNPEDNO"), size = 1) +
   geom_point(aes(y = CENAPI, color = "CENAPI"), size = 1) +
   facet_wrap(~abreviacion, nrow = 4, scales = "free") +
   scale_color_manual(values = c('RNPEDNO' = '#ca0020','CENAPI' = '#0571b0')) +
   scale_x_continuous(breaks = seq(from=min(tempo$year), to=max(tempo$year), by=4)) + 
   labs(title = "Tendencias por fuente y entidad",
        subtitle = "2000 - 2018, ordenados de las que más crecieron a las que más decrecieron",
        caption = "Fuente: Elaboración propia con datos del CENAPI y RNPEDNO.\nCENAPI 2018 anualizado.",
        x = "", y = "", color = "Fuente") +
   tema +
   theme(legend.position = "top",
         axis.text.x = element_text(size = 8, face = "bold"))

 ggsave(files$plot3_png, width = 16, height = 10)
 ggsave(files$plot3_svg, width = 16, height = 10)
 
# done. 