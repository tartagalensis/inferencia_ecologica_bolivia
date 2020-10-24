#### ANALISIS RESULTADOS INFERENCIA ECOLOGICA
#### Author: Franco Galeano
#### Init: 25 Oct 2020

########################################################
### El objetivo de este script es realizar tablas y 
###un diagrama de sankey con los resultados del script s05
#########################################################

library(ggalluvial) #paquete para diagramas de sankey
library(ggrepel)
library(janitor)
library(magick)
library(tidyverse)

# Tabla en numeros enteros para el documento
tabla_enteros <- read_csv("data/inferenciaEcoResultados.csv") %>% 
  rename("2019 / 2020" = "indice",
         "Camacho (CREEMOS)" = "creemos_2020",
         "Arce (MAS-IPSP)" = "mas_2020",
         "Chi (FPV)" = "fpv_2020",
         "Mamani (PAN)" = "pan_2020",
         "Mesa (CC)" = "cc_2020",
         "No Voto / Blanco" = "NoVoto_Blanco") %>% 
  adorn_totals("row") %>%
  mutate(Total = rowSums(.[2:7])) %>% 
  print()


## Tabla en porcentajes para el documento  
tabla_pcts <- read_csv("data/inferenciaEcoResultados.csv") %>%
  rename("2019 / 2020" = "indice",
         "Camacho (CREEMOS)" = "creemos_2020",
         "Arce (MAS-IPSP)" = "mas_2020",
         "Chi (FPV)" = "fpv_2020",
         "Mamani (PAN)" = "pan_2020",
         "Mesa (CC)" = "cc_2020",
         "No Voto / Blanco" = "NoVoto_Blanco") %>% 
  adorn_totals("row") %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting() %>%
  #adorn_ns() %>% 
  print()


######################
### DIAGRAMA DE SANKEY
######################
paso <- tibble(paso = c("Mesa (CC)", "Rodriguez (FPV)",
                        "Patsi (MTS)", "Cárdenas (UCS)",
                        "Morales (MAS-IPSP)","Ortiz (BDN)",
                        "Chi (PDC)", "Trigo (MNR)", "Juchani (PAN)", "No votó/Blanco"))

base_sankey <- read_csv("data/inferenciaEcoResultados.csv") %>% 
  select("Camacho (CREEMOS)" = creemos_2020,
         "Arce (MAS-IPSP)" = mas_2020,
         "Chi (FPV)" = fpv_2020,
         "Mamani (PAN)" = pan_2020,
         "Mesa (CC)" = cc_2020,
         "No Voto / Blanco" = NoVoto_Blanco) %>% 
  bind_cols(paso) %>% 
  select(paso, everything()) %>% 
  gather(key="generales", value = "freq", -paso) %>%
  arrange(desc(paso))%>% 
  print()

plot_sankey <-base_sankey %>% 
  ggplot(aes(axis1 = paso, axis2 = generales,
             y = freq)) +
  scale_x_discrete(limits = c("2019", "2020"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = freq)) +
  geom_stratum() +
  geom_text_repel(stat = "stratum",infer.label = TRUE,size = 3.5) +
  theme_minimal() +
  labs(title = "Transferencia de votos - Elecciones presidenciales Bolivia (2020)",
       caption= "@Tartagalensis") +
  hrbrthemes::theme_ipsum_rc(base_size = 14, plot_title_size = 18, subtitle_size = 14,
                             strip_text_size = 14, axis_title_size = 0, base_family = "Encode Sans Wide" ) +
  theme(plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank())



## SAVE
ggsave(plot = plot_sankey, filename = "sankey_bolivia.png",
       width = 10, height = 7, units = "in")

## TRIM
image_read("sankey_bolivia.png") %>%
  image_trim() %>% 
  image_write("sankey_bolivia_resize.png")

