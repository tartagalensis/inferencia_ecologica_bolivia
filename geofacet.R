#### GEOFACET
#### Author: Franco Galeano
#### Init: 25 Oct 2020


library(tidyverse)
library(geofacet)
library(magick) 

## READ DATA ####
bolivia_100 <- read_csv("data/recuento-definitivo-2020-100.csv") %>% 
  filter(PAIS == "Bolivia") %>%
  filter(CANDIDATURA == "PRESIDENTE") %>% 
  select(DEPARTAMENTO, INSCRITOS_HABILITADOS, VOTO_EMITIDO, CREEMOS,MAS_IPSP, CC) %>% 
  group_by(DEPARTAMENTO) %>% 
  summarise_if(is.numeric, sum) %>% 
  ungroup() %>% 
  mutate(DEPARTAMENTO = (str_replace(DEPARTAMENTO, "Potos\xed",  "Potosí")),
         creemos = round(CREEMOS/VOTO_EMITIDO*100,2),
         mas = round(MAS_IPSP/VOTO_EMITIDO*100,2),
         cc = round(CC/VOTO_EMITIDO*100,2)) %>% 
  select(DEPARTAMENTO, creemos, mas, cc) %>% 
  gather("lista", "votos", creemos:cc) %>% 
  print()


# MAPA GRILLA DEPTOS BOLIVIA

bolivia_grid <- data.frame(
  col = c(1,1,2,3,1,2,3,4,3),
  row = c(1,2,2,2,3,3,3,3,4),
  code = c("BO-PAN","BO-PAZ","BO-CBB","BO-BEN","BO-ORU","BO-POT","BO-CHU","BO-SCZ","BO-TRJ"),
  name_es = c("Pando", "La Paz","Cochabamba","Beni","Oruro","Potosí", "Chuquisaca","Santa Cruz", "Tarija"),
  stringsAsFactors = FALSE
)


grid_preview(bolivia_grid)

## COLORES DE CANDIDATOS + RELEVANTES

cols_fza_pol <- c("creemos" =  "magenta",
                  "mas" = "darkblue",
                  "cc" = "orange")



## GEOFACET DE LAS PASO

bolivia_geofacet <- bolivia_100 %>% 
  ggplot() +
  theme_minimal() +
  geom_col(aes(lista, votos, fill = lista)) +
  scale_fill_manual(values = cols_fza_pol) +
  facet_geo(~DEPARTAMENTO, grid = bolivia_grid, label = "name") +
  coord_flip() +
  labs(title = "Resultados por departamento - Carga al 100%",
       subtitle = "Elecciones Generales - Bolivia 2020",
       caption= "@Tartagalensis") +
  hrbrthemes::theme_ipsum_rc(base_size = 14, plot_title_size = 18, subtitle_size = 14,
                             strip_text_size = 14, axis_title_size = 0, base_family = "Encode Sans Wide" ) +
  theme(axis.text.x = element_text(size = 14 * 0.8, hjust = 0, colour = "grey50"),
        legend.position = "bottom", legend.title = element_text(size = 15), legend.text = element_text(size = 10),
        plot.caption = element_text(size = 15))


bolivia_geofacet

## SAVE
ggsave(plot = bolivia_geofacet, filename = "bolivia_geofacet.png",
       width = 10, height = 7, units = "in")

## TRIM
image_read("bolivia_geofacet.png") %>%
  image_trim() %>% 
  image_write("bolivia_geofacet_resize.png")
