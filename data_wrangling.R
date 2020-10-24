## Data Wrangling - Inferencia Ecológica Bolivia
#### Author: Franco Galeano
#### Init: 25 Oct 2020

## Importamos libs y paquetes
library(janitor)
library(readxl)
library(tidyverse)


### IMPORTAR DATA
###########################################################################
## ESCRUTINIO DEFINITIVO 2020 DISPONIBLE EN: https://computo.oep.org.bo/
###########################################################################

##2019
eleccion_2019 <- read_excel("data/recuento-definitivo-2019-100.xlsx") %>%
  clean_names() %>%
  filter(eleccion == "Presidente y Vicepresidente") %>%
  filter(pais == "Bolivia") %>% 
  filter(inscritos > 100) %>% 
  mutate(votos_positivos_2019 = mas_ipsp + cc + fpv + mts + ucs + x21f + pdc + mnr + pan_bol,
                         votos_2019 = votos_positivos_2019 + blancos) %>% 
  select(numero_mesa, departamento, provincia, municipio, localidad, recinto, inscritos_2019 = inscritos,
         cc_2019 = cc, fpv_2019 = fpv, mts_2019 = mts, ucs_2019 = ucs,
         mas_2019 = mas_ipsp, x21f_2019 = x21f, pdc_2019 = pdc, mnr_2019 = mnr,
         pan_2019 = pan_bol, blancos_2019 = blancos,
         votos_positivos_2019,votos_2019) %>% 
  print()
  
# 2020
eleccion_2020 <- read_csv("data/recuento-definitivo-2020-100.csv") %>% 
  filter(CANDIDATURA == "PRESIDENTE") %>% 
  filter(PAIS == "Bolivia") %>% 
  filter(INSCRITOS_HABILITADOS > 100) %>% 
  print()
