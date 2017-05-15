################################# PROCESAMIENTO DE DATOS FRANCESES ######################################
################################ UNIFICACIÓN DE DATOS 2007/2009/2012 ####################################
###################################### FAZH ITAM 2017 130435 ############################################
#########################################################################################################

### PREÁMBULO ####
library(tidyverse)
library(magrittr)


### LEEMOS LOS DATOS ####
load("DATOS_PROCESADOS/PRESIDENCIALES_2007.RData")
load("DATOS_PROCESADOS/LEGISLATIVAS_2007.RData")
load("DATOS_PROCESADOS/EUROPEAS_2009.RData")
load("DATOS_PROCESADOS/PRESIDENCIALES_2012.RData")
load("DATOS_PROCESADOS/LEGISLATIVAS_2012.RData")

### BASE GENERAL DE CÓDIGOS CODGEO QUE VAN CAMBIANDO ####

BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = character(), 
                                  CODGEO_2009 = character(), 
                                  CODGEO_2012 = character())

### NOMBRES DE VARIABLES DE BASES DE COMUNAS ####

variables_base_comunas <- names(PRESIDENCIALES_2007$BASE_COMUNAS)

### VERIFICAMOS QUE BASES DEL 2007 ESTÉN HOMOLOGADAS ####

# No deben haber problemas dentro del mismo año, por el procesamiento anterior
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS, LEGISLATIVAS_2007$BASE_COMUNAS, by = variables_base_comunas) 
anti_join(LEGISLATIVAS_2007$BASE_COMUNAS, PRESIDENCIALES_2007$BASE_COMUNAS, by = variables_base_comunas)

### VERIFICAMOS QUE BASES DEL 2012 ESTÉN HOMOLOGADAS ####

anti_join(PRESIDENCIALES_2012$BASE_COMUNAS, LEGISLATIVAS_2012$BASE_COMUNAS, by = variables_base_comunas) 
anti_join(LEGISLATIVAS_2012$BASE_COMUNAS, PRESIDENCIALES_2012$BASE_COMUNAS, by = variables_base_comunas)


### PROBLEMAS DE 2007 A 2009 #### 

# Vemos si hay problemas de 2007 a 2009
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS, EUROPEAS_2009$BASE_COMUNAS, by = variables_base_comunas)

# Vemos si hay problemas de comunas que desaparecieron
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS,EUROPEAS_2009$BASE_COMUNAS,by="CODGEO") %T>% 
{nrow(.) %>% print} %>%
  left_join(EUROPEAS_2009$BASE_COMUNAS,by=variables_base_comunas[-1]) %T>%
  {nrow(.) %>% print}

# Fort-Mardyck se une a Dunkerque
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "59248", 
                                  CODGEO_2009 = "59183",
                                  CODGEO_2012 = "59183") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Saint-Pol-sur-Mer se une a Dunkerque
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "59540", 
                                  CODGEO_2009 = "59183",
                                  CODGEO_2012 = "59183") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Vemos si hay problemas de nombres de comunas
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS,EUROPEAS_2009$BASE_COMUNAS,by="NOM_COMUNA") %T>% 
  {nrow(.) %>% print} %>%
  left_join(EUROPEAS_2009$BASE_COMUNAS,by=variables_base_comunas[-2]) %T>%
  {nrow(.) %>% print}

# Por regla general tomamos como nombre válido el más reciente. 

PRESIDENCIALES_2007$BASE_COMUNAS %<>% 
  left_join(EUROPEAS_2009$BASE_COMUNAS,by=variables_base_comunas[-2]) %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA=NOM_COMUNA.x)

LEGISLATIVAS_2007$BASE_COMUNAS %<>% 
  left_join(EUROPEAS_2009$BASE_COMUNAS,by=variables_base_comunas[-2]) %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA=NOM_COMUNA.x)

# Vemos si hay más problemas de 2007 a 2009
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS, EUROPEAS_2009$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2007"))

anti_join(LEGISLATIVAS_2007$BASE_COMUNAS, EUROPEAS_2009$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2007"))

### PROBLEMAS DE 2009 A 2007 #### 

# Vemos si hay problemas de 2009 a 2007
anti_join(EUROPEAS_2009$BASE_COMUNAS, PRESIDENCIALES_2007$BASE_COMUNAS, by = variables_base_comunas)

# Vemos si hay problemas de comunas que surgieron
anti_join(EUROPEAS_2009$BASE_COMUNAS, PRESIDENCIALES_2007$BASE_COMUNAS,by="CODGEO") %T>% 
{nrow(.) %>% print} %>%
  left_join(PRESIDENCIALES_2007$BASE_COMUNAS,by=variables_base_comunas[-1]) %T>%
  {nrow(.) %>% print}

# Lieoux se separa de Saint-Gaudens
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "31483", 
                                  CODGEO_2009 = "31300",
                                  CODGEO_2012 = "31300") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Saint-Symphorien se separa de Hédé
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "35130", 
                                  CODGEO_2009 = "35317",
                                  CODGEO_2012 = "35317") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Verquigneul se separa de Béthune
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "62119", 
                                  CODGEO_2009 = "62847",
                                  CODGEO_2012 = "62847") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Rosoy se separa de Sens
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "89387", 
                                  CODGEO_2009 = "89326",
                                  CODGEO_2012 = "89326") %>% 
  full_join(BASE_CAMBIOS_CODGEO)


# Vemos si hay más problemas de 2009 a 2007
anti_join(EUROPEAS_2009$BASE_COMUNAS, PRESIDENCIALES_2007$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2009"))


### PROBLEMAS DE 2009 A 2012 #### 

# Vemos si hay problemas de 2009 a 2012
anti_join(EUROPEAS_2009$BASE_COMUNAS, PRESIDENCIALES_2012$BASE_COMUNAS, by = variables_base_comunas)

# Vemos si hay problemas de comunas que desaparecieron
anti_join(EUROPEAS_2009$BASE_COMUNAS,PRESIDENCIALES_2012$BASE_COMUNAS,by="CODGEO") %T>% 
{nrow(.) %>% print} %>%
  left_join(PRESIDENCIALES_2012$BASE_COMUNAS,by=variables_base_comunas[-1]) %T>%
  {nrow(.) %>% print}

# Hubo agrupaciones de comunas y que resultaron en varias comunas. Revisar los Motivos de Errores en las bases de 2012.

# Se agruparon comunas en Dévoluy
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = c("05002","05042","05138"), 
                                  CODGEO_2009 = c("05002","05042","05138"),
                                  CODGEO_2012 = "05139") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Saint-Bonnet-en-Champsaur
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = c("05020","05067"), 
                                  CODGEO_2009 = c("05020","05067"),
                                  CODGEO_2012 = "05132") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Baugé-en-Anjou
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "49199", 
                                  CODGEO_2009 = "49199",
                                  CODGEO_2012 = "49092") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Chemillé-Melay
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = c("49213","49245","49303","49372"), 
                                  CODGEO_2009 = c("49213","49245","49303","49372"),
                                  CODGEO_2012 = "49018") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Clefs-Val d'Anjou
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "49380", 
                                  CODGEO_2009 = "49380",
                                  CODGEO_2012 = "49101") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Épizon
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "52379", 
                                  CODGEO_2009 = "52379",
                                  CODGEO_2012 = "52187") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Thizy-les-Bourgs
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = c("69025","69041","69128","69129"), 
                                  CODGEO_2009 = c("69025","69041","69128","69129"),
                                  CODGEO_2012 = "69248") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Saint-Germain-Nuelles
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "69144", 
                                  CODGEO_2009 = "69144",
                                  CODGEO_2012 = "69208") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Beaussais-Vitré
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "79353", 
                                  CODGEO_2009 = "79353",
                                  CODGEO_2012 = "79030") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Voulmentin
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "79356", 
                                  CODGEO_2009 = "79356",
                                  CODGEO_2012 = "79242") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Se agruparon comunas en Fontenoy-le-Château
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "88282", 
                                  CODGEO_2009 = "88282",
                                  CODGEO_2012 = "88176") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Vemos si hay problemas de nombres de comunas
anti_join(EUROPEAS_2009$BASE_COMUNAS,PRESIDENCIALES_2012$BASE_COMUNAS,by="NOM_COMUNA") %T>% 
{nrow(.) %>% print} %>%
  left_join(PRESIDENCIALES_2012$BASE_COMUNAS,by=variables_base_comunas[-2]) %T>%
  {nrow(.) %>% print}

# Por regla general tomamos como nombre válido el más reciente. 
EUROPEAS_2009$BASE_COMUNAS %<>% 
  left_join(PRESIDENCIALES_2012$BASE_COMUNAS,by=variables_base_comunas[-2]) %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA=NOM_COMUNA.x)

# Vemos si hay más problemas de 2009 a 2012
anti_join(EUROPEAS_2009$BASE_COMUNAS,PRESIDENCIALES_2012$BASE_COMUNAS,  by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2009"))

# Bihorel se une a Bois-Guillaume-Bihorel
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "76095", 
                                  CODGEO_2009 = "76095",
                                  CODGEO_2012 = "76108") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Bleury se une a Bleury-Saint-Symphorien
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "28042", 
                                  CODGEO_2009 = "28042",
                                  CODGEO_2012 = "28361") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Vemos si hay más problemas de 2009 a 2012
anti_join(EUROPEAS_2009$BASE_COMUNAS,PRESIDENCIALES_2012$BASE_COMUNAS,  by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2009"))

### PROBLEMAS DE 2012 A 2009 #### 

# Vemos si hay problemas de 2012 a 2009
anti_join(PRESIDENCIALES_2012$BASE_COMUNAS, EUROPEAS_2009$BASE_COMUNAS, by = variables_base_comunas)

# Vemos si hay problemas de comunas que surgieron
anti_join(PRESIDENCIALES_2012$BASE_COMUNAS,EUROPEAS_2009$BASE_COMUNAS, by="CODGEO") %T>% 
{nrow(.) %>% print} %>%
  left_join(EUROPEAS_2009$BASE_COMUNAS, by=variables_base_comunas[-1]) %T>%
  {nrow(.) %>% print}

# Avrecourt y Saulxures se separan de Val-de-Meuse
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "52332", 
                                  CODGEO_2009 = "52332",
                                  CODGEO_2012 = c("52033","52465")) %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Chézeaux se separa de Varennes-sur-Amance
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "52504", 
                                  CODGEO_2009 = "52504",
                                  CODGEO_2012 = "52124") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Laneuville-à-Rémy se separa de Robert-Magny
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "52427", 
                                  CODGEO_2009 = "52427",
                                  CODGEO_2012 = "52266") %>% 
  full_join(BASE_CAMBIOS_CODGEO)

# Lavilleneuve-au-Roi se separa de Autreville-sur-la-Renne
BASE_CAMBIOS_CODGEO <- data_frame(CODGEO_2007 = "52031", 
                                  CODGEO_2009 = "52031",
                                  CODGEO_2012 = "52278") %>% 
  full_join(BASE_CAMBIOS_CODGEO)


# Vemos si hay más problemas de 2012 a 2009
anti_join(PRESIDENCIALES_2012$BASE_COMUNAS, EUROPEAS_2009$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2012"))

### PROBLEMAS DE 2007 A 2012 #### 

# Vemos si hay problemas de 2007 a 2012 y que no se hayan ya considerado de 2009 a 2012
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS, PRESIDENCIALES_2012$BASE_COMUNAS, by = variables_base_comunas)  %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2007"))

# Vemos si hay problemas de comunas que desaparecieron y que no se hayan ya considerado de 2009 a 2012
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS,PRESIDENCIALES_2012$BASE_COMUNAS,by="CODGEO") %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2007"))

# Vemos si hay problemas de nombres de comunas
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS,PRESIDENCIALES_2012$BASE_COMUNAS,by="NOM_COMUNA") %T>% 
{nrow(.) %>% print} %>%
  left_join(PRESIDENCIALES_2012$BASE_COMUNAS,by=variables_base_comunas[-2]) %T>%
  {nrow(.) %>% print}

# Por regla general tomamos como nombre válido el más reciente. 

PRESIDENCIALES_2007$BASE_COMUNAS %<>% 
  left_join(PRESIDENCIALES_2012$BASE_COMUNAS,by=variables_base_comunas[-2]) %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA=NOM_COMUNA.x)

LEGISLATIVAS_2007$BASE_COMUNAS %<>% 
  left_join(PRESIDENCIALES_2012$BASE_COMUNAS,by=variables_base_comunas[-2]) %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA=NOM_COMUNA.x)

# Vemos si hay más problemas de 2007 a 2012
anti_join(PRESIDENCIALES_2007$BASE_COMUNAS, PRESIDENCIALES_2012$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2007"))

anti_join(LEGISLATIVAS_2007$BASE_COMUNAS, PRESIDENCIALES_2012$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2007"))

### PROBLEMAS DE 2012 A 2007 #### 

# Vemos si hay problemas de 2012 a 2007
anti_join(PRESIDENCIALES_2012$BASE_COMUNAS, PRESIDENCIALES_2007$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by = c("CODGEO"="CODGEO_2012"))

# Vemos si hay problemas de comunas que surgieron y que no se hayan considerado de 2012 a 2009
anti_join(PRESIDENCIALES_2012$BASE_COMUNAS,PRESIDENCIALES_2007$BASE_COMUNAS, by="CODGEO") %>%
  anti_join(BASE_CAMBIOS_CODGEO,by = c("CODGEO"="CODGEO_2012"))

# Vemos si hay problemas de nombres de comunas
anti_join(PRESIDENCIALES_2012$BASE_COMUNAS,PRESIDENCIALES_2007$BASE_COMUNAS,by="NOM_COMUNA") %>%
  anti_join(BASE_CAMBIOS_CODGEO,by = c("CODGEO"="CODGEO_2012"))

# Vemos si hay más problemas de 2012 a 2007
anti_join(PRESIDENCIALES_2012$BASE_COMUNAS, PRESIDENCIALES_2007$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2012"))

anti_join(PRESIDENCIALES_2012$BASE_COMUNAS, LEGISLATIVAS_2007$BASE_COMUNAS, by = variables_base_comunas) %>%
  anti_join(BASE_CAMBIOS_CODGEO,by=c("CODGEO"="CODGEO_2012"))

### GUARDAMOS DATOS PROCESADOS ####

save(BASE_CAMBIOS_CODGEO,file = "DATOS_UNIFICADOS/BASE_CAMBIOS_CODGEO.RData")
save(PRESIDENCIALES_2007,file = "DATOS_UNIFICADOS/PRESIDENCIALES_2007.RData")
save(LEGISLATIVAS_2007,file = "DATOS_UNIFICADOS/LEGISLATIVAS_2007.RData")
save(EUROPEAS_2009,file = "DATOS_UNIFICADOS/EUROPEAS_2009.RData")
save(PRESIDENCIALES_2012,file = "DATOS_UNIFICADOS/PRESIDENCIALES_2012.RData")
save(LEGISLATIVAS_2012,file = "DATOS_UNIFICADOS/LEGISLATIVAS_2012.RData")
