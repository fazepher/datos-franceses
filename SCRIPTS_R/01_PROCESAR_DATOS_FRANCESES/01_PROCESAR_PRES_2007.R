################################# PROCESAMIENTO DE DATOS FRANCESES ######################################
################################## ELECCIONES PRESIDENCIALES 2007 #######################################
###################################### FAZH ITAM 2017 130435 ############################################
#########################################################################################################

### PREÁMBULO ####
library(tidyverse)
library(magrittr)
library(stringr)

### DATOS DE VOTACIÓN POR CASILLA ####

pres_07 <- read_csv2(file = "DATOS_BRUTOS/PR07_Bvot_T1T2_FAZH.txt", 
                     col_names = c("VUELTA",
                                   "COD_DEPARTAMENTO","COD_COMUNA","NOM_COMUNA",
                                   "CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                   "NUM_CANDIDATO","APELLIDO_CANDIDATO","NOMBRE_CANDIDATO","ETIQUETA",
                                   "VOT_CANDIDATO"),
                     locale = locale(encoding = "latin1")) %>%
  # Filtramos a las comunas de Francia continental y Córsega
  filter(!(substr(COD_DEPARTAMENTO,1,1) == "Z")) %>% 
  # Filtramos a los resultados de la primera vuelta
  filter(VUELTA == 1) %>%
  # Modificamos los Nombres de las comunas cuando son Arrondisements de París, Lyon y Marsella
  mutate(NOM_COMUNA = if_else(NOM_COMUNA == "Paris",paste("Paris",substr(CASILLA,1,2), sep = " "),
                              if_else(NOM_COMUNA == "Lyon", paste("Lyon",substr(CASILLA,2,2), sep = " "),
                                      if_else(NOM_COMUNA == "Marseille",paste("Marseille",substr(CASILLA,1,2), sep = " "),
                                              NOM_COMUNA)))) %>%
  # Agregamos el CODGEO del INSEE para las comunas (ajustando para los Arrondisements de París, Lyon y Marsella)
  mutate(CODGEO = paste(COD_DEPARTAMENTO,COD_COMUNA,sep="")) %>%
  mutate(CODGEO = if_else(CODGEO == "75056",paste("751",substr(CASILLA,1,2),sep=""),
                          if_else(CODGEO == "69123", paste("6938",substr(CASILLA,2,2),sep=""),
                                  if_else(CODGEO == "13055",paste("132",substr(CASILLA,1,2),sep=""),
                                                     CODGEO)))) %>%
  # Modificamos el código de casillas para hacerlo único
  mutate(CASILLA = paste(CODGEO,CASILLA,sep="_"))
  

# Agregamos los Departamentos
# https://www.insee.fr/fr/information/2560452
pres_07 <- read_delim(file = "DATOS_ADMINISTRATIVOS/depts2007.txt",
                         delim = "\t",locale = locale(encoding = "latin1")) %>%
  transmute(COD_REGION = REGION,COD_DEPARTAMENTO = DEP, NOM_DEPARTAMENTO = NCCENR) %>%
  left_join(pres_07,., by = "COD_DEPARTAMENTO")

# Agregamos las Regiones Antiguas
pres_07 <- read_delim(file = "DATOS_ADMINISTRATIVOS/reg2007.txt",
                         delim = "\t",locale = locale(encoding = "latin1")) %>%
  transmute(COD_REGION = REGION, NOM_REGION = NCCENR) %>%
  left_join(pres_07,., by = "COD_REGION")

# Agregamos los nombres de las Regiones Nuevas
pres_07 <- read_delim(file = "DATOS_ADMINISTRATIVOS/AUXILIAR_REG_DEPART.csv",
                         delim = ",",locale = locale(encoding = "latin1")) %>%
  select(-NOM_REGION) %>%
  left_join(pres_07,., by = "COD_REGION")

# Se ordenan las columnas, conservando solo las que nos importan
pres_07 <- pres_07 %>% 
  select(CODGEO,
         NOM_COMUNA,NOM_DEPARTAMENTO,NOM_REGION,NOM_NVA_REG,
         COD_COMUNA,COD_DEPARTAMENTO,COD_REGION,COD_NVA_REG,
         CASILLA,INSCRITOS,VOTANTES,VOT_EF,
         ETIQUETA, APELLIDO_CANDIDATO, VOT_CANDIDATO)

### BASE DEL CENSO ####
IMG1_07 <- read_csv2(file = "DATOS_BRUTOS/BTT_TD_IMG1_2007.txt", 
                     col_names = c("NIVEL","CODGEO","C_SEXO","C_EDAD4","C_TIPO_ACT","C_MIGRATORIA","VALOR"),
                     col_types = "ccccccd",
                     locale = locale(decimal_mark = ","),
                     skip = 1) %>% 
  # Atención con los códigos
  mutate(C_SEXO = factor(C_SEXO,labels = c("HOMBRES","MUJERES")),
         C_EDAD4 = factor(C_EDAD4),
         C_TIPO_ACT = factor(C_TIPO_ACT,labels = c("EMPLEADOS","DESEMPLEADOS","RETIRADOS","ESTUDIANTES","HOGAR","OTROS")),
         C_MIGRATORIA = factor(C_MIGRATORIA, labels = c("INMIGRANTES","LOCALES"))) %>%
  mutate(CAT = paste(C_SEXO,C_TIPO_ACT,C_MIGRATORIA,C_EDAD4,sep="_")) %>%
  select(CODGEO,NIVEL,CAT,VALOR) %>% 
  spread(CAT,VALOR)

### ERRORES DE  COMUNAS ####

# Para homologar resultados electorales y censo necesitamos las geografías administrativas oficiales para cada base
COMUNAS_07 <- read_delim(file = "DATOS_ADMINISTRATIVOS/comsimp2007.txt", 
                         delim = "\t",locale = locale(encoding = "latin1")) %>% 
  transmute(CODGEO = paste(DEP,COM,sep=""), NOM_COMUNA = NCCENR, ART = ARTMIN) %>% 
  # Añadimos el artículo al nombre
  mutate(NOM_COMUNA = if_else(is.na(ART),
                              NOM_COMUNA,
                              paste(substring(ART,2,nchar(ART)-1),NOM_COMUNA,sep=" "))) %>% 
  transmute(CODGEO,
            NOM_COMUNA = if_else(substring(NOM_COMUNA,2,3)=="' ",
                                 paste(substring(NOM_COMUNA,1,2),substring(NOM_COMUNA,4),sep=""),
                                 NOM_COMUNA)) %>% 
  # Corregimos las ligaduras OE y oe
  mutate(NOM_COMUNA=str_replace_all(NOM_COMUNA,"\u008c","OE")) %>% 
  mutate(NOM_COMUNA=str_replace_all(NOM_COMUNA,"\u009c","oe"))
  
COMUNAS_09 <- read_delim(file = "DATOS_ADMINISTRATIVOS/comsimp2009.txt", 
                         delim = "\t",locale = locale(encoding = "latin1")) %>% 
  transmute(CODGEO = paste(DEP,COM,sep=""), NOM_COMUNA = NCCENR, ART = ARTMIN) %>% 
  # Añadimos el artículo al nombre
  mutate(NOM_COMUNA = if_else(is.na(ART),
                              NOM_COMUNA,
                              paste(substring(ART,2,nchar(ART)-1),NOM_COMUNA,sep=" "))) %>% 
  transmute(CODGEO,
            NOM_COMUNA = if_else(substring(NOM_COMUNA,2,3)=="' ",
                                 paste(substring(NOM_COMUNA,1,2),substring(NOM_COMUNA,4),sep=""),
                                 NOM_COMUNA)) %>% 
  # Corregimos las ligaduras OE y oe
  mutate(NOM_COMUNA=str_replace_all(NOM_COMUNA,"\u008c","OE")) %>% 
  mutate(NOM_COMUNA=str_replace_all(NOM_COMUNA,"\u009c","oe"))


# Utilizamos los nombres oficiales del 2007
pres_07 <- pres_07 %>%
  left_join(COMUNAS_07,by="CODGEO") %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA = NOM_COMUNA.x)

# Comunas que están en los resultados electorales, pero no en el censo y que sí existían en 2007
ERRORES_COMUNAS <- anti_join(pres_07,IMG1_07,by="CODGEO") %>% 
  semi_join(COMUNAS_07,by="CODGEO") %>% 
  select(CODGEO,NOM_COMUNA,NOM_DEPARTAMENTO) %>% 
  distinct %>% 
  cbind(MOTIVO_ERROR = NA)
# Errores por que se agregaron a otra comuna 
ERRORES_COMUNAS$MOTIVO_ERROR <- c("01/01/2009 : Saint-Germain-Source-Seine est rattachée à Blessey (fusion simple) qui devient Source-Seine.",
                                  "29/06/2007 : Guitalens est rattachée à Lalbarède (fusion simple) qui devient Guitalens-L'Albarède.")
# Corregimos el error en los resultados electorales con el nombre existente en 2009. 
# Saint-Germain-Source-Seine (21551) y Blessey (21084) => Source-Seine (21084)
pres_07 <- pres_07 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("21084","21551"),"21084",CODGEO),
         COD_COMUNA = if_else(CODGEO %in% c("21084","21551"),"084",COD_COMUNA),
         NOM_COMUNA = if_else(CODGEO %in% c("21084","21551"),"Source-Seine",NOM_COMUNA))
# Guitalens (81107) y Lalbarède (81132) => Guitalens-L'Albarède (81132)
pres_07 <- pres_07 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("81132","81107"),"81132",CODGEO),
         COD_COMUNA = if_else(CODGEO %in% c("81132","81107"),"132",COD_COMUNA),
         NOM_COMUNA = if_else(CODGEO %in% c("81132","81107"),"Guitalens-L'Albarède",NOM_COMUNA))

# Comunas que están en los resultados electorales, pero no en el censo y que sí existían en 2009
ERRORES_COMUNAS <- anti_join(pres_07,IMG1_07,by="CODGEO") %>% 
  semi_join(COMUNAS_09,by="CODGEO") %>% 
  select(CODGEO,NOM_COMUNA,NOM_DEPARTAMENTO) %>% 
  distinct %>% 
  full_join(ERRORES_COMUNAS)

# Comunas que están en los resultados electorales, pero no en el censo y que NO existían NI en 2007 NI en 2009
ERRORES_COMUNAS <- anti_join(pres_07,IMG1_07,by="CODGEO") %>% 
  select(CODGEO,NOM_COMUNA,NOM_DEPARTAMENTO) %>% 
  distinct %>% 
  full_join(ERRORES_COMUNAS)
# Errores de captura 
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  mutate(MOTIVO_ERROR = if_else(is.na(MOTIVO_ERROR),"CODGEO inválido",MOTIVO_ERROR))
# Corregimos el error el CODGEO en los resultados electorales con el correcto. 
# La Répara-Auriples (26020)
pres_07 <- pres_07 %>% 
  mutate(CODGEO = if_else(NOM_COMUNA == "La Répara-Auriples","26020",CODGEO),
         COD_COMUNA = if_else(NOM_COMUNA == "La Répara-Auriples","020",COD_COMUNA))
# Bagnoles-de-l'Orne (61483)
pres_07 <- pres_07 %>% 
  mutate(CODGEO = if_else(NOM_COMUNA == "Bagnoles-de-l'Orne","61483",CODGEO),
         COD_COMUNA = if_else(NOM_COMUNA == "Bagnoles-de-l'Orne","483",COD_COMUNA))
# Ban-sur-Meurthe-Clefcy (88106)
pres_07 <- pres_07 %>% 
  mutate(CODGEO = if_else(NOM_COMUNA == "Ban-sur-Meurthe-Clefcy","88106",CODGEO),
         COD_COMUNA = if_else(NOM_COMUNA == "Ban-sur-Meurthe-Clefcy","106",COD_COMUNA))

# Comunas que están en el censo, pero no en los resultados electorales y que sí existían en 2007
ERRORES_COMUNAS <- anti_join(IMG1_07,pres_07,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  semi_join(COMUNAS_07,by="CODGEO") %>% 
  full_join(ERRORES_COMUNAS)
# Errores por ser comunas de las grandes ciudades
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  mutate(MOTIVO_ERROR = if_else(CODGEO == "13055","Marseille",
                                if_else(CODGEO == "69123","Lyon",
                                        if_else(CODGEO == "75056","Paris",MOTIVO_ERROR))))
# Errores por ser comunas de alta mar
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  mutate(MOTIVO_ERROR = if_else(substr(CODGEO,1,2) == "97","Alta Mar",MOTIVO_ERROR))
# Se eliminan estas comunas del Censo
IMG1_07 <- ERRORES_COMUNAS %>% 
  filter(MOTIVO_ERROR %in% c("Marseille","Lyon","Paris","Alta Mar")) %>%
  extract2("CODGEO") %>%
  {filter(IMG1_07,!{CODGEO %in% .})}


# Comunas que están en el censo, pero no en los resultados electorales y que sí existían en 2009
ERRORES_COMUNAS <- anti_join(IMG1_07,pres_07,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  semi_join(COMUNAS_09, by="CODGEO") %>% 
  full_join(ERRORES_COMUNAS)

# Errores por que se agregaron a otra comuna 
ERRORES_COMUNAS <- ERRORES_COMUNAS %>%  
  mutate(MOTIVO_ERROR = if_else(CODGEO == "31300","13/02/2008 : Lieoux est rétablie. 
                                01/01/1974 : Lieoux est rattachée à Saint-Gaudens (fusion association).",
                                if_else(CODGEO == "35317", "01/01/2008 : Saint-Symphorien est rétablie. 
                                        01/07/1973 : Saint-Symphorien est rattachée à Hédé (fusion association).",
                                        if_else(CODGEO == "62847","01/01/2008 : Verquigneul est rétablie.
                                                10/12/1990 : Verquigneul est rattachée à Béthune (fusion association).",
                                                if_else(CODGEO == "89326","12/02/2008 : Rosoy est rétablie. 
                                                        01/01/1973 : Rosoy est rattachée à Sens (fusion association).",
                                                        MOTIVO_ERROR)))))


# Corregimos el error agregando los datos en el censo con el nombre existente en 2007. 

# Lieoux (31300) => Saint-Gaudens (31483)
IMG1_07 <- IMG1_07 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("31483","31300"),"31483",CODGEO)) 

# Saint-Symphorien (35317) => Hédé (35130)
IMG1_07 <- IMG1_07 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("35130","35317"),"35130",CODGEO)) 

# Verquigneul (62847) => Béthune (62119)
IMG1_07 <- IMG1_07 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("62119","62847"),"62119",CODGEO)) 

# Rosoy (89326) => Sens (89387)
IMG1_07 <- IMG1_07 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("89387","89326"),"89387",CODGEO))

# Agregamos los datos 
IMG1_07 <- IMG1_07 %>%
  group_by(NIVEL,CODGEO) %>%
  summarise_if(is.numeric,funs(sum(.,na.rm=TRUE))) %>%
  ungroup

# Comunas que están en el censo, pero no en los resultados electorales y que NO existían NI en 2007 NI en 2009
ERRORES_COMUNAS <- anti_join(IMG1_07,pres_07,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  full_join(ERRORES_COMUNAS)

# Conservamos solo las columnas de CODGEO y el motivo del error
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  select(CODGEO,MOTIVO_ERROR)

### BASE ADMINISTRATIVA DE COMUNAS ####

BASE_COMUNAS <- pres_07 %>%
  select(CODGEO:COD_NVA_REG) %>%
  distinct %>% 
  left_join(select(IMG1_07,CODGEO,NIVEL),by="CODGEO")

### RESULTADOS ELECTORALES POR COMUNA ####

VUELTA_1_PRES_07 <- pres_07 %>%
  select(CODGEO,INSCRITOS:VOT_CANDIDATO) %>%
  group_by(CODGEO,ETIQUETA,APELLIDO_CANDIDATO) %>%
  summarise_if(is.numeric,funs(sum(.))) %>%
  ungroup %>% 
  left_join(read_csv("BASES_CANDIDATOS/FAMILIAS_POLITICAS_07.csv",
                     locale = locale(encoding = "latin1")),by="APELLIDO_CANDIDATO") %>% 
  transmute(CODGEO,
            INSCRITOS,VOTANTES,PCT_PART = VOTANTES/INSCRITOS,VOT_EF,
            FAMILIA_CANDIDATO = FAMILIA, PARTIDO_CANDIDATO = PARTIDO, ETIQUETA_CANDIDATO = ETIQUETA, APELLIDO_CANDIDATO,
            VOT_CANDIDATO,PCT_CANDIDATO = VOT_CANDIDATO/VOT_EF) 


### COMPACTAMOS BASE DEL CENSO ####

IMG1_07 <- IMG1_07 %>% 
  select(-ends_with("00"),-NIVEL) %>%
  gather(VARIABLE,PERSONAS,-CODGEO) %>% 
  separate(VARIABLE,c("SEXO","ACTIVIDAD","COND_MIGRATORIA","EDAD_4A")) %>%
  arrange(CODGEO) %>%
  filter(PERSONAS > 0) %>% 
  group_by(CODGEO) %>% 
  mutate(POB_TOTAL = sum(PERSONAS), 
         PCT_POB = PERSONAS/POB_TOTAL, 
         CAT_COMUNA = if_else(POB_TOTAL < 10000,"CHICA","GRANDE"))

### GUARDAR RESULTADOS ####

PRESIDENCIALES_2007 <- list(BASE_COMUNAS = BASE_COMUNAS,
                            ERRORES_COMUNAS = ERRORES_COMUNAS,
                            CENSO=IMG1_07,
                            VUELTA_1=VUELTA_1_PRES_07)
save(PRESIDENCIALES_2007,file = "DATOS_PROCESADOS/PRESIDENCIALES_2007.RData")









