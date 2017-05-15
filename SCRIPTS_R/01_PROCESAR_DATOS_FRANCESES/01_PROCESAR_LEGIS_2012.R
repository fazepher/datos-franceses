################################# PROCESAMIENTO DE DATOS FRANCESES ######################################
################################## ELECCIONES LEGISLATIVAS 2012 #######################################
###################################### FAZH ITAM 2017 130435 ############################################
#########################################################################################################

### PREÁMBULO ####
library(tidyverse)
library(magrittr)
library(stringr)
library(forcats)

### DATOS DE VOTACIÓN POR CASILLA ####

legis_12 <- read_csv2(file = "DATOS_BRUTOS/LG12_Bvot_T1T2_FAZH.txt", 
                      col_names = c("VUELTA",
                                    "COD_DEPARTAMENTO","COD_COMUNA","NOM_COMUNA",
                                    "CIRC_LEGIS","COD_CANT",
                                    "CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                    "NUM_CANDIDATO","APELLIDO_CANDIDATO","NOMBRE_CANDIDATO","ETIQUETA",
                                    "VOT_CANDIDATO"),
                      col_types = c("ccccccciiicccci"),
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
legis_12 <- read_delim(file = "DATOS_ADMINISTRATIVOS/depts2012.txt",
                         delim = "\t",locale = locale(encoding = "latin1")) %>%
  transmute(COD_REGION = REGION,COD_DEPARTAMENTO = DEP, NOM_DEPARTAMENTO = NCCENR) %>%
  left_join(legis_12,., by = "COD_DEPARTAMENTO")

# Agregamos las Regiones Antiguas
legis_12 <- read_delim(file = "DATOS_ADMINISTRATIVOS/reg2012.txt",
                         delim = "\t",locale = locale(encoding = "latin1")) %>%
  transmute(COD_REGION = REGION, NOM_REGION = NCCENR) %>%
  left_join(legis_12,., by = "COD_REGION")

# Agregamos los nombres de las Regiones Nuevas
legis_12 <- read_delim(file = "DATOS_ADMINISTRATIVOS/AUXILIAR_REG_DEPART.csv",
                         delim = ",",locale = locale(encoding = "latin1")) %>%
  select(-NOM_REGION) %>%
  left_join(legis_12,., by = "COD_REGION")

# Se ordenan las columnas, conservando solo las que nos importan
legis_12 <- legis_12 %>% 
  select(CODGEO,
         NOM_COMUNA,NOM_DEPARTAMENTO,NOM_REGION,NOM_NVA_REG,
         COD_COMUNA,COD_DEPARTAMENTO,COD_REGION,COD_NVA_REG,
         CASILLA,INSCRITOS,VOTANTES,VOT_EF,
         ETIQUETA, APELLIDO_CANDIDATO, VOT_CANDIDATO)

### BASE DEL CENSO ####
IMG2A_12 <- read_csv2(file = "DATOS_BRUTOS/BTT_TD_IMG2A_2012.txt", 
                      col_names = c("NIVEL","CODGEO","NOM_COM","C_SEXO","C_EDAD4_A","C_MIGRATORIA","C_TIPO_ACT","VALOR"),
                      col_types = "cccccccd",
                      locale = locale(decimal_mark = ",", encoding = "latin1"),
                      skip = 1) %>% 
  # Atención con los códigos
  mutate(C_SEXO = factor(C_SEXO,labels = c("HOMBRES","MUJERES")),
         C_EDAD4_A = factor(C_EDAD4_A),
         C_TIPO_ACT = factor(C_TIPO_ACT,labels = c("EMPLEADOS","DESEMPLEADOS","RETIRADOS","ESTUDIANTES","HOGAR","OTROS")),
         C_MIGRATORIA = factor(C_MIGRATORIA, labels = c("INMIGRANTES","LOCALES"))) %>%
  mutate(CAT = paste(C_SEXO,C_TIPO_ACT,C_MIGRATORIA,C_EDAD4_A,sep="_")) %>%
  select(CODGEO,NIVEL,CAT,VALOR) %>% 
  spread(CAT,VALOR)

### ERRORES DE  COMUNAS ####

# Para homologar resultados electorales y censo necesitamos las geografías administrativas oficiales para cada base
COMUNAS_12 <- read_delim(file = "DATOS_ADMINISTRATIVOS/comsimp2012.txt", 
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

COMUNAS_14 <- read_delim(file = "DATOS_ADMINISTRATIVOS/comsimp2014.txt", 
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

# Utilizamos los nombres oficiales del 2012
legis_12 <- legis_12 %>%
  left_join(COMUNAS_12,by="CODGEO") %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA = NOM_COMUNA.x)

# Comunas que están en los resultados electorales, pero no en el censo y que sí existían en 2012
ERRORES_COMUNAS <- anti_join(legis_12,IMG2A_12,by="CODGEO") %>% 
  semi_join(COMUNAS_12,by="CODGEO") %>% 
  select(CODGEO,NOM_COMUNA,NOM_DEPARTAMENTO) %>% 
  distinct %>% 
  cbind(MOTIVO_ERROR = NA)
# Errores por que se agregaron a otra comuna 
ERRORES_COMUNAS$MOTIVO_ERROR <- c("01/01/2013 : Agnières-en-Dévoluy devient commune déléguée au sein de Dévoluy (commune nouvelle).",
                                  "01/01/2013 : Bénévent-et-Charbillac devient commune déléguée au sein de Saint-Bonnet-en-Champsaur (commune nouvelle).",
                                  "01/01/2013 : La Cluse devient commune déléguée au sein de Dévoluy (commune nouvelle).",
                                  "01/01/2013 : Les Infournas devient commune déléguée au sein de Saint-Bonnet-en-Champsaur (commune nouvelle).",
                                  "01/01/2013 : Saint-Disdier devient commune déléguée au sein de Dévoluy (commune nouvelle).",
                                  "01/01/2013 : Melay devient commune déléguée au sein de Chemillé-Melay (commune nouvelle).",
                                  "01/01/2013 : Montpollin devient commune déléguée au sein de Baugé-en-Anjou (commune nouvelle).",
                                  "01/01/2013 : Pontigné devient commune déléguée au sein de Baugé-en-Anjou (commune nouvelle).",
                                  "01/01/2013 : Saint-Martin-d'Arcé devient commune déléguée au sein de Baugé-en-Anjou (commune nouvelle).",
                                  "01/01/2013 : Le Vieil-Baugé devient commune déléguée au sein de Baugé-en-Anjou (commune nouvelle).",
                                  "01/01/2013 : Vaulandry devient commune déléguée au sein de Clefs-Val d'Anjou (commune nouvelle).",
                                  "28/02/2013 : Pautaines-Augeville devient commune déléguée au sein d'Épizon (commune nouvelle).",
                                  "01/01/2013 : Bourg-de-Thizy devient commune déléguée au sein de Thizy-les-Bourgs (commune nouvelle).",
                                  "01/01/2013 : La Chapelle-de-Mardore devient commune déléguée au sein de Thizy-les-Bourgs (commune nouvelle).",
                                  "01/01/2013 : Mardore devient commune déléguée au sein de Thizy-les-Bourgs (commune nouvelle).",
                                  "01/01/2013 : Marnand devient commune déléguée au sein de Thizy-les-Bourgs (commune nouvelle).",
                                  "01/01/2013 : Nuelles devient commune déléguée au sein de Saint-Germain-Nuelles (commune nouvelle).",
                                  "01/01/2013 : Vitré devient commune déléguée au sein de Beaussais-Vitré (commune nouvelle).",
                                  "01/01/2013 : Voultegon devient commune déléguée au sein de Voulmentin (commune nouvelle).",
                                  "01/01/2013 : Le Magny devient commune déléguée au sein de Fontenoy-le-Château (commune nouvelle).")

# Se revisaron, las agrupaciones en el sitio del INSEE
# En los resultados electorales cambiamos primero los CODGEO para las comunas que se agregaron 
# Posteriormente cambiamos los nombres a los existentes en 2014.  
legis_12 <- legis_12 %>% 
  mutate(CODGEO = factor(CODGEO) %>% 
           fct_collapse("05139" = c("05002","05042","05138","05139"),
                        "05132" = c("05020","05067","05132"),
                        "49092" = c("49092","49199"), 
                        "49018" = c("49018","49213","49245","49303","49372"),
                        "49101" = c("49101","49380"), 
                        "52187" = c("52187","52379"),
                        "69248" = c("69025","69041","69128","69129","69248"),
                        "69208" = c("69144","69208"),
                        "79030" = c("79030","79353"),
                        "79242" = c("79242","79356"),
                        "88176" = c("88176","88282")) %>%
           as.character) %>%
  mutate(COD_COMUNA = if_else(CODGEO == "05139", "139",
                      if_else(CODGEO == "05132", "132",
                      if_else(CODGEO == "49092", "092", 
                      if_else(CODGEO == "49018", "018",
                      if_else(CODGEO == "49101", "101",
                      if_else(CODGEO == "52187", "187",
                      if_else(CODGEO == "69248", "248",
                      if_else(CODGEO == "69208", "208",
                      if_else(CODGEO == "79030", "030",
                      if_else(CODGEO == "79242", "242",
                      if_else(CODGEO == "88176", "176",COD_COMUNA)))))))))))) %>%
  mutate(NOM_COMUNA = if_else(CODGEO == "05139", "Dévoluy",
                      if_else(CODGEO == "05132", "Saint-Bonnet-en-Champsaur",
                      if_else(CODGEO == "49092", "Chemillé-Melay", 
                      if_else(CODGEO == "49018", "Baugé-en-Anjou",
                      if_else(CODGEO == "49101", "Clefs-Val d'Anjou",
                      if_else(CODGEO == "52187", "Épizon",
                      if_else(CODGEO == "69248", "Thizy-les-Bourgs",
                      if_else(CODGEO == "69208", "Saint-Germain-Nuelles",
                      if_else(CODGEO == "79030", "Beaussais-Vitré",
                      if_else(CODGEO == "79242", "Voulmentin",
                      if_else(CODGEO == "88176", "Fontenoy-le-Château",NOM_COMUNA))))))))))))

# Comunas que están en el censo, pero no en los resultados electorales y que sí existían en 2012
ERRORES_COMUNAS <- anti_join(IMG2A_12,legis_12,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  semi_join(COMUNAS_12,by="CODGEO") %>% 
  full_join(ERRORES_COMUNAS)

# Errores por ser comunas de las grandes ciudades
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  mutate(MOTIVO_ERROR = if_else(CODGEO == "13055","Marseille",
                                if_else(CODGEO == "69123","Lyon",
                                        if_else(CODGEO == "75056","Paris",MOTIVO_ERROR))))

# Errores por ser comunas de alta mar
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  mutate(MOTIVO_ERROR = if_else(substr(CODGEO,1,2) == "97","Alta Mar",MOTIVO_ERROR))

# Errores por ser "comunas muertas"
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  mutate(MOTIVO_ERROR = if_else(is.na(MOTIVO_ERROR),"Comune Morte pour la France",MOTIVO_ERROR))

# Se eliminan estas comunas del Censo
IMG2A_12 <- ERRORES_COMUNAS %>% 
  filter(MOTIVO_ERROR %in% c("Marseille","Lyon","Paris","Alta Mar","Comune Morte pour la France")) %>%
  extract2("CODGEO") %>%
  {filter(IMG2A_12,!{CODGEO %in% .})}


# Comunas que están en el censo, pero no en los resultados electorales y que sí existían en 2014
ERRORES_COMUNAS <- anti_join(IMG2A_12,legis_12,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  semi_join(COMUNAS_14, by="CODGEO") %>% 
  full_join(ERRORES_COMUNAS)

# Errores por que se agregaron a otra comuna 
ERRORES_COMUNAS <- ERRORES_COMUNAS %>%  
  mutate(MOTIVO_ERROR = if_else(CODGEO == "76095","01/01/2014 : Bihorel est rétablie. 
                                01/01/2012 : Bihorel devient commune déléguée au sein de Bois-Guillaume-Bihorel (commune nouvelle).",
                                MOTIVO_ERROR))


# Corregimos el error agregando los datos en el censo con el nombre existente en 2012. 

# Bihorel (76095) y Bois-Guillaume (76108) => Bois-Guillaume-Bihorel (76108)
IMG2A_12 <- IMG2A_12 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("76095","76108"),"76108",CODGEO)) 

# Agregamos los datos 
IMG2A_12 <- IMG2A_12 %>%
  group_by(NIVEL,CODGEO) %>%
  summarise_if(is.numeric,funs(sum(.,na.rm=TRUE))) %>%
  ungroup

# Comunas que están en el censo, pero no en los resultados electorales y que NO existían NI en 2012 NI en 2014
ERRORES_COMUNAS <- anti_join(IMG2A_12,legis_12,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  full_join(ERRORES_COMUNAS)

# Conservamos solo las columnas de CODGEO y el motivo del error
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  select(CODGEO,MOTIVO_ERROR)

### BASE ADMINISTRATIVA DE COMUNAS ####

BASE_COMUNAS <- legis_12 %>%
  select(CODGEO:COD_NVA_REG) %>%
  distinct %>% 
  left_join(select(IMG2A_12,CODGEO,NIVEL),by="CODGEO")

### RESULTADOS ELECTORALES POR COMUNA ####

VUELTA_1_LEGIS_12 <- legis_12 %>%
  select(CODGEO,INSCRITOS:VOT_CANDIDATO) %>%
  group_by(CODGEO,ETIQUETA,APELLIDO_CANDIDATO) %>%
  summarise_if(is.numeric,funs(sum(.))) %>%
  ungroup %>% 
  left_join(read_csv("BASES_CANDIDATOS/FAMILIAS_POLITICAS_12_LEGISLATIVAS.csv",
                     locale = locale(encoding = "latin1")),by="ETIQUETA") %>% 
  transmute(CODGEO,
            INSCRITOS,VOTANTES,PCT_PART = VOTANTES/INSCRITOS,VOT_EF,
            FAMILIA_CANDIDATO = FAMILIA, ETIQUETA_CANDIDATO = ETIQUETA, APELLIDO_CANDIDATO,
            VOT_CANDIDATO,PCT_CANDIDATO = VOT_CANDIDATO/VOT_EF) 


### COMPACTAMOS BASE DEL CENSO ####

IMG2A_12 <- IMG2A_12 %>% 
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

LEGISLATIVAS_2012 <- list(BASE_COMUNAS = BASE_COMUNAS,
                          ERRORES_COMUNAS = ERRORES_COMUNAS,
                          CENSO=IMG2A_12,
                          VUELTA_1=VUELTA_1_LEGIS_12)
save(LEGISLATIVAS_2012,file = "DATOS_PROCESADOS/LEGISLATIVAS_2012.RData")









