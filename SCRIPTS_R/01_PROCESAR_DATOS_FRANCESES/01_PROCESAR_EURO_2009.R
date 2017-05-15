################################# PROCESAMIENTO DE DATOS FRANCESES ######################################
##################################### ELECCIONES EUROPEAS 2009 ##########################################
###################################### FAZH ITAM 2017 130435 ############################################
#########################################################################################################

### PREÁMBULO ####
library(tidyverse)
library(magrittr)
library(stringr)

### DATOS DE VOTACIÓN POR CASILLA ####

euro_09 <- read_csv2(file = "DATOS_BRUTOS/ER09_BVOT_FAZH.txt", 
                     col_names = c("VUELTA",
                                   "COD_DEPARTAMENTO","COD_COMUNA","NOM_COMUNA",
                                   "CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                   "APELLIDO_CANDIDATO","NOMBRE_CANDIDATO","ETIQUETA",
                                   "VOT_CANDIDATO"),
                     col_types = c("ccccciiiccci"),
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

# Se detectaron algunos errores de origen en la base de resultados electorales. 
# Se eliminan en este momento, al verificar los datos oficiales de forma manual en 
# http://www.interieur.gouv.fr/Elections/Les-resultats/Europeennes/elecresult__europeennes_2009/(path)/europeennes_2009/05/082/001/001001.html

# Hay una fila extra del candidato LOUIS de la lista inexistente LDD
euro_09 <- filter(euro_09,!{APELLIDO_CANDIDATO == "LOUIS" & ETIQUETA == "LDD"})
# Hay una fila extra del candidato SANMARTIN de la lista inexistente LDV
euro_09 <- filter(euro_09,!{APELLIDO_CANDIDATO == "LOUIS" & ETIQUETA == "LDD"})
# Hay una fila extra de la candidata GOMEZ de la lista inexistente LPC
euro_09 <- filter(euro_09,!{APELLIDO_CANDIDATO == "GOMEZ" & ETIQUETA == "LPC"})

# Agregamos los Departamentos
# https://www.insee.fr/fr/information/2560452
euro_09 <- read_delim(file = "DATOS_ADMINISTRATIVOS/depts2009.txt",
                         delim = "\t",locale = locale(encoding = "latin1")) %>%
  transmute(COD_REGION = REGION,COD_DEPARTAMENTO = DEP, NOM_DEPARTAMENTO = NCCENR) %>%
  left_join(euro_09,., by = "COD_DEPARTAMENTO")

# Agregamos las Regiones Antiguas
euro_09 <- read_delim(file = "DATOS_ADMINISTRATIVOS/reg2009.txt",
                         delim = "\t",locale = locale(encoding = "latin1")) %>%
  transmute(COD_REGION = REGION, NOM_REGION = NCCENR) %>%
  left_join(euro_09,., by = "COD_REGION")

# Agregamos los nombres de las Regiones Nuevas
euro_09 <- read_delim(file = "DATOS_ADMINISTRATIVOS/AUXILIAR_REG_DEPART.csv",
                         delim = ",",locale = locale(encoding = "latin1")) %>%
  select(-NOM_REGION) %>%
  left_join(euro_09,., by = "COD_REGION")

# Se ordenan las columnas, conservando solo las que nos importan
euro_09 <- euro_09 %>% 
  select(CODGEO,
         NOM_COMUNA,NOM_DEPARTAMENTO,NOM_REGION,NOM_NVA_REG,
         COD_COMUNA,COD_DEPARTAMENTO,COD_REGION,COD_NVA_REG,
         CASILLA,INSCRITOS,VOTANTES,VOT_EF,
         ETIQUETA, APELLIDO_CANDIDATO, VOT_CANDIDATO)

### BASE DEL CENSO ####
IMG2A_09 <- read_csv2(file = "DATOS_BRUTOS/BTT_TD_IMG2A_2009.txt", 
                      col_names = c("NIVEL","CODGEO","COD_REG","COD_DEP","EPCI","C_SEXO","C_EDAD4_A","C_TIPO_ACT","C_MIGRATORIA","VALOR"),
                      col_types = "cccccccccd",
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

COMUNAS_11 <- read_delim(file = "DATOS_ADMINISTRATIVOS/comsimp2011.txt", 
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

# Utilizamos los nombres oficiales del 2009
euro_09 <- euro_09 %>%
  left_join(COMUNAS_09,by="CODGEO") %>% 
  mutate(NOM_COMUNA.x=if_else(is.na(NOM_COMUNA.y),NOM_COMUNA.x,NOM_COMUNA.y)) %>% 
  select(-NOM_COMUNA.y) %>%
  rename(NOM_COMUNA = NOM_COMUNA.x)

# Comunas que están en los resultados electorales, pero no en el censo y que sí existían en 2009
ERRORES_COMUNAS <- anti_join(euro_09,IMG2A_09,by="CODGEO") %>% 
  semi_join(COMUNAS_09,by="CODGEO") %>% 
  select(CODGEO,NOM_COMUNA,NOM_DEPARTAMENTO) %>% 
  distinct %>% 
  cbind(MOTIVO_ERROR = NA)
# Errores por que se agregaron a otra comuna 
ERRORES_COMUNAS$MOTIVO_ERROR <- c("09/12/2010 : Fort-Mardyck est rattachée à Dunkerque (fusion association).",
                                  "09/12/2010 : Saint-Pol-sur-Mer est rattachée à Dunkerque (fusion association).")

# Se revisaron, las agrupaciones en el sitio del INSEE
# Corregimos el error en los resultados electorales con el nombre existente en 2009. 
# Fort-Mardyck (59248), Saint-Pol-sur-Mer (59540) y Dunkerque (59183) => Dunkerque (59183) 
euro_09 <- euro_09 %>% 
  mutate(CODGEO = if_else(CODGEO %in% c("59248","59540","59183"),"59183",CODGEO)) %>% 
  mutate(COD_COMUNA = if_else(CODGEO %in% c("59248","59540","59183"),"183",COD_COMUNA),
         NOM_COMUNA = if_else(CODGEO %in% c("59248","59540","59183"),"Dunkerque",NOM_COMUNA))

# Comunas que están en el censo, pero no en los resultados electorales y que sí existían en 2009
ERRORES_COMUNAS <- anti_join(IMG2A_09,euro_09,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  semi_join(COMUNAS_09,by="CODGEO") %>% 
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
IMG2A_09 <- ERRORES_COMUNAS %>% 
  filter(MOTIVO_ERROR %in% c("Marseille","Lyon","Paris","Alta Mar")) %>%
  extract2("CODGEO") %>%
  {filter(IMG2A_09,!{CODGEO %in% .})}

# Comunas que están en el censo, pero no en los resultados electorales y que sí existían en 2011
ERRORES_COMUNAS <- anti_join(IMG2A_09,euro_09,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  semi_join(COMUNAS_11, by="CODGEO") %>% 
  full_join(ERRORES_COMUNAS)

# Comunas que están en el censo, pero no en los resultados electorales y que NO existían NI en 2009 NI en 2011
ERRORES_COMUNAS <- anti_join(IMG2A_09,euro_09,by="CODGEO") %>% 
  select(CODGEO,NIVEL) %>% 
  distinct %>%
  full_join(ERRORES_COMUNAS)

# Conservamos solo las columnas de CODGEO y el motivo del error
ERRORES_COMUNAS <- ERRORES_COMUNAS %>% 
  select(CODGEO,MOTIVO_ERROR)

### BASE ADMINISTRATIVA DE COMUNAS ####

BASE_COMUNAS <- euro_09 %>%
  select(CODGEO:COD_NVA_REG) %>%
  distinct %>% 
  left_join(select(IMG2A_09,CODGEO,NIVEL),by="CODGEO")

### RESULTADOS ELECTORALES POR COMUNA ####

VUELTA_1_EURO_09 <- euro_09 %>%
  select(CODGEO,INSCRITOS:VOT_CANDIDATO) %>%
  group_by(CODGEO,ETIQUETA,APELLIDO_CANDIDATO) %>%
  summarise_if(is.numeric,funs(sum(.))) %>%
  ungroup %>% 
  left_join(read_csv("BASES_CANDIDATOS/FAMILIAS_POLITICAS_09_EUROPEAS.csv",
                     locale = locale(encoding = "latin1")),by="ETIQUETA") %>% 
  transmute(CODGEO,
            INSCRITOS,VOTANTES,PCT_PART = VOTANTES/INSCRITOS,VOT_EF,
            FAMILIA_CANDIDATO = FAMILIA, ETIQUETA_CANDIDATO = ETIQUETA, APELLIDO_CANDIDATO,
            VOT_CANDIDATO,PCT_CANDIDATO = VOT_CANDIDATO/VOT_EF) 


### COMPACTAMOS BASE DEL CENSO ####

IMG2A_09 <- IMG2A_09 %>% 
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

EUROPEAS_2009 <- list(BASE_COMUNAS = BASE_COMUNAS,
                          ERRORES_COMUNAS = ERRORES_COMUNAS,
                          CENSO=IMG2A_09,
                          VUELTA_1=VUELTA_1_EURO_09)
save(EUROPEAS_2009,file = "DATOS_PROCESADOS/EUROPEAS_2009.RData")









