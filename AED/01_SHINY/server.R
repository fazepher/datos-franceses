####################### SERVER PARA ANALISIS EXPLORATORIO DE DATOS FRANCESES ###################

### CARGAMOS SHINY ####
library(shiny)


### PREAMBULO ####
library(tidyverse)
library(magrittr)
library(gridExtra)

# calcula_pob_por_comuna
# FunciOn que calcula la poblaciOn por comuna para una elecciOn particular
# ParAmetros
# 1) eleccion: Lista de la elecciOn a calcular (de nombre TIPO_AAAA y que contiene los tibbles de CENSO Y BASE_COMUNAS)
# 2) variables: (default "FALSE") vector de variables de cortes por las que se quiere obtener la poblaciOn de la comuna 
# si es FALSE se ignora y devuelve la poblaciOn total POB_TOTAL
# 3) porcentaje: (default "TRUE") lOgico, se debe usar la variable PCT_POB para obtener el porcentaje de la poblaciOn?
# si es FALSE se devuelve la suma de PERSONAS. Solo funciona si se proporciona el vector de variables.
# Resultado
# Un tibble con datos de la comuna y su poblaciOn
calcula_pob_por_comuna <- function(eleccion,variables = FALSE, porcentaje = TRUE){
  if(is.character(variables)){
    r <- eleccion %>% 
      extract2("CENSO") %>% 
      group_by_(.dots = variables,add=TRUE)
    if(porcentaje){
      r %<>% summarise(VARIABLE_EXPLICATIVA = sum(PCT_POB)) %>% 
        ungroup %>%
        unite_("GRUPO_SOCIAL",variables[variables != "CAT_COMUNA"])
    } else {
      r %<>% summarise(VARIABLE_EXPLICATIVA = sum(PERSONAS)) %>% 
        ungroup %>%
        unite_("GRUPO_SOCIAL",variables[variables != "CAT_COMUNA"])
    }
  } else{
    r <- eleccion %>% 
      extract2("CENSO") %>% 
      select(CODGEO,CAT_COMUNA,POB_TOTAL) %>% 
      ungroup %>% 
      distinct(CODGEO,POB_TOTAL,CAT_COMUNA)
  }
  r %<>% 
    full_join(extract2(eleccion,"BASE_COMUNAS"),.,by="CODGEO")
  return(r)
}

### DATOS GENERALES ####

# Leemos los datos ya procesados y unificados
load("DATOS_PROCESADOS/PRESIDENCIALES_2007.RData")
load("DATOS_PROCESADOS/LEGISLATIVAS_2007.RData")
load("DATOS_PROCESADOS/EUROPEAS_2009.RData")
load("DATOS_PROCESADOS/PRESIDENCIALES_2012.RData")
load("DATOS_PROCESADOS/LEGISLATIVAS_2012.RData")

# Vector de colores de Familias PolIticas
colores_familias <- c("FN"="darkslategrey",
                      "DERECHA" = "dodgerblue4",
                      "IZQUIERDA" = "lightcoral",
                      "CENTRO" = "orange1",
                      "OTRAS DERECHAS" = "steelblue2",
                      "OTRAS IZQUIERDAS" = "firebrick3",
                      "OTROS" = "forestgreen")


### FUNCION SERVER SHINY ####
function(input,output){
  
  # Variable Censal
  output$Variable <- renderUI(checkboxGroupInput("variable","Elija la(s) variable(s) censal(es) explicativas",
                                          choices = c("Sexo"="SEXO",
                                                      "Grupos de Edad" = "EDAD_4A",
                                                      "Estatus Migratorio" = "COND_MIGRATORIA",
                                                      "Actividad" = "ACTIVIDAD"),
                                          "Sexo"))

  # Grupo de Variable
  output$Valor = renderUI({
    
    eleccion <- switch(input$eleccion,
                       "PRESIDENCIALES 2007" = PRESIDENCIALES_2007,
                       "LEGISLATIVAS 2007" = LEGISLATIVAS_2007,
                       "EUROPEAS 2009" = EUROPEAS_2009,
                       "PRESIDENCIALES 2012" = PRESIDENCIALES_2012,
                       "LEGISLATIVAS 2012" = LEGISLATIVAS_2012
    )
    
    POB_POR_COMUNA_POR_VARIABLE <- calcula_pob_por_comuna(eleccion,
                                                          variables = c("CAT_COMUNA",input$variable),
                                                          porcentaje = input$porcentaje)
    
    distinct(POB_POR_COMUNA_POR_VARIABLE,GRUPO_SOCIAL) %>% 
      selectInput("valor","Elija un grupo de esta variable", choices = . , extract2(.,1,"GRUPO_SOCIAL"))
                          
    })
  
  
  output$grafico <- renderPlot({
    
    eleccion <- switch(input$eleccion,
                       "PRESIDENCIALES 2007" = PRESIDENCIALES_2007,
                       "LEGISLATIVAS 2007" = LEGISLATIVAS_2007,
                       "EUROPEAS 2009" = EUROPEAS_2009,
                       "PRESIDENCIALES 2012" = PRESIDENCIALES_2012,
                       "LEGISLATIVAS 2012" = LEGISLATIVAS_2012
    )
    
    POB_POR_COMUNA_POR_VARIABLE <- calcula_pob_por_comuna(eleccion,
                                                          variables = c("CAT_COMUNA",input$variable),
                                                          porcentaje = input$porcentaje)
  
  datos_graf <- extract2(eleccion,"VUELTA_1") %>% 
    mutate(FAMILIA_CANDIDATO = factor(FAMILIA_CANDIDATO,
                                      levels=c("FN",
                                               "DERECHA",
                                               "IZQUIERDA",
                                               "CENTRO",
                                               "OTRAS DERECHAS",
                                               "OTRAS IZQUIERDAS",
                                               "OTROS"),
                                      ordered = TRUE)) %>% 
    select(CODGEO,FAMILIA_CANDIDATO,PCT_CANDIDATO) %>% 
    full_join(POB_POR_COMUNA_POR_VARIABLE,.) %>% 
    filter_(paste("GRUPO_SOCIAL ==","\"",input$valor,"\"",sep="")) %>% 
    filter(COD_NVA_REG == input$region) %>% 
    mutate(PCT_CANDIDATO_C = (PCT_CANDIDATO*(n()-1)+0.5)/n())
    
  graf_1 <- ggplot(datos_graf,aes(y=pnorm(PCT_CANDIDATO),x=VARIABLE_EXPLICATIVA,color=FAMILIA_CANDIDATO)) + 
      geom_point(size=0.8,alpha=0.5) + 
      scale_color_manual(values=colores_familias)+
      geom_smooth(method = "lm")
    
  graf_2 <- ggplot(datos_graf,aes(x=PCT_CANDIDATO,fill=FAMILIA_CANDIDATO))+
      geom_density(alpha=0.5,adjust=2)+
      scale_fill_manual(values=colores_familias)
    
  if(input$cat){
    graf_1 <- graf_1 + facet_grid(CAT_COMUNA~FAMILIA_CANDIDATO)
    graf_2 <- graf_2 + facet_grid(CAT_COMUNA~FAMILIA_CANDIDATO)
  } else{
    graf_1 <- graf_1 + facet_grid(.~FAMILIA_CANDIDATO)
    graf_2 <- graf_2 + facet_grid(.~FAMILIA_CANDIDATO)
  }
  
  
  
  grid.arrange(graf_1,graf_2,nrow=2)
})
    
}
