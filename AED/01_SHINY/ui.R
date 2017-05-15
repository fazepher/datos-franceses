
library(shiny)

# Define UI for dataset viewer application
fluidPage(
  
  # Application title
  titlePanel("Mi primer Shiny de Datos Franceses"),
  
  sidebarLayout(
    sidebarPanel(
      # Botones para elegir la eleccion sobre la que se hara el analisis
      radioButtons("eleccion", "Escoja una ELECCION:", 
                  choices = c("PRESIDENCIALES 2007", 
                              "LEGISLATIVAS 2007", 
                              "EUROPEAS 2009",
                              "PRESIDENCIALES 2012",
                              "LEGISLATIVAS 2012")),
      
      # Cuadro para elegir la region a analizar
      selectInput("region", "Escoja la REGION:", 
                  choices = c("Auvergne-Rh\u00F4ne-Alpes"="84", 
                              "Hauts-de-France"="32", 
                              "Provence-Alpes-C\u00F4te d\'Azur"="93",
                              "Grand-Est"="44",
                              "R\u00E9gion Occitanie"="76",
                              "Normandie"="28",
                              "Nouvelle-Aquitaine"="75",
                              "Centre-Val de Loire"="24",
                              "Corse"="94",
                              "Bourgogne-Franche-Comt\u00E9"="27",
                              "Bretagne"="53",
                              "Pays de la Loire"="52",
                              "\u00CEle-de-France"="11")),
      
      # Casilla para activar la separacion por categoria de comunas o no
      checkboxInput("cat", "Separar entre comunas chicas y grandes", FALSE),
      
      # Casilla para activar la separacion por categoria de comunas o no
      checkboxInput("porcentaje", "Utilizar para la variable explicativa porcentaje?", TRUE),
      
      # Cuadro para elegir la variable censal
      uiOutput("Variable"),
      
      # Cuadro para elegir el grupo de la variable censal
      uiOutput("Valor")
    ),
    
    mainPanel(
      
      plotOutput("grafico")
    )
  )
)
