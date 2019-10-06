#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dvvsc <- function(tvvsc,vvvsc){
  dvvsc=tvvsc/vvvsc
  return(dvvsc)
}
dvvsc(80,20)

#Viaje vacio sobre camino
tvvsc <- function(dvvsc){ tvvsc=dvvsc/88.3 
return(tvvsc)}
t2<-input$DE/88.2, #Viaje vacio sobre lote
tvvsl <- function(dvvsl){tvvsl=dvvsl/88.2
return(tvvsl)}
t3<- exp(1.96+0.62*log(input$VolT/(input$VolT/input$DE*100))), #Viaje mientras carga
t4<- exp(0.11+0.73*log(input$VolT)-0.29*log(input$VolI)), #Carga
t5<- input$DE/75.4, # viaje cargado sobre lote
t6<- input$DSC/109.4, # Viaje cargado sobre camino
t7<- exp(0.11+(0.78*log(input$VolT))-(0.32*log(input$VolI)))
)




ui <- fluidPage(
  # Titulo ----
  titlePanel("Tiempos de actividades"),
  # Definicion del menu de entrada y salida ----
  sidebarLayout(
    # Sidebar para demostrar varias opciones de menu ----
    sidebarPanel(
      # Input: Distancia de carreteo sobre camino ----
      sliderInput("DSC", "Distancia de carreteo sobre camino:",
                  min = 50, max = 500,
                  value = 150),
      # Input: Distancia de Extraccion ----
      sliderInput("DE", "Distancia de extracci?n:",
                  min = 50, max = 500,
                  value = 250, step = 50),
      # Input: Distancia entre vias de saca ----
      # sliderInput("DVS", "Distancia entre v?as de saca:",
      #            min = 50, max = 400,
      #            value = 200, step = 25),
      # Input: Volumen del rodal ----
      selectInput(inputId = "VolumenRodal",
                  label = "Volumen a cosechar",
                  choices = sort(unique(c(50,100,150,200,250,300,350,400))),
                  multiple = F),
      # Input:  Volumen total de carga ----
      sliderInput("VolT", "Volumen total de carga:",
                  min = 5, max = 20,
                  value = 10),
      # Input: Volumen individual ----
      sliderInput("VolI", "Volumen del producto:",
                  min = 0.05, max = 0.5,
                  value = 0.3, step = 0.1)
      # Input: Potencia ----
      #selectInput(inputId = "Pot",
      #           label = "Potencia del tractor",
      #          choices = sort(unique(datos$potencia)),
      #         multiple = FALSE, 
      #        selected = 90),
      #selectInput(inputId = "department",
      #           label = "Operaci?n",
      #          choices = sort(unique(datos$tipo_practica)),
      #         multiple = TRUE)
    ),
    
    # Panel principal de salidas ----
    mainPanel(
      # Output: Tabla resumen de los valores de entrada ----
      tableOutput("values"),
      tableOutput("values2"),
      # Output: grafico de las variables seleccionadas ----
      #plotOutput("plot1"),
      # Output: grafico con ggplot ----
      plotOutput("plot2"),
      #tableOutput2("values2")
      plotOutput("plot3"),
      tableOutput("values3")
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  sliderValues3<- reactive({
    data.frame(DE1  = as.double(c(d<-seq(50,500,by=50))),
               PEF = as.double(c(pef <-(input$VolT/((input$DSC/88.3+
                                                       d/88.2+
                                                       exp(1.96+0.62*log(input$VolT/(input$VolT/input$DE*100)))+
                                                       exp(0.11+0.73*log(input$VolT)-0.29*log(input$VolI))+
                                                       d/75.4+ # viaje cargado sobre lote
                                                       input$DSC/109.4+ # Viaje cargado sobre camino
                                                       exp(0.11+(0.78*log(input$VolT))-(0.32*log(input$VolI))))/60))
               )),
               PEF2 = as.double(c(pef <-1.1*(input$VolT/((input$DSC/88.3+
                                                            d/88.2+
                                                            exp(1.96+0.62*log(input$VolT/(input$VolT/input$DE*100)))+
                                                            exp(0.11+0.73*log(input$VolT)-0.29*log(input$VolI))+
                                                            d/75.4+ # viaje cargado sobre lote
                                                            input$DSC/109.4+ # Viaje cargado sobre camino
                                                            exp(0.11+(0.78*log(input$VolT))-(0.32*log(input$VolI))))/60))
               )),
               PEF3 = as.double(c(pef <-0.9*(input$VolT/((input$DSC/88.3+
                                                            d/88.2+
                                                            exp(1.96+0.62*log(input$VolT/(input$VolT/input$DE*100)))+
                                                            exp(0.11+0.73*log(input$VolT)-0.29*log(input$VolI))+
                                                            d/75.4+ # viaje cargado sobre lote
                                                            input$DSC/109.4+ # Viaje cargado sobre camino
                                                            exp(0.11+(0.78*log(input$VolT))-(0.32*log(input$VolI))))/60))
               )),
               stringsAsFactors = FALSE
    )
  })
  sliderValues2<- reactive({
    data.frame(Actividad= c("Tiempo total"),
               Tiempo = as.double(c(tt <-input$DSC/88.3+
                                      input$DE/88.2+
                                      exp(1.96+0.62*log(input$VolT/(input$VolT/input$DE*100)))+
                                      exp(0.11+0.73*log(input$VolT)-0.29*log(input$VolI))+
                                      input$DE/75.4+ # viaje cargado sobre lote
                                      input$DSC/109.4+ # Viaje cargado sobre camino
                                      exp(0.11+(0.78*log(input$VolT))-(0.32*log(input$VolI)))
               )),
               stringsAsFactors = FALSE
    )
  })
  # Exprecion reactiva para crear los datos que van en la tabla ----
  sliderValues <- reactive({
    data.frame(
      Actividad = c("Viaje vacio sobre camino:",
                    "Viaje vacio sobre lote:",
                    "Movimiento en la carga:",
                    "Carga:",
                    "Viaje cargado sobre lote:",
                    "Viaje cargado sobre camino:",
                    "Descarga:"),
      Tiempo = as.double(c(t1<-input$DSC/88.3, #Viaje vacio sobre camino
                           t2<-input$DE/88.2, #Viaje vacio sobre lote
                           t3<- exp(1.96+0.62*log(input$VolT/(input$VolT/input$DE*100))), #Viaje mientras carga
                           t4<- exp(0.11+0.73*log(input$VolT)-0.29*log(input$VolI)), #Carga
                           t5<- input$DE/75.4, # viaje cargado sobre lote
                           t6<- input$DSC/109.4, # Viaje cargado sobre camino
                           t7<- exp(0.11+(0.78*log(input$VolT))-(0.32*log(input$VolI)))
      )),
      Unidad =as.character( c("Min",
                              "Min",
                              "Min",
                              "Min",
                              "Min",
                              "Min",
                              "Min")),
      #Porcentajes =as.character( c(p1<-round(t1/tt*100),
      #                            p2<-round(t2/tt*100),
      #                           p3<-round(t3/tt*100),
      #                          p4<-round(t4/tt*100),
      #                         p5<-round(t5/tt*100),
      #                        p6<-round(t6/tt*100),
      #                       p7<-round(t7/tt*100)
      #                      )),
      stringsAsFactors = FALSE
      # Generate a summary of the dataset
    )
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  output$values2 <- renderTable({
    sliderValues2()
  })
  
  #  output$plot2<- renderPlot({
  #    ggplot(datos, aes(DE, PEF))+geom_point()+
  #      geom_point(aes(x=input$DSC,y=25), col="red", size= 5)+
  #     geom_smooth(method="lm", formula=y~x, col="black")
  #  })
  output$plot2<- renderPlot({
    ggplot(sliderValues3(), aes(x=DE1, y=PEF))+geom_point()+
      geom_smooth(method="loess", formula=y~x, col="black")+
      geom_line(aes(x=DE1, y=PEF2), col="red")+
      geom_line(aes(x=DE1, y=PEF3), col="red")+
      geom_point(aes(x=input$DE,y=mean(PEF)), col="red", size= 5)+
      xlab("Distancia de Extraccion") + ylab("Productividad Efectiva") + # Set axis labels
      ggtitle("Productividad de los tractorcitos") +     # Set title
      theme_bw()
  })
  output$plot3 <- renderPlot({
    bp<- ggplot(sliderValues(), aes(x="", y=Tiempo, fill=Actividad))+
      geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
      scale_fill_brewer() + theme_minimal()+
      theme(axis.text.x=element_blank()) +
      geom_text(aes(y = Tiempo/7 + c(0, cumsum(Tiempo)[-length(Tiempo)]), 
                    label = percent(Tiempo/100)), size=5)
    
    bp
  })
}
# Create Shiny app ----
shinyApp(ui, server)
