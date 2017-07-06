# Bibliotecas que vou usar
library(shiny)
library(mc2d)
library(ggplot2)
library(lhs)
library(readxl)
library(dplyr)
library(oshcba)

texto_de_ajuda = ("Carregue os dados de Input e o app irá calcular as suas Despesas com Absenteismo")

ui <- fluidPage(title = "CAlculadora em SST",
  # Titulo
  # titlePanel("Simulador Probabilistico - v0"),
  navbarPage("Calculadora",
             tabPanel("Calcuadora e Histograma",
                      sidebarLayout(
                        sidebarPanel(
                          helpText(texto_de_ajuda),
                          fileInput("Arquivo de Inputs",
                                    inputId = "DadosInput")
                        ),
                        # Show a plot of the generated distribution
                        mainPanel("Teste",
                          plotOutput("histograma_absenteismo")
                        )
                      )
             )
  )
)

server <- function(input, output) {

  # Dados de Absenteismo simulados
  dados_simulados = reactive(
    {
      dados = simular_lhs(input$DadosInput)
      return(dados)
    })

  output$histograma_absenteismo <- renderPlot({
    qplot(dados,geom = "histogram",
          main="Histograma de Despesas em Absenteismo")
  })
}

shinyApp(ui = ui, server = server)
