# Este é um app que possui a interface para a calculadora oshcba.
## Se está rodando o app pela primeira vez, rode estes comando antes de executar o aplicativo: (Antes de rodar o comando, exclua o "#")
# install.packages(c("shiny","ggplot2","readxl","mc2d","dplyr","devtools"))
#Se precisar atualizaro app, rode este comando:
# library(devtools)
# install_github("pedroliman/oshcba")
library(shiny)
library(shinythemes)
library(oshcba)
library(ggplot2)
library(readxl)
library(mc2d)
library(dplyr)
library(reshape2)
library(purrr)

### Definindo uma seed fixa fora do app para ter replicações mehor comparáveis.
set.seed(1000)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("Calculadora de Custos e Beneficios SST - V. 1.0.0"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      "Faca Upload de seus dados de Input",
      fileInput("Dados de Input",
                inputId = "DadosInput",buttonLabel = "Arquivo.xlsx")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("INpu",
                 selectInput("Iniciativa", "Selecione a Iniciativa para exibir os Graficos",
                             c("Iniciativa1", "Iniciativa2", "Iniciativa3", "Iniciativa4", "Iniciatva5", "Iniciativa6", "Iniciativa7", "Iniciativa8", "Iniciativa9", "Iniciativa10", "TodasIniciativas")),
                 plotOutput("histograma_absenteismo")
                 ), 
        tabPanel("Tabela de Resultados", tableOutput("table"), downloadButton('downloadData', 'Baixar Tabela de Resultados'))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Esta função apenas retorna o arquivo de Dados
  CarregaDados <- reactive({
    
    validate(
      need(input$DadosInput != "", "Escolha o arquivo de simulacao de dados corretamente!")
    )
    
    arquivodados <- input$DadosInput
    if (is.null(arquivodados))
      return(NULL)
    file.copy(arquivodados$datapath,
              paste(arquivodados$datapath, ".xlsx", sep=""))
    return(arquivodados)
  })
  
  # Esta função retorna a lista inputs
  inputs = reactive({
    
    validate(
      need(CarregaDados() != "", "Escolha o arquivo de simulacao de dados corretamente!")
    )
    
    arquivoinputs = CarregaDados()
    # if (is.null(arquivoinputs))
    #   return(NULL)
    # inputs = carregar_inputs(arquivoinputs)
    return(carregar_inputs(inputs))
  })
  
  # Dados de Absenteismo simulados
  output_oshcba = reactive(
    {
      
      
      inputs = CarregaDados()
      if (is.null(inputs))
        return(NULL)
      dados = simular_cba(paste(inputs$datapath, ".xlsx", sep=""), modo = "completo")
      return(dados)
    })
  
  # Parametros
  resultados_inputs = reactive(
    {
      output_oshcba()$Inputs
    })
  # Resultados CBR  
  resultados_cbr = reactive(
    {
      output_oshcba()$Resultados_CBR
    })
  
  # Resultados
  resultados = reactive(
    {
      output_oshcba()$Resultados
    })
  
  # Resultados Descontados
  resultados = reactive(
    {
      output_oshcba()$ResultadosDescontados
    })
  
  # Iniciativas Simuladas: Este código provavelmente deveria ser implementado fora do layout
  iniciativas = reactive({
    inputs = inputs()
    if (is.null(inputs))
      return(NULL)
    iniciativas = obter_cenarios(inputs) %>%
      filter(!CenarioASIS) %>%
      select(Cenario)
    iniciativas = as.vector(t(iniciativas))
    return(iniciativas)
  })
  
  output$histograma_absenteismo <- renderPlot({
    dados_simulados = resultados_cbr()
    if (!is.null(dados_simulados))
      dados_simulados = dados_simulados %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAbsenteismo, BeneficioTurnover, BeneficioMultas, BeneficioAcoesRegressivasINSS
                                                                                             , BeneficioTotalCBR, RazaoBeneficioCusto)

    
      
      #dados_simulados = dados_simulados %>% filter(Cenario == input$Iniciativa) %>% select(Cenario, NFaltas, Nev_Afmenor15_Tipico, Nev_Afmaior15_Tipico, Nev_Safast_Tipico, Nev_Obito_Tipico, Nev_Afmenor15_Trajeto, Nev_Afmaior15_Trajeto, Nev_Safast_Trajeto, Nev_Obito_Trajeto, Nev_Afmenor15_DoenOcup, Nev_Afmaior15_DoenOcup, Nev_Safast_DoenOcup, Nev_Obito_DoenOcup, Nev_Afmenor15_NRelac, Nev_Afmaior15_NRelac, Nev_Safast_NRelac, Nev_Obito_NRelac, DespesaTurnover, NSubstituidos, DiasAbsenteismo, DespesaAbsenteismo, DespesaMultas, NumeroMultas_Lei1, DespesaAcoesRegressivasINSS, AcoesRegressivasINSS, Nev_AcaoRegressivaINSSAcumulado, Nev_AcaoRegressivaINSS)

    ggplot(data = melt(dados_simulados), mapping = aes(x = value)) + 
      geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x')
    
    # qplot(dados_simulados$RazaoBeneficioCusto,geom = "histogram",
    #       main="Histograma de Despesas em Absenteismo")
  })
  
  output$table <- renderTable({
    head(resultados_cbr(),n = 100)
  })
  
  output$completetable <- renderTable({
    resultados_cbr()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("output_simulacao", '.csv', sep='') },
    content = function(file) {
      #write.csv(datasetInput(), file)
      write.table(resultados_cbr(),file,sep=";",dec=",",row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)