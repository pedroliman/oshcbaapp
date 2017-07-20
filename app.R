# Este é um app que possui a interface para a calculadora oshcba.
## Se está rodando o app pela primeira vez, rode estes comando antes de executar o aplicativo: (Antes de rodar o comando, exclua o "#")
# install.packages(c("shiny","ggplot2","readxl","mc2d","dplyr","devtools"))
#Se precisar atualizaro app, rode este comando:
# library(devtools)
# install_github("pedroliman/oshcba")
library(fitdistrplus)
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
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Calculadora SST | FPS",
    tabPanel("Inputs",
             sidebarPanel(
               "Faca Upload de seus dados de Input",
                   fileInput("Dados de Input",
                             inputId = "DadosInput",buttonLabel = "Arquivo.xlsx"),
               "Apos informar os dados de inputs, verifique na guia ao lado se suas informacoes foram carregadas. Caso contrario, verifique se o arquivo de dados esta correto."
             ),
             mainPanel(
               "Abaixo serao exibidos os inputs que voce inseriu no arquivo de dados.<br>",
               tabsetPanel(
                 tabPanel("Configuracoes",
                          tableOutput("configstable")
                 ),
                 tabPanel("Parametros",
                          tableOutput("parametrostable")
                 ),
                 tabPanel("Custos",
                          tableOutput("custostable")
                 )
               )
             )
    ),
    tabPanel("Processamento",
             "Esta aba apresenta resultados da simulacao. Use esta aba para observar os calculos realizados pela calculadora.",
             mainPanel(
             tabsetPanel(
               tabPanel("Resultados das Simulacoes",
                        "Mostrando primeiras 100 linhas dos resultados",
                        tableOutput("resultados_descontadostable")
                        ),
               tabPanel("Resultados da Análise de Custo Beneficio",
                        "Mostrando primeiras 100 linhas dos resultados",
                        tableOutput("resultados_cbrtable")
                        ),
               tabPanel("Console",
                        verbatimTextOutput("consoletext")
               )
              )
             )
             ),
    tabPanel("Resultados",
             "Esta aba apresenta resultados da simulacao. Use esta aba para verificar os dados simulados.",
             mainPanel(
               tabsetPanel(
                 tabPanel("Resultados Gerais",
                          selectInput("Iniciativa", "Selecione a Iniciativa para exibir os Graficos",
                                                                  c("Iniciativa1", "Iniciativa2", "Iniciativa3", "Iniciativa4", "Iniciatva5", "Iniciativa6", "Iniciativa7", "Iniciativa8", "Iniciativa9", "Iniciativa10", "TodasIniciativas")
                                      ),
                          fluidRow(
                              column(6,
                                     plotOutput("beneficioabsenteismo_plot"),
                                     verbatimTextOutput("beneficioabsenteismo_confinttext"),
                                     plotOutput("beneficioturnover_plot"),
                                     verbatimTextOutput("beneficioturnover_confinttext"),
                                     plotOutput("beneficiomultas_plot"),
                                     verbatimTextOutput("beneficiomultas_confinttext")
                                     ),
                              column(6,
                                     plotOutput("beneficioacoesregressivas_plot"),
                                     verbatimTextOutput("beneficioacoesregressivas_confinttext"),
                                     plotOutput("beneficiototal_plot"),
                                     verbatimTextOutput("beneficiototal_confinttext"),
                                     plotOutput("razaocustobeneficio_plot"),
                                     verbatimTextOutput("razaocustobeneficio_confinttext")
                                     )
                            )

                          # tableOutput("resultados_descontadostable")
                 )
               )
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
    arquivoinputs = CarregaDados()
    # if (is.null(arquivoinputs))
    #   return(NULL)
    # inputs = carregar_inputs(arquivoinputs)
    return(carregar_inputs(inputs))
  })
  
  # Dados de Absenteismo simulados
  output_oshcba = reactive({
      inputs = CarregaDados()
      if (is.null(inputs))
        return(NULL)
      withProgress(message = 'Calculando...', value = 0.3, {
        dados = simular_cba(paste(inputs$datapath, ".xlsx", sep=""), modo = "completo")
        incProgress(1, detail = "Finalizando")
      })
      return(dados)
    })
  # Parametros
  resultados_inputs = reactive({
      output_oshcba()$Inputs
    })
  # Resultados CBR  
  resultados_cbr = reactive({
      output_oshcba()$Resultados_CBR
    })
  
  # Resultados
  resultados = reactive({
      output_oshcba()$Resultados
    })
  
  # Resultados Descontados
  resultados_descontados = reactive({
      output_oshcba()$Resultados_Descontados
    })
  
  # Iniciativas Simuladas: Este código provavelmente deveria ser implementado fora do layout
  # iniciativas = reactive({
  #   inputs = inputs()
  #   if (is.null(inputs))
  #     return(NULL)
  #   iniciativas = obter_cenarios(inputs) %>%
  #     filter(!CenarioASIS) %>%
  #     select(Cenario)
  #   iniciativas = as.vector(t(iniciativas))
  #   return(iniciativas)
  # })
  
  # output$histogramas <- renderPlot({
  #   dados_simulados = resultados_cbr()
  #   if (!is.null(dados_simulados))
  #     dados_simulados = dados_simulados %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAbsenteismo, BeneficioTurnover, BeneficioMultas, BeneficioAcoesRegressivasINSS
  #                                                                                            , BeneficioTotalCBR, RazaoBeneficioCusto)
  #     #dados_simulados = dados_simulados %>% filter(Cenario == input$Iniciativa) %>% select(Cenario, NFaltas, Nev_Afmenor15_Tipico, Nev_Afmaior15_Tipico, Nev_Safast_Tipico, Nev_Obito_Tipico, Nev_Afmenor15_Trajeto, Nev_Afmaior15_Trajeto, Nev_Safast_Trajeto, Nev_Obito_Trajeto, Nev_Afmenor15_DoenOcup, Nev_Afmaior15_DoenOcup, Nev_Safast_DoenOcup, Nev_Obito_DoenOcup, Nev_Afmenor15_NRelac, Nev_Afmaior15_NRelac, Nev_Safast_NRelac, Nev_Obito_NRelac, DespesaTurnover, NSubstituidos, DiasAbsenteismo, DespesaAbsenteismo, DespesaMultas, NumeroMultas_Lei1, DespesaAcoesRegressivasINSS, AcoesRegressivasINSS, Nev_AcaoRegressivaINSSAcumulado, Nev_AcaoRegressivaINSS)
  # 
  #   ggplot(data = melt(dados_simulados), mapping = aes(x = value)) + 
  #     geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x')
  #   
  #   # qplot(dados_simulados$RazaoBeneficioCusto,geom = "histogram",
  #   #       main="Histograma de Despesas em Absenteismo")
  # })
  
  
  output$beneficioabsenteismo_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAbsenteismo)
    var = dplyr::pull(dados[,3])
    title = names(dados[,3])
    qplot(var, geom = 'blank', main=title) +   
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
      # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
      geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
      theme(legend.position = c(0.85, 0.85))
  })
  output$beneficioturnover_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTurnover)
    var = dplyr::pull(dados[,3])
    title = names(dados[,3])
    qplot(var, geom = 'blank', main=title) +   
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
      # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
      geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
      theme(legend.position = c(0.85, 0.85))
  })
  output$beneficiomultas_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioMultas)
    var = dplyr::pull(dados[,3])
    title = names(dados[,3])
    qplot(var, geom = 'blank', main=title) +   
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
      # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
      geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
      theme(legend.position = c(0.85, 0.85))
  })
  output$beneficioacoesregressivas_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAcoesRegressivasINSS)
    var = dplyr::pull(dados[,3])
    title = names(dados[,3])
    qplot(var, geom = 'blank', main=title) +   
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
      # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
      geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
      theme(legend.position = c(0.85, 0.85))
  })
  output$beneficiototal_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTotalCBR)
    var = dplyr::pull(dados[,3])
    title = names(dados[,3])
    qplot(var, geom = 'blank', main=title) +   
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
      # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
      geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
      theme(legend.position = c(0.85, 0.85))
  })
  output$razaocustobeneficio_plot = renderPlot({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, RazaoBeneficioCusto)
    var = dplyr::pull(dados[,3])
    title = names(dados[,3])
    qplot(var, geom = 'blank', main=title) +   
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
      # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
      geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
      theme(legend.position = c(0.85, 0.85))
  })

  output$beneficioabsenteismo_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAbsenteismo)
    var = dplyr::pull(dados[,3])
    var_fit = fitdistrplus::fitdist(var,"norm")
    var_medio = as.numeric(var_fit$estimate[["mean"]])
    media_text = paste("Media = ", round(var_medio,2))
    a = round(confint(var_fit, parm = c("mean")),2)
    paste(media_text, "entre (", a[1],"e", a[2],") com 95% de confianca.")
  })
  output$beneficioturnover_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTurnover)
    var = dplyr::pull(dados[,3])
    var_fit = fitdistrplus::fitdist(var,"norm")
    var_medio = as.numeric(var_fit$estimate[["mean"]])
    media_text = paste("Media = ", round(var_medio,2))
    a = round(confint(var_fit, parm = c("mean")),2)
    paste(media_text, "entre (", a[1],"e", a[2],") com 95% de confianca.")
  })
  output$beneficiomultas_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioMultas)
    var = dplyr::pull(dados[,3])
    var_fit = fitdistrplus::fitdist(var,"norm")
    var_medio = as.numeric(var_fit$estimate[["mean"]])
    media_text = paste("Media = ", round(var_medio,2))
    a = round(confint(var_fit, parm = c("mean")),2)
    paste(media_text, "entre (", a[1],"e", a[2],") com 95% de confianca.")
  })
  output$beneficioacoesregressivas_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioAcoesRegressivasINSS)
    var = dplyr::pull(dados[,3])
    var_fit = fitdistrplus::fitdist(var,"norm")
    var_medio = as.numeric(var_fit$estimate[["mean"]])
    media_text = paste("Media = ", round(var_medio,2))
    a = round(confint(var_fit, parm = c("mean")),2)
    paste(media_text, "entre (", a[1],"e", a[2],") com 95% de confianca.")
  })
  output$beneficiototal_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, BeneficioTotalCBR)
    var = dplyr::pull(dados[,3])
    var_fit = fitdistrplus::fitdist(var,"norm")
    var_medio = as.numeric(var_fit$estimate[["mean"]])
    media_text = paste("Media = ", round(var_medio,2))
    a = round(confint(var_fit, parm = c("mean")),2)
    paste(media_text, "entre (", a[1],"e", a[2],") com 95% de confianca.")
  })
  output$razaocustobeneficio_confinttext = renderPrint({
    dados = resultados_cbr() %>% filter(Cenario.y == input$Iniciativa) %>% select(Cenario.y, RazaoBeneficioCusto)
    var = dplyr::pull(dados[,3])
    var_fit = fitdistrplus::fitdist(var,"norm")
    var_medio = as.numeric(var_fit$estimate[["mean"]])
    media_text = paste("Media = ", round(var_medio,2))
    a = round(confint(var_fit, parm = c("mean")),2)
    paste(media_text, "entre (", a[1],"e", a[2],") com 95% de confianca.")
  })
  
  output$resultadostable <- renderTable({
    head(resultados(),n = 100)
  })
  
  output$resultados_descontadostable <- renderTable({
    head(resultados_descontados(), n = 100)
  })
  
  output$resultados_cbrtable <- renderTable({
    head(resultados_cbr(),n = 100)
  })
  
  output$configstable <- renderTable({
    resultados_inputs()$Configs
  })
  
  output$parametrostable <- renderTable({
    resultados_inputs()$Parametros
  })
  
  # output$cenariostable <- renderTable({
  #   resultados_inputs()$Cenarios
  # })
  
  output$custostable <- renderTable({
    resultados_inputs()$Custos
  })
  
  
  output$completetable <- renderTable({
    resultados_cbr()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("output_simulacao", '.csv', sep='') },
    content = function(file) {
      write.table(resultados_cbr(),file,sep=";",dec=",",row.names = FALSE)
    }
  )
  
}


# Run the application
shinyApp(ui = ui, server = server)