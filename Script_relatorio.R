library(oshcba)
library(dplyr)
library(ggplot2)

# Declarando as Funções para o Gráfico


caminho_dados = "D:/OneDrive/Analise_SESI/Dados.xlsx"

media_var = function(var){
  media_text = paste("Média = ", round(mean(var),2))
  a = round(quantile(var, c(0.0275, 0.975)),2)
  paste(media_text, "entre (", a[1],"e", a[2],") em 95% dos casos.")
}

plot_var = function(var, nome) {
  title = nome
  qplot(var, geom = 'blank', main=title) +   
    geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
    # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
    geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
    scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
    theme(legend.position = c(0.85, 0.85))
}



resultados = simular_cba(ArquivoInputs = caminho_dados, modo = "completo")

resultados_cbr = resultados$Resultados_CBR

resultados_descontados = resultados$Resultados_Descontados

# Escrevendo Planilha de Dados
oshcba::exportar_dados_simulados(resultados_cbr)


## Resultados por Iniciativa

iniciativa = "Iniciativa1"

dados = resultados_cbr %>% filter(Cenario.y == iniciativa)

variaveis = names(dados)

variaveis = variaveis[40:60]

resultados_cbr.df = as.data.frame(resultados_cbr) 

for (i in 1:length(variaveis)) {
  variavel = variaveis[i]
  print(variavel)
  print(plot_var(var = resultados_cbr.df[[variavel]], nome = variavel))
  print(media_var(resultados_cbr.df[[variavel]]))
  # variavel = "CustoTotalCBR"
  # resultados_cbr[[variavel]]
}
