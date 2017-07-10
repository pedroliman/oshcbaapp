
library(shiny)
library(mc2d)
library(ggplot2)
library(lhs)
library(readxl)
library(dplyr)
library(oshcba)
library(reshape2)
library(purrr)

simulado = simular_temp_absenteismo()

variaveis = c("RazaoBeneficioCusto", "BeneficioTotal", "BeneficioAbsenteismo", "CustoTotalCBR")

dados_grafico = simulado %>% select(Cenario.y, BeneficioAbsenteismo, BeneficioTotalCBR, RazaoBeneficioCusto) %>% filter(Cenario.y == "Iniciativa2")


# Nada do que está aqui em baixo funcionou. Preciso estudar o GGPLOT para fazer estas coisas:
ggplot(data = melt(dados_grafico), mapping = aes(x = value)) + 
  geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x')


ggplot(data = melt(dados_grafico), mapping = aes(x = value)) + 
  geom_histogram(bins = 15) + facet_grid(. ~ Cenario.y)


p <- ggplot(mpg, aes(displ, cty)) + geom_point()

p + facet_grid(. ~ cyl)