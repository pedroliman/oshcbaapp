# Plots

resultados_cbr = resultados$Resultados_CBR


dados = resultados_cbr %>% filter(Cenario.y == "Iniciativa1") %>% select(Cenario.y, BeneficioAbsenteismo)

var = dplyr::pull(dados[,3])
title = names(dados[,3])
qplot(var, geom = 'blank', main=title) +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + 
  # stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.7) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  theme(legend.position = c(0.85, 0.85))



var_fit = fitdistrplus::fitdist(var,"norm")
var_medio = as.numeric(var_fit$estimate[["mean"]])
media_text = paste("Media = ", round(var_medio,2))
a = round(confint(var_fit, parm = c("mean")),2)
paste(media_text, "entre (", a[1],"e", a[2],") com 95% de confianca.")






ggplot(data=resultados_cbr, aes(resultados_cbr$Soma_DespesaTurnover.y)) + 
  geom_histogram(aes(y =..average..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count")

