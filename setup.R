g# Setup

bibliotecas = (c("shiny","ggplot2","readxl","mc2d","dplyr","devtools"))
setup_oshcba = function () {
  # Instalar Bibliotecas do Cran
  carregar_bibliotecas_cran(bibliotecas)
  instalar_oshcba()
}

instalar_oshcba = function () {
  library(devtools)
  install_github("pedroliman/oshcba")
}

carregar_bibliotecas_cran <- function(bibliotecas){
  for( b in bibliotecas ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( b , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( b , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}