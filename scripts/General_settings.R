#######################
#https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#https://lost-stats.github.io/Model_Estimation/Research_Design/two_by_two_difference_in_difference.html
#setwd("C:/Users/USER/Desktop/DID roads/")

if (Sys.info()["nodename"] ==  "Jaimes-MacBook-Pro.local" ){
  General_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/ROAD_TO_THE_FUTURE/" 
  data_dir <- paste0(General_path , "Data/")
  graphs_dir <-  paste0(General_path , "Graph/") 
  tables_dir <- paste0(General_path , "Tables/")   
}  else if ( Sys.info()["nodename"] ==  "51768"  ){
  General_path ="~/Polanco/ROAD_TO_THE_FUTURE" 
  data_dir <- paste0(General_path , "Data/")
  graphs_dir <-  paste0(General_path , "Graph/") 
  tables_dir <- paste0(General_path , "Tables/")   
}


global_path = General_path

 
lista = c('readr','readxl','sqldf','plyr', 'arrow',
          'did' , 'arrow',  'plyr', 'ggplot2',
          'dplyr','fixest' , 'gargle' , 'stringr', 'broom', 
          "panelView", "bacondecomp","fect","PanelMatch","bigrquery" # "paneltools",
)
for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}
source(paste0(global_path ,'/scripts/R/functions.R'))
source("~/Polanco/apis.R")

