#######################
#https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#https://lost-stats.github.io/Model_Estimation/Research_Design/two_by_two_difference_in_difference.html
#setwd("C:/Users/USER/Desktop/DID roads/")
# Downloading tabless
global_path = 'C:/Users/USER/Desktop/DID roads/ROAD_TO_THE_FUTURE/'
shell.exec(paste0(global_path ,'Omit/descarga_bases.bat') )



#load("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
lista = c('readr','readxl','sqldf','plyr', 
           'did' , 'arrow',  'plyr', 'ggplot2',
          'dplyr','fixest' , 'gargle' , 'stringr'
          #, 'bigrquery' 
)
for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}
 
 
gc()
########################## loading full data ################################## 

setwd(paste0(global_path ,'Data'))
temp = list.files("./",pattern="*.parquet")

 
list2env(
  lapply(setNames(temp, make.names(gsub("*.parquet$", "", temp))), 
         arrow::read_parquet), envir = .GlobalEnv)

# base_ols <- read_csv("base_ols.csv")
# colnames(base_ols)
# summary(lm(data = base_ols, math_c~DISTANCE  + factor(treat_) ) )

 
rm(temp) 
rm(lista)
rm(i)
###############3
#save.image("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
source(paste0(global_path ,'scripts/functions.R'))
 
