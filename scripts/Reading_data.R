#######################
#https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#https://lost-stats.github.io/Model_Estimation/Research_Design/two_by_two_difference_in_difference.html
#setwd("C:/Users/USER/Desktop/DID roads/")
# Downloading tabless
global_path = 'C:/Users/USER/Desktop/DID roads/'
# shell.exec(paste0(global_path ,'Omit/descarga_bases.bat') )
# mainDir <- 'C:/Users/USER/Desktop/DID roads/ROAD_TO_THE_FUTURE/Data'
# subDir <- "graph"
global_path = 'C:/Users/USER/Desktop/DID roads/ROAD_TO_THE_FUTURE/'
#dir.create(file.path(mainDir, subDir))

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

# source("C:/Users/USER/Desktop/DID roads/ROAD_TO_THE_FUTURE/scripts/functions.R", echo=TRUE) 
source(paste0(global_path ,'scripts/functions.R'))
# devtools::source_url("https://github.com/JAPJ182/ROAD_TO_THE_FUTURE/blob/main/scripts/functions.R")  
gc()
########################## loading full data ################################## 

setwd(paste0(global_path ,'Data'))
temp = list.files("./",pattern="*.parquet")

 
list2env(
  lapply(setNames(temp, make.names(gsub("*.parquet$", "", temp))), 
         arrow::read_parquet), envir = .GlobalEnv  )

## Biased ATT



tabla = base_ic %>% subset( .$buffer_km == 1000 )
ATT_biased(base = tabla , Y = 'reading_c_sd', 
           treat= 'treat_' , period_var = 'year')

tabla = base_ai %>% subset( .$buffer_km == 1000 )
ATT_biased(base = tabla , Y = 'reading_c_sd', 
           treat= 'treat_' , period_var = 'year')

tabla = base_10p %>% subset( .$buffer_km == 1000 )
ATT_biased(base = tabla , Y = 'reading_c_sd', 
           treat= 'treat_' , period_var = 'year')

tabla = base_50p %>% subset( .$buffer_km == 1000 )
ATT_biased(base = tabla , Y = 'reading_c_sd', 
           treat= 'treat_' , period_var = 'year')


tabla = base_ent %>% subset( .$buffer_km == 1000 )
ATT_biased(base = tabla , Y = 'reading_c_sd', 
           treat= 'treat_' , period_var = 'year')


# base_ols <- read_csv("base_ols.csv")
# colnames(base_ols)
# summary(lm(data = base_ols, math_c~DISTANCE  + factor(treat_) ) )

 
rm(temp) 
rm(lista)
rm(i)
###############3
#save.image("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
warning("the data of project had been read")
table(base_10p$buffer_km)
base_10p$buffer_km = ifelse(base_10p$buffer_km<=2000,2000,base_10p$buffer_km)
base_50p$buffer_km =  ifelse(base_50p$buffer_km<=2000,2000,base_50p$buffer_km)
base_ai$buffer_km = ifelse(base_ai$buffer_km<=2000,2000,base_ai$buffer_km)
base_ent$buffer_km =  ifelse(base_ent$buffer_km<=2000,2000,base_ent$buffer_km)
base_ic$buffer_km = ifelse(base_ic$buffer_km<=2000,2000,base_ic$buffer_km)
