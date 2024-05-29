
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
