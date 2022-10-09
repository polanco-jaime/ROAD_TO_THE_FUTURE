#######################
#https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
#https://lost-stats.github.io/Model_Estimation/Research_Design/two_by_two_difference_in_difference.html

#load("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
lista = c('readr','readxl',
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
library(ggplot2)
library(sqldf)
library(arrow)
library(plyr)
gc()
########################## loading full data ################################## 
#setwd("C:/Users/USER/Desktop/DID roads/Data")
#temp = list.files("C:/Users/USER/Desktop/DID roads/Data",pattern="*.csv")
#list2env(
#  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
#         read.csv), envir = .GlobalEnv)
# SEGUNDA VERSION
if (1==1){
    setwd("C:/Users/USER/Desktop/DID roads/Data")
    temp = list.files("C:/Users/USER/Desktop/DID roads/Data",pattern="*.PARQUET")
    list2env(
      lapply(setNames(temp, make.names(gsub(gsub("*.PARQUET$", "", temp), replacement = "", pattern = "00000000000" ))), 
             read_parquet), envir = .GlobalEnv) 
    
    Road_to_the_future <- mget(ls(pattern = "Road_to_the_future_*"))
    Road_to_the_future  = plyr::rbind.fill(Road_to_the_future)
    Road_to_the_future =  subset(Road_to_the_future, Road_to_the_future$Fecha_inicio_consecion != 1994 )
    schools_id_name = data.frame( 'cod_dane' =  unique(Road_to_the_future$cole_cod_dane_institucion) )
    schools_id_name$id_name  = row_number(schools_id_name$cod_dane)
    #Road_to_the_future$buffer_km = ifelse(Road_to_the_future$buffer_km <=500, 500 , Road_to_the_future$buffer_km )
    rm(list = ls()[grepl("Road_to_the_future_", ls())])
    #base_roads_to_the_future = read_parquet("Road_to_the_future_000000000000.PARQUET")
    
    ################################################################################################################################################################################
     
    Road_to_the_future_l500 = Road_to_the_future %>% subset(Road_to_the_future$buffer_km <= 1000)
    
    Road_to_the_future$buffer_km =ifelse(Road_to_the_future$buffer_km <= 1000, 1000, Road_to_the_future$buffer_km)
     
    
    
    Road_to_the_future = sqldf("
                               SELECT * FROM Road_to_the_future
                               INNER JOIN schools_id_name
                               ON cole_cod_dane_institucion = cod_dane
                               ")  
    
    Road_to_the_future_l500 = sqldf("
                               SELECT * FROM Road_to_the_future_l500
                               INNER JOIN schools_id_name
                               ON cole_cod_dane_institucion = cod_dane
                               ")  
    #### Setting base for buffer to 1 km
    library(readr)
    base_ols <- read_csv("C:/Users/USER/Desktop/DID roads/Data/base_ols.csv")
    
    table(Road_to_the_future$buffer_km)
}

if (2==2){    
    ##############
     
    if (5 ==5 ) {
       
      
      data  = Road_to_the_future
      
      base_  <- data %>%
        mutate(treat = treat_) %>%
        mutate(cole_cod_dane = as.character(cole_cod_dane_institucion)) %>%
        mutate(year =  as.integer(ANIO) ) %>%
        mutate(year_treated_fic =  as.numeric( data$Fecha_inicio_consecion))%>%
        mutate(year_treated_f10p =  as.numeric( data$Fecha_al_10_) )%>%
        mutate(year_treated_fai =  as.numeric( data$Fecha_Acta_de_inicio) )%>%
        mutate(math_c =  as.numeric( data$median_norm_math_c) )%>%
        mutate(reading_c =  as.numeric( data$median_norm_reading_c) ) %>%
        mutate(Participate_saberpro =  as.numeric( data$Participate_saberpro) )  %>%
        mutate(id_name =  as.numeric( data$id_name) ) 
     
      sql_ai = paste0("
               SELECT *,
                  CASE WHEN treat = 0 
                    THEN 0 ELSE year_treated 
                  END AS year_treated_att,  
                  CASE WHEN treat = 0 
                    THEN 10000 ELSE year_treated 
                  END AS year_treated_sa,
                  CASE WHEN year_treated IS NOT NULL THEN
                      CASE WHEN BASE != 'CONTROL'THEN year - year_treated 
                      ELSE 0 END
                  ELSE 0  END AS time_to_treat       
        
     FROM (
          SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes_prom,
                  treat, cole_cod_dane , id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fai  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fai  END  , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza,buffer_km
             
     )
     
    " )  
      
      sql_f10p = paste0("
     SELECT * ,
     CASE WHEN treat = 0 THEN 10000 ELSE year_treated END AS year_treated_sa,
                  CASE WHEN treat = 0 
                    THEN 0 ELSE year_treated 
                  END AS year_treated_att,   
      CASE WHEN year_treated IS NOT NULL THEN
            CASE WHEN 
                  BASE != 'CONTROL'
                THEN year - year_treated
            ELSE 0 END 
      ELSE 0  END AS time_to_treat     
        
     FROM (
          SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes,
                  treat, cole_cod_dane  , id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_f10p  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_f10p  END , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
              
             
     )
     
    " ) 
      
      
      sql_ic = paste0("
     SELECT * ,
     CASE WHEN treat = 0 THEN 10000 ELSE year_treated END AS year_treated_sa,
                  CASE WHEN treat = 0 
                    THEN 0 ELSE year_treated 
                  END AS year_treated_att,   
    CASE WHEN year_treated IS NOT NULL THEN
      CASE WHEN BASE != 'CONTROL'THEN year - year_treated ELSE 0 END 
      ELSE 0  END AS time_to_treat 
      FROM (
           SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes,
                  treat, cole_cod_dane  ,  id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fic  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fic  END  , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
    
     )
     
    " ) 
    
    sql_ent = paste0("
     SELECT * ,
     
                  CASE WHEN treat = 0 
                  THEN 10000 ELSE year_treated END AS year_treated_sa,
                  CASE WHEN treat = 0 
                    THEN 0 ELSE year_treated 
                  END AS year_treated_att,   
    CASE WHEN year_treated IS NOT NULL THEN
      CASE WHEN BASE != 'CONTROL'THEN year - year_treated ELSE 0 END 
      ELSE 0  END AS time_to_treat
      FROM (
           SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes,
                  treat, cole_cod_dane  ,  id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      Fecha_entrega  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      Fecha_entrega  END  , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
    
     )
     
    " )
      
      
      assign(paste0('base_ai' ), data.frame(sqldf(sql_ai)), envir = .GlobalEnv)
      assign(paste0('base_10p'), data.frame(sqldf(sql_f10p)), envir = .GlobalEnv)
      assign(paste0('base_ic'), data.frame(sqldf(sql_ic)), envir = .GlobalEnv)
      assign(paste0('base_ent'), data.frame(sqldf(sql_ent)), envir = .GlobalEnv)
      
    }else{
      print("no")
    }
    
     
    if (5 ==5 ) {
      
      
      data  = Road_to_the_future_l500
      
      base_  <- data %>%
        mutate(treat = treat_) %>%
        mutate(cole_cod_dane = as.character(cole_cod_dane_institucion)) %>%
        mutate(year =  as.integer(ANIO) ) %>%
        mutate(year_treated_fic =  as.numeric( data$Fecha_inicio_consecion))%>%
        mutate(year_treated_f10p =  as.numeric( data$Fecha_al_10_) )%>%
        mutate(year_treated_fai =  as.numeric( data$Fecha_Acta_de_inicio) )%>%
        mutate(math_c =  as.numeric( data$median_norm_math_c) )%>%
        mutate(reading_c =  as.numeric( data$median_norm_reading_c) ) %>%
        mutate(Participate_saberpro =  as.numeric( data$Participate_saberpro) )  %>%
        mutate(id_name =  as.numeric( data$id_name) ) 
      
      table(base_$year_treated_fai) 
      
      sql_ai = paste0("
               SELECT * ,
                  CASE WHEN treat = 0 
                    THEN 10000 ELSE year_treated 
                  END AS year_treated_sa,
                  CASE WHEN year_treated IS NOT NULL THEN
                      CASE WHEN BASE != 'CONTROL'THEN year - year_treated 
                      ELSE 0 END
                  ELSE 0  END AS time_to_treat       
        
     FROM (
          SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes_prom,
                  treat, cole_cod_dane , id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fai  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fai  END  , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza,buffer_km
             
     )
     
    " )  
      
      sql_f10p = paste0("
     SELECT * ,
     CASE WHEN treat = 0 THEN 10000 ELSE year_treated END AS year_treated_sa,
      CASE WHEN year_treated IS NOT NULL THEN
            CASE WHEN 
                  BASE != 'CONTROL'
                THEN year - year_treated
            ELSE 0 END 
      ELSE 0  END AS time_to_treat     
        
     FROM (
          SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes,
                  treat, cole_cod_dane  , id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_f10p  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_f10p  END , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
              
             
     )
     
    " ) 
      
      
      sql_ic = paste0("
     SELECT * ,
     CASE WHEN treat = 0 THEN 10000 ELSE year_treated END AS year_treated_sa,
    CASE WHEN year_treated IS NOT NULL THEN
      CASE WHEN BASE != 'CONTROL'THEN year - year_treated ELSE 0 END 
      ELSE 0  END AS time_to_treat 
      FROM (
           SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes,
                  treat, cole_cod_dane  ,  id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fic  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      year_treated_fic  END  , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
    
     )
     
    " ) 
      
      sql_ent = paste0("
     SELECT * ,
     CASE WHEN treat = 0 THEN 10000 ELSE year_treated END AS year_treated_sa,
    CASE WHEN year_treated IS NOT NULL THEN
      CASE WHEN BASE != 'CONTROL'THEN year - year_treated ELSE 0 END 
      ELSE 0  END AS time_to_treat
      FROM (
           SELECT  avg(math_c) math_c,  avg(reading_c)  reading_c, 
                  sum(Participate_saberpro) Participate_saberpro,
                  avg(FAC_EXP_ESTU_TRABAJA) estu_trabaja,
                  avg(FAC_EXP) estudiantes,
                  treat, cole_cod_dane  ,  id_name,
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      Fecha_entrega  END  as year_treated, 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
                   
              FROM base_ WHERE year < 2020 
              GROUP BY 
              treat, cole_cod_dane  , 
                  CASE WHEN BASE = 'CONTROL' THEN NULL ELSE 
                      Fecha_entrega  END  , 
                  year ,  COLE_MCPIO_UBICACION  , BASE,cole_naturaleza, buffer_km
    
     )
     
    " )
      
      
      assign(paste0('l500_base_ai' ), data.frame(sqldf(sql_ai)), envir = .GlobalEnv)
      assign(paste0('l500_base_10p'), data.frame(sqldf(sql_f10p)), envir = .GlobalEnv)
      assign(paste0('l500_base_ic'), data.frame(sqldf(sql_ic)), envir = .GlobalEnv)
      assign(paste0('l500_base_ent'), data.frame(sqldf(sql_ent)), envir = .GlobalEnv)
      
    }else{
      print("no")
    }
    
    
    
    library(plyr)

}



rm(list = ls()[grepl("Model_SA_Reading", ls())]) 
rm(list = ls()[grepl("base_ai_", ls())])
rm(list = ls()[grepl("base_10p_", ls())])
rm(list = ls()[grepl("base_ic_", ls())])
rm(list = ls()[grepl("base_ent_", ls())])

rm(list = ls()[grepl("base_roads_to_the_future", ls())])
rm(data)
rm(base_)
rm(df)
rm(Road_to_the_future)
rm(Road_to_the_future_l500)

table(l500_base_10p $buffer_km)
table(base_10p $buffer_km)

base_ai$fuente ="base_ai"
 
base_10p$fuente ="base_10p"
colnames(base_10p)[5] = "estudiantes"
colnames(base_ai)[5] = "estudiantes"

base_ai = base_ai %>%  select( colnames(base_10p) )
BASE = rbind(base_ai, base_10p)
BASE = subset(BASE, buffer_km == 1000)
write.csv(BASE, 'BASE.csv')
table(base_ai $buffer_km)

table(base_ai$time_to_treat)
table(base_10p$year_treated)
table(base_ic$year_treated)
table(base_ent$year_treated)

###############3
#save.image("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
source('C:/Users/USER/Desktop/DID roads/functions.R')

