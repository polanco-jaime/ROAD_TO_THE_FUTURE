# Set your query
################################################################
## si tab no funion  "Alt + Shift + [".
options(scipen=111)
summary(base_ols)
base_ols$DISTANCE = base_ols$DISTANCE/1000
base_ols_treat = subset(base_ols, base_ols$BASE != "CONTROL")
hist(base_ols_treat$DISTANCE, 20) 
 

summary(lm(data= base_ols_treat, median_norm_math_c ~ log(DISTANCE) * treat ) )

summary(lm(data= base_ols_treat, median_norm_reading_c ~ log(DISTANCE) ) )


summary(lm(data= base_ols, (median_norm_math_c) ~   (DISTANCE) * BASE  ) )

summary(lm(data= base_ols, median_norm_reading_c ~  (DISTANCE)   * BASE ) )

############# Section 1 ############################
#### TWFE ####
###  TWFE for Math
#################################################
#### Train 1
### Median of schools who studies into the fist buffer (1 km)
### Treated group according road operation input 

##### time 0
###
library(fixest)

 #glimpse(base1)
############# math
#rm(list = ls()[grepl("Read", ls())])
rm(list = ls()[grepl("model", ls())])
rm(list = ls()[grepl("TWFE", ls())])
rm(list = ls()[grepl("MODEL", ls())])
rm(list = ls()[grepl("df", ls())])

base_ic$math_c =  100*base_ic$math_c
base_10p$math_c =  100*base_10p$math_c
base_ai$math_c =  100*base_ai$math_c
base_ent$math_c =  100*base_ent$math_c

base_ic$reading_c =  100*base_ic$reading_c
base_10p$reading_c =  100*base_10p$reading_c
base_ai$reading_c =  100*base_ai$reading_c
base_ent$reading_c =  100*base_ent$reading_c
summary(base_10p$Participate_saberpro) 
summary(base_10p$estu_trabaja) 

#base_ic$estu_trabaja = log( base_ic$estu_trabaja  )
#base_10p$estu_trabaja = log( base_10p$estu_trabaja  )
#base_ai$estu_trabaja = log( base_ai$estu_trabaja  )
#base_ent$estu_trabaja = log( base_ent$estu_trabaja  )

#base_ic$estudiantes = log( base_ic$estudiantes  )
#base_10p$estudiantes = log( base_10p$estudiantes  )
#base_ai$estudiantes = log( base_ai$estudiantes  )
#base_ent$estudiantes = log( base_ent$estudiantes  )

base_ic$frac_trabaja =  ( base_ic$estu_trabaja / base_ic$estudiantes  )
base_10p$frac_trabaja =  ( base_10p$estu_trabaja /base_10p$estudiantes  )
base_ai$frac_trabaja =  ( base_ai$estu_trabaja /base_ai$estudiantes  )
base_ent$frac_trabaja =  ( base_ent$estu_trabaja /base_ent$estudiantes  )
summary(base_ent$frac_trabaja)
base_ic$finished_uni =  ( base_ic$Participate_saberpro / base_ic$estudiantes  )
base_10p$finished_uni =  ( base_10p$Participate_saberpro /base_10p$estudiantes  )
base_ai$finished_uni =  ( base_ai$Participate_saberpro /base_ai$estudiantes  )
base_ent$finished_uni =  ( base_ent$Participate_saberpro /base_ent$estudiantes  )

 
############# Section 1 ############################
#### TWFE ####
#################################################
if(1==1){
  kilometros = c(1000,1500,2000,2500,3000,3500,4000,4500)
  reference_time = -1
  
  tablas = c( 'base_ai','base_10p', 'base_ic', 'base_ent')
  
  for (j in tablas) {
    print(j)
    tabla = j
    lista_modelos = paste0('TWFE_Math_', j)
    Sun_Abraham_Modelos =   paste0('SA_Math_',  j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    for (i in kilometros) {
      
      df = get(tabla)
      df = df %>% subset(df$time_to_treat >= -8)
      MODEL = feols( math_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
                    |  cole_cod_dane+ year,                             ## FEs
                    cluster = ~ cole_cod_dane,                              ## Clustered SEs
                    data = subset(df, df$buffer_km == i )
                    )
      MODEL_SA = feols( math_c ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                       |  cole_cod_dane+ year,                             ## FEs
                       cluster = ~ cole_cod_dane,                         ## Clustered SEs
                       data = subset(df, df$buffer_km == i ) )
      
      name_in_enviroment = paste0("Score at ", i, " Meters")
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      
      if (j == 'base_10p') {
        TWFE_Math_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_Math_base_10p[[name_in_enviroment]] <- (MODEL_SA) 
      } else if (j == 'base_ic') {
        TWFE_Math_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_Math_base_ic[[name_in_enviroment]] <- (MODEL_SA) 
      } else if (j == 'base_ent') {
        TWFE_Math_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_Math_base_ent[[name_in_enviroment]] <- (MODEL_SA) 
      }  else if (j == 'base_ai') {
        TWFE_Math_base_ai[[name_in_enviroment]] <- (MODEL)
        SA_Math_base_ai[[name_in_enviroment]] <- (MODEL_SA) 
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
    
  }
  
  for (j in tablas) {
    print(j)
    tabla = j
    lista_modelos = paste0('TWFE_Reading_', j)
    Sun_Abraham_Modelos =   paste0('SA_Reading_',  j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
     
    for (i in kilometros) {
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df = df %>% subset(df$time_to_treat >= -8)
      ####### Modelos
      MODEL = feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
                    |  cole_cod_dane+ year,                             ## FEs
                    cluster = ~ cole_cod_dane,                              ## Clustered SEs
                    data = subset(df, df$buffer_km == i 
                    ) )
      
      MODEL_SA = feols( reading_c ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                       |  cole_cod_dane+ year,                             ## FEs
                       cluster = ~ cole_cod_dane,                         ## Clustered SEs
                       data = subset(df, df$buffer_km == i ) )
      
      name_in_enviroment = paste0(  "Score at ", i, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_Reading_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_Reading_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_Reading_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_Reading_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_Reading_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_Reading_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_Reading_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_Reading_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
    
  }
  
}
  
if(5=5){
    ############# Section 1 ############################
    #### TWFE ####
    #################################################
    
    # kilometros = c(100, 200,300,400, 500,1000)
    # reference_time = -1
    
    # tablas = c( 'l500_base_ai','l500_base_10p', 'l500_base_ic', 'l500_base_ent')
    
    # for (j in 1:length(tablas)) {
    #   print(tablas[j])
    #   tabla = tablas[j]
    #   lista_modelos = paste0('TWFE_Math_',  tablas[j])
    #   Sun_Abraham_Modelos =   paste0('SA_Math_',  tablas[j])
    
    #   assign( lista_modelos, list(), envir = .GlobalEnv)
    #   assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    #   for (i in kilometros) {
    
    #     df = get(tabla)
    #     df = df %>% subset(df$time_to_treat >= -10)
    #     MODEL = feols(math_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
    #                   |  cole_cod_dane+ year,                             ## FEs
    #                   cluster = ~ cole_cod_dane,                              ## Clustered SEs
    #                   data = subset(df, df$buffer_km == i ) )
    #     MODEL_SA = feols(reading_c ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
    #                      |  cole_cod_dane+ year,                             ## FEs
    #                      cluster = ~ cole_cod_dane,                         ## Clustered SEs
    #                      data = subset(df, df$buffer_km == i ) )
    
    #     name_in_enviroment = paste0('Math score by ',gsub(tabla, pattern = 'l500_base_', 
    #                                                       replacement = ''), " at ", i, " Meters")
    #     #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
    
    #     if (tablas[j] == 'l500_base_10p') {
    #       TWFE_Math_l500_base_10p[[name_in_enviroment]] <- (MODEL) 
    #       SA_Math_l500_base_10p[[name_in_enviroment]] <- (MODEL_SA) 
    #     } else if (tablas[j] == 'l500_base_ic') {
    #       TWFE_Math_l500_base_ic[[name_in_enviroment]] <- (MODEL)
    #       SA_Math_l500_base_ic[[name_in_enviroment]] <- (MODEL_SA)
    #     } else if (tablas[j] == 'l500_base_ent') {
    #       TWFE_Math_l500_base_ent[[name_in_enviroment]] <- (MODEL)  
    #       SA_Math_l500_base_ent[[name_in_enviroment]] <- (MODEL_SA) 
    #     }  else if (tablas[j] == 'l500_base_ai') {
    #       TWFE_Math_l500_base_ai[[name_in_enviroment]] <- (MODEL)  
    #       SA_Math_l500_base_ai[[name_in_enviroment]] <- (MODEL_SA)  
    #     } else {
    #       message('no esta en las tablas')
    #     }
    #     #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    #   }
    
    # }
    
    # for (j in 1:length(tablas)) {
    #   print(tablas[j])
    #   tabla = tablas[j]
    #   lista_modelos = paste0('TWFE_Reading_',  tablas[j])
    #   Sun_Abraham_Modelos =   paste0('SA_Reading_',  tablas[j])
    
    #   assign( lista_modelos, list(), envir = .GlobalEnv)
    #   assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    #   for (i in kilometros) {
    
    #     df = get(tabla)
    #     df = df %>% subset(df$time_to_treat >= -10)
    #     MODEL = feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
    #                   |  cole_cod_dane+ year,                             ## FEs
    #                   cluster = ~ cole_cod_dane,                              ## Clustered SEs
    #                   data = subset(df, df$buffer_km == i )
    #     )
    #     MODEL_SA = feols(reading_c ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
    #                      |  cole_cod_dane+ year,                             ## FEs
    #                      cluster = ~ cole_cod_dane,                         ## Clustered SEs
    #                      data = subset(df, df$buffer_km == i ) )
    
    
    #     name_in_enviroment = paste0('Math score by ',gsub(tabla, pattern = 'l500_base_', 
    #                                                       replacement = ''), " at ", i, " Meters")
    
    #     if (tablas[j] == 'l500_base_10p') {
    #       TWFE_Reading_l500_base_10p[[name_in_enviroment]] <- (MODEL)
    #       SA_Reading_l500_base_10p[[name_in_enviroment]] <- (MODEL_SA)
    
    #     } else if (tablas[j] == 'l500_base_ic') {
    #       TWFE_Reading_l500_base_ic[[name_in_enviroment]] <- (MODEL)
    #       SA_Reading_l500_base_ic[[name_in_enviroment]] <- (MODEL_SA)
    
    #     } else if (tablas[j] == 'l500_base_ent') {
    #       TWFE_Reading_l500_base_ent[[name_in_enviroment]] <- (MODEL)  
    #       SA_Reading_l500_base_ent[[name_in_enviroment]] <- (MODEL_SA)  
    
    #     }  else if (tablas[j] == 'l500_base_ai') {
    #       TWFE_Reading_l500_base_ai[[name_in_enviroment]] <- (MODEL)  
    #       SA_Reading_l500_base_ai[[name_in_enviroment]] <- (MODEL_SA)  
    
    #     } else {
    #       message('no esta en las tablas')
    #     }
    #     #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    #   }
    
    # }
  }


if(2==2){
  kilometros = c(1000,1500,2000,2500,3000,3500,4000,4500)
  reference_time = -1
  
  tablas = c( 'base_ai','base_10p', 'base_ic')
  
  
  for (j in tablas) {
    print(j)
    tabla = j
    lista_modelos = paste0('TWFE_Participate_saberpro_', j)
    Sun_Abraham_Modelos =   paste0('SA_Participate_saberpro_',  j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    
    for (i in kilometros) {
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df = df %>% subset(df$time_to_treat >= -5 & df$year <= 2013)
      ####### Modelos
      MODEL = feols(finished_uni ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
                    |  cole_cod_dane+ year,                             ## FEs
                    cluster = ~ cole_cod_dane,                              ## Clustered SEs
                    data = subset(df, df$buffer_km == i 
                    ) )
      
      MODEL_SA = feols(finished_uni ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                       |  cole_cod_dane+ year,                             ## FEs
                       cluster = ~ cole_cod_dane,                         ## Clustered SEs
                       data = subset(df, df$buffer_km == i ) )
      
      name_in_enviroment = paste0(  "Score at ", i, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_Participate_saberpro_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_Participate_saberpro_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_Participate_saberpro_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_Participate_saberpro_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_Participate_saberpro_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_Participate_saberpro_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_Participate_saberpro_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_Participate_saberpro_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
    
  }
  tablas = c( 'base_ai','base_10p', 'base_ic', 'base_ent')
  for (j in tablas) {
    print(j)
    tabla = j
    lista_modelos = paste0('TWFE_estu_trabaja_', j)
    Sun_Abraham_Modelos =   paste0('SA_estu_trabaja_',  j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    
    for (i in kilometros) {
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df = df %>% subset(df$time_to_treat >= -8)
      ####### Modelos
      MODEL = feols( (frac_trabaja) ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
                    |  cole_cod_dane+ year,                             ## FEs
                    cluster = ~ cole_cod_dane,                              ## Clustered SEs
                    data = subset(df, df$buffer_km == i 
                    ) )
      
      MODEL_SA = feols( (frac_trabaja) ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                       |  cole_cod_dane+ year,                             ## FEs
                       cluster = ~ cole_cod_dane,                         ## Clustered SEs
                       data = subset(df, df$buffer_km == i ) )
      
      name_in_enviroment = paste0(  "Score at ", i, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_estu_trabaja_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_estu_trabaja_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_estu_trabaja_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_estu_trabaja_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_estu_trabaja_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_estu_trabaja_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_estu_trabaja_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_estu_trabaja_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
    
  }
  
  
}




#########################################################################
### Mix results at 1km
#%%  "10 % advance of construction"
#%%   "Work Initiation Act"
#%% "Work award of the contract"
#%% "completion of road construction"
#########################################################################
#########################################################################
#############################################  "Work award of the contract"
Math_TWFE_ic = TWFE_table('TWFE' , TWFE_Math_base_ic[[1]] )
Math_SA_ic = SA_table(  SA_Math_base_ic[[1]] )

unique(SA_Math_base_10p_table$estimator)
SA_table(  SA_Math_base_ic[[1]]  )
Math_Callaway_ic = Callaway_table(buffer = 1000,  tabla= base_ic , anticipation =0,  yname ='math_c' )
#Math_Gardner_ic = Gardner_table( yname = 'math_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 
Math_ic = do.call("rbind", list(Math_TWFE_ic, Math_SA_ic, Math_Callaway_ic))
png(paste0("graph/Math_ic_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Math_ic , seperate = F,
                  TITULO = 'Math Score\n Work award of the contract' )
dev.off() 
#############################################  "Work Initiation Act"
Math_TWFE_ai = TWFE_table('TWFE' , TWFE_Math_base_ai[[1]] )
Math_SA_ai = SA_table(  SA_Math_base_ai[[1]] )
Math_Callaway_ai = Callaway_table(buffer = 1000,  tabla= base_ai , anticipation =0,  yname ='math_c' )

#Math_Gardner_ai = Gardner_table( yname = 'math_c', df =  subset(base_ai, base_ai$buffer_km == 1000)  ) 
Math_ai =  do.call("rbind", list(Math_TWFE_ai, Math_SA_ai, Math_Callaway_ai))
png(paste0("graph/Math_ai_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Math_ai , seperate = F,
                  TITULO = 'Math Score\n Work Initiation Act' )
dev.off() 
#############################################  "10 % advance of construction"
Math_TWFE_10p = TWFE_table('TWFE' , TWFE_Math_base_10p[[1]] )
Math_SA_10p = SA_table(  SA_Math_base_10p[[1]] )
Math_Callaway_10p = Callaway_table(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='math_c' )
#Math_Gardner_10p = Gardner_table( yname = 'math_c', df = subset(base_10p, base_10p$buffer_km == 1000) ) 
Math_10p  =  do.call("rbind", list(Math_TWFE_10p, Math_SA_10p, Math_Callaway_10p ))

png(paste0("graph/Math_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot( Math_10p , seperate = F , 
                  TITULO = 'Math Score\n 10% advance of construction' )
dev.off() 
############################################# #%% "completion of road construction"
TWFE_ent = TWFE_table('TWFE' , TWFE_Math_base_ent[[1]] )
SA_ent = SA_table(  SA_Math_base_ent[[1]] )
Callaway_ent = Callaway_table(buffer = 1000,  tabla= base_ent , anticipation =0,  yname ='math_c' )
#Math_Gardner_ent = Gardner_table( yname = 'math_c', df = subset(base_ent, base_ent$buffer_km == 1000) ) 
Math_ent  =  do.call("rbind", list(TWFE_ent, Callaway_ent, SA_ent ))
png(paste0("graph/Math_ent_1000m",".png"),  width = 1030, height = 598)
event_study_plot( Math_ent, seperate = F, 
                  TITULO = 'Math Score\n Completion of road construction' )

dev.off()
#########################################################################
# Reading
#########################################################################
as.numeric(1.04e-5)
Reading_TWFE_ic = TWFE_table('TWFE' , TWFE_Reading_base_ic[[1]] )
Reading_SA_ic = SA_table(  SA_Reading_base_ic[[1]] )
Reading_Callaway_ic = Callaway_table(buffer = 1000,  tabla= base_ic , anticipation =0,  yname ='reading_c' )
#Reading_Gardner_ic = Gardner_table( yname = 'reading_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 

Reading_ic = do.call("rbind", list(Reading_TWFE_ic,Reading_SA_ic,  Reading_Callaway_ic))
png(paste0("graph/Reading_ic_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Reading_ic , seperate = F,
                   TITULO = 'Reading Score\n Work award of the contract' )
dev.off() 
#############################################  "Work Initiation Act"
Reading_TWFE_ai = TWFE_table('TWFE' , TWFE_Reading_base_ai[[1]] )
Reading_SA_ai = SA_table(  SA_Reading_base_ai[[1]] )
Reading_Callaway_ai = Callaway_table(buffer = 1000,  tabla= base_ai , anticipation =0,  yname ='reading_c' )
#Reading_Gardner_ai = Gardner_table( yname = 'reading_c', df =  subset(base_ai, base_ai$buffer_km == 1000)  ) 

Reading_ai =  do.call("rbind", list(Reading_TWFE_ai, Reading_SA_ai, Reading_Callaway_ai))
png(paste0("graph/Reading_ai_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Reading_ai , seperate = F,
                   TITULO = 'Reading Score\n Work Initiation Act' )
dev.off() 
#############################################  "10 % advance of construction"
Reading_TWFE_10p = TWFE_table('TWFE' , TWFE_Reading_base_10p[[1]] )
Reading_SA_10p = SA_table(  SA_Reading_base_10p[[1]] )
Reading_SA_10p = subset(Reading_SA_10p, Reading_SA_10p$estimate >= -0.5 &   Reading_SA_10p$estimate <= 0.5  )

Reading_Callaway_10p = Callaway_table(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='reading_c' )
#Reading_Gardner_10p = Gardner_table( yname = 'reading_c', df = subset(base_10p, base_10p$buffer_km == 1000) ) 
Reading_10p  =  do.call("rbind", list(Reading_TWFE_10p, Reading_SA_10p, Reading_Callaway_10p ))

png(paste0("graph/Reading_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot( Reading_10p , seperate = F , 
                  TITULO = 'Reading Score\n 10% advance of construction' )
dev.off() 
############################################# #%% "completion of road construction"
TWFE_ent = TWFE_table('TWFE' , TWFE_Reading_base_ent[[1]] )
SA_ent = SA_table(  SA_Reading_base_ent[[1]] )
Callaway_ent = Callaway_table(buffer = 1000,  tabla= base_ent , anticipation =0,  yname ='reading_c' )
#Reading_Gardner_ent = Gardner_table( yname = 'reading_c', df = subset(base_ent, base_ent$buffer_km == 1000) ) 
Reading_ent  =  do.call("rbind", list(TWFE_ent, Callaway_ent, SA_ent ))
png(paste0("graph/Reading_ent_1000m",".png"),  width = 1030, height = 598)
event_study_plot( Reading_ent, seperate = F, 
                  TITULO = 'Reading Score\n Completion of road construction' )

dev.off()


######################################################################### 
a = data.frame(summary(TWFE_result) )
lista = drop_character(a)
lista
a$term = drop_character (row.names(a) )
#######################################################33
# analysis by treatment date Acta de inicio
####################### grafical analysis ###############################################
data.frame(names(Math_base_10p)[1:8])
setwd('C:/Users/USER/Desktop/DID roads/Data')
 
########################################
############   short term    ########### 
getwd()
# safe_results_did (Result = ls(pattern = "TWFE_Math_l500_base*") ,
#                   academic_subject = 'Math' , 
#                   distancia = '_short' )

# safe_results_did (Result = ls(pattern = "SA_Math_l500_base*") ,
#                   academic_subject = 'Math' , 
#                   distancia = '_short' ,interaction="Sun & Abraham")

# ############      ############ 
# ls(pattern = "TWFE_Reading_l500_base" )


# safe_results_did (Result = ls(pattern = "TWFE_Reading_l500_base*") ,
#                   academic_subject = 'Reading Literacy' , 
#                   distancia = '_short' ,
#                   interaction="TWFE")


# safe_results_did (Result = ls(pattern = "SA_Reading_l500_base*") ,
#                   academic_subject = 'Reading Literacy' , 
#                   distancia = '_short' ,
#                   interaction="Sun & Abraham")

########################################
############   Long term    ########### 
safe_results_did (Result = ls(pattern = "TWFE_Math_base*") ,
                  academic_subject = 'Math' , 
                  distancia = '_long' )

safe_results_did (Result = ls(pattern = "SA_Math_base*") ,
                  academic_subject = 'Math' , 
                  distancia = '_long' ,
                  interaction="Sun & Abraham")

############      ############ 
safe_results_did (Result = ls(pattern = "TWFE_Reading_base*") ,
                  academic_subject = 'Literacy' , 
                  distancia = '_long' )

safe_results_did (Result = ls(pattern = "SA_Reading_base*") ,
                  academic_subject = 'Literacy' , 
                  distancia = '_long'  ,
                  interaction="Sun & Abraham")
 
safe_results_did (Result = ls(pattern = "SA_estu_trabaja_base*") ,
                  academic_subject = 'Student labor force participation,' , 
                  distancia = '_long'  ,
                  interaction="Sun & Abraham")

safe_results_did (Result = ls(pattern = "SA_Participate_saberpro_base*") ,
                  academic_subject = 'Participate and end in a universitary program,' , 
                  distancia = '_long'  ,
                  interaction="Sun & Abraham")






############# Calculo tabular ATT  math_c
ATT_Math_base_ai = table_result_ATT(buffer = 1000,  tabla= base_ai , anticipation =0,  yname ='math_c' )
ATT_Math_base_ai
ATT_Math_base_ic = table_result_ATT(buffer = 1000,  tabla= base_ic , anticipation =0,  yname ='math_c' )
ATT_Math_base_ic
ATT_Math_base_ent = table_result_ATT(buffer = 1000,  tabla= base_ent , anticipation =0,  yname ='math_c' )
ATT_Math_base_ent
ATT_Math_base_10p = table_result_ATT(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='math_c' )



 
table_result_ATT(buffer = 1500,  tabla= base_10p , anticipation =0,  yname ='math_c' )
table_result_ATT(buffer = 1500,  tabla= base_ai , anticipation =0,  yname ='math_c' )
table_result_ATT(buffer = 1500,  tabla= base_ic , anticipation =0,  yname ='math_c' )
table_result_ATT(buffer = 1500,  tabla= base_ent , anticipation =0,  yname ='math_c' )


############# Calculo tabular ATT  Reading
ATT_Reading_base_ai = table_result_ATT(buffer = 1000,  tabla= base_ai , anticipation =0,  yname ='reading_c' )

ATT_Reading_base_ai
ATT_Reading_base_ic = table_result_ATT(buffer = 1000,  tabla= base_ic , anticipation =0,  yname ='reading_c' )
ATT_Reading_base_ic
ATT_Reading_base_ent = table_result_ATT(buffer = 1000,  tabla= base_ent , anticipation =0,  yname ='reading_c' )
ATT_Reading_base_ent
ATT_Reading_base_10p = table_result_ATT(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='reading_c' )


Callaway_model(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='reading_c' )
Callaway_model(buffer = 1000,  tabla= base_ai , anticipation =0,  yname ='reading_c' )

Callaway_model(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='math_c' )
Callaway_model(buffer = 1000,  tabla= base_ai , anticipation =0,  yname ='math_c' )
 #rm(list = ls()[grepl("ATT_r", ls())]) 


table(base_ic$reading_c)
table(base_ai$year_treated_att)
table(base_10p$year_treated_att)
library(stringr)
#################################################################################################
############## Cross Validation ATT Callawy
#################################################################################################
library(caret)
set.seed(123)
base_10p_1000 =  subset(base_10p, base_10p$buffer_km ==1000)
random_sample <- createDataPartition( base_10p_1000[[7]], p = 0.5, list = FALSE)
library(modelr)
 
cv  <- crossv_kfold(base_10p_1000, k = 5, id = "cole_cod_dane")
nrow(base_10p_1000)
X1 = base_10p_1000[cv$train$`1`$idx, ]
X2 = base_10p_1000[cv$train$`2`$idx, ]
X3 = base_10p_1000[cv$train$`3`$idx, ]
X4 = base_10p_1000[cv$train$`4`$idx, ]
X5 = base_10p_1000[cv$train$`5`$idx, ]

XV_10p_Reading = Callaway_model(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='reading_c' ) 
XV_10p_Reading_1 =Callaway_model(buffer = 1000,  tabla= X1 , anticipation =0,  yname ='reading_c' ) 
XV_10p_Reading_2 =Callaway_model(buffer = 1000,  tabla= X2 , anticipation =0,  yname ='reading_c' ) 
XV_10p_Reading_3 =Callaway_model(buffer = 1000,  tabla= X3 , anticipation =0,  yname ='reading_c' ) 
XV_10p_Reading_4 =Callaway_model(buffer = 1000,  tabla= X4 , anticipation =0,  yname ='reading_c' ) 
XV_10p_Reading_5 =Callaway_model(buffer = 1000,  tabla= X5 , anticipation =0,  yname ='reading_c' ) 

XV_10p_Math = Callaway_model(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='math_c' ) 
XV_10p_Math_1 =Callaway_model(buffer = 1000,  tabla= X1 , anticipation =0,  yname ='math_c' ) 
XV_10p_Math_2 =Callaway_model(buffer = 1000,  tabla= X2 , anticipation =0,  yname ='math_c' ) 
XV_10p_Math_3 =Callaway_model(buffer = 1000,  tabla= X3 , anticipation =0,  yname ='math_c' ) 
XV_10p_Math_4 =Callaway_model(buffer = 1000,  tabla= X4 , anticipation =0,  yname ='math_c' ) 
XV_10p_Math_5 =Callaway_model(buffer = 1000,  tabla= X5 , anticipation =0,  yname ='math_c' ) 
significancia_un_valor(0.0236               , 0.0075        )
 
###############################################################################################
                        ##################################################
                        ################## Math ##########################
                        #################################################
subject = "Math"
rm(summary_1k)
############################### "10 % advance of construction" ############################### 
summary_1k = data.frame()
### RESULTADOS POR TWFE AND SA   IN LONG DISTANCE
pvaluebuffer(TWFE_Math_base_10p, "10 % advance of construction",subject,reference_time) 
pvaluebuffer(SA_Math_base_10p, "10 % advance of construction",subject,reference_time)
## Callaway, Brantly and Pedro H.C. Sant'Anna. = CBPS
summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'math_10p', 
                                         TWFE_result = TWFE_Math_base_10p[[1]],
                                         SA_result =SA_Math_base_10p[[1]], 
                                         ATT_result = ATT_Math_base_10p)
                  )

### RESULTADOS POR TWFE AND SA  IN SHORT DISTANCE
#pvaluebuffer(TWFE_Math_l500_base_10p,"10 % advance of construction",subject,reference_time) 
#pvaluebuffer(SA_Math_l500_base_10p,"10 % advance of construction",subject,reference_time) 
#table_result_DiD(Buffer = '100 Metros', TWFE_result = TWFE_Math_l500_base_10p[1], SA_result =SA_Math_l500_base_10p[1])

 
###############################  "Work Initiation Act" 
### RESULTADOS POR TWFE AND SA IN LONG DISTANCE
pvaluebuffer(SA_Math_base_ai,     "Work Initiation Act" ,subject,reference_time)
pvaluebuffer(TWFE_Math_base_ai,  "Work Initiation Act" ,subject,reference_time) 
## Callaway, Brantly and Pedro H.C. Sant'Anna. = CBPS
summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'math_ai', 
                                         TWFE_result = TWFE_Math_base_ai[1],
                                         SA_result =SA_Math_base_ai[1], 
                                         ATT_result = ATT_Math_base_ai)
                  )
### RESULTADOS POR TWFE AND SA  IN SHORT DISTANCE
pvaluebuffer(TWFE_Math_l500_base_ai, " Work Initiation Act" ,subject,reference_time)
pvaluebuffer(SA_Math_l500_base_ai, " Work Initiation Act" ,subject,reference_time)
table_result_DiD(Buffer = '100M', 
                 TWFE_Math_l500_base_ai[1],
                 SA_Math_l500_base_ai[1])

###############################  "Work award of the contract"  
pvaluebuffer(TWFE_Math_base_ic, "Work award of the contract" ,subject,reference_time)
pvaluebuffer(SA_Math_base_ic, "Work award of the contract" ,subject,reference_time)

summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'math_ic', 
                                         TWFE_result = TWFE_Math_base_ic[1],
                                         SA_result =SA_Math_base_ic[1], 
                                         ATT_result = ATT_Math_base_ic)
)

###############################  "completion of road construction" 
pvaluebuffer(TWFE_Math_base_ent, "completion of road construction" ,subject,reference_time)
pvaluebuffer(SA_Math_base_ent, "completion of road construction" ,subject,reference_time)
table_result_TWFE_SA_C(Buffer = '1000 Metros Matematicas', 
                       TWFE_result = TWFE_Math_base_ent[1],
                       SA_result =SA_Math_base_ent[1], 
                       ATT_result = ATT_Math_base_ent)

summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'math_ent', 
                                         TWFE_result = TWFE_Math_base_ent[1],
                                         SA_result =SA_Math_base_ent[1], 
                                         ATT_result = ATT_Math_base_ent)
)
###############################################################################################
                      ##################################################
                      ################## Reading Literacy ############# 
                      #################################################
###############################################################################################
subject = "Reading Literacy"

############################### "10 % advance of construction"  
### RESULTADOS POR TWFE AND SA   IN LONG DISTANCE
pvaluebuffer(TWFE_Reading_base_10p, "10 % advance of construction",subject,reference_time) 
pvaluebuffer(SA_Reading_base_10p, "10 % advance of construction",subject,reference_time)
## Callaway, Brantly and Pedro H.C. Sant'Anna. = CBPS
summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'Reading_10p', 
                                         TWFE_result = TWFE_Reading_base_10p[1],
                                         SA_result =SA_Reading_base_10p[1], 
                                         ATT_result = ATT_Reading_base_10p)
)
 

### RESULTADOS POR TWFE AND SA  IN SHORT DISTANCE
pvaluebuffer(TWFE_Reading_l500_base_10p,"10 % advance of construction",subject,reference_time) 
pvaluebuffer(SA_Reading_l500_base_10p,"10 % advance of construction",subject,reference_time) 
table_result_DiD(Buffer = '100 Metros', TWFE_result = TWFE_reading_l500_base_10p[1], SA_result =SA_reading_l500_base_10p[1])


###############################  "Work Initiation Act" 
### RESULTADOS POR TWFE AND SA IN LONG DISTANCE
pvaluebuffer(SA_Reading_base_ai,     "Work Initiation Act" ,subject,reference_time)
pvaluebuffer(TWFE_Reading_base_ai,  "Work Initiation Act" ,subject,reference_time) 
## Callaway, Brantly and Pedro H.C. Sant'Anna. = CBPS
summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'Reading_a1', 
                                         TWFE_result = TWFE_Reading_base_ai[1],
                                         SA_result =SA_Reading_base_ai[1], 
                                         ATT_result = ATT_Reading_base_ai)
)



### RESULTADOS POR TWFE AND SA  IN SHORT DISTANCE
pvaluebuffer(TWFE_Rreading_l500_base_ai, " Work Initiation Act" ,subject,reference_time)
pvaluebuffer(SA_Reading_l500_base_ai, " Work Initiation Act" ,subject,reference_time)
table_result_DiD(Buffer = '100M', 
                 TWFE_reading_l500_base_ai[1],
                 SA_reading_l500_base_ai[1])

###############################  "Work award of the contract"  
pvaluebuffer(TWFE_Reading_base_ic, "Work award of the contract" ,subject,reference_time)
pvaluebuffer(SA_Reading_base_ic, "Work award of the contract" ,subject,reference_time)


summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'Reading_ic', 
                                         TWFE_result = TWFE_Reading_base_ic[1],
                                         SA_result =SA_Reading_base_ic[1], 
                                         ATT_result = ATT_Reading_base_ic)
)

###############################  "completion of road construction"  
pvaluebuffer(TWFE_Reading_base_ent, "completion of road construction" ,subject,reference_time)
pvaluebuffer(SA_Reading_base_ent, "completion of road construction" ,subject,reference_time)
summary_1k =rbind(summary_1k, 
                  table_result_TWFE_SA_C(Buffer = 'Reading_ent', 
                                         TWFE_result = TWFE_Reading_base_ent[1],
                                         SA_result =SA_Reading_base_ent[1], 
                                         ATT_result = ATT_Reading_base_ent) 
)

###############################################################################################
##################################################
################## otros outcomes ############# 
#################################################
###############################################################################################



sql = "
SELECT DISTINCT    TREATED_CONS_STARTED, --TREATED_ROAD_FINISHED
    AVG(median_norm_math_c) median_norm_math_c,
    AVG(median_norm_reading_c)  median_norm_reading_c, 
    COLE_COD_ICFES, Fecha_entrega,
     -- Fecha_inicio_consecion, 
    ANIO , buffer_km,  COLE_MCPIO_UBICACION
--       CASE 
--           WHEN TREATED_CONS_STARTED = 1  
--               THEN  SAFE_CAST(ANIO AS INT64) - SAFE_CAST(Fecha_entrega AS INT64) 
--           ELSE 0 END ,
--       ifnull(Fecha_entrega, 0) > SAFE_CAST(ANIO AS INT64)
    
    FROM base
    WHERE 
    (    case when Fecha_entrega =2016 then 1
        when  Fecha_entrega =2021 then 1
        when  Fecha_entrega is null then 0 else 0 end) = 0
    GROUP BY 1,4,5,6,7,8
        
"
base_2018= sqldf(sql)
base_2018$treat  = as.factor(ifelse(is.na(base_2018$Fecha_entrega), 0,1)) # as.factor(base_2018$TREATED_CONS_STARTED)
base_2018$year  =  base_2018$ANIO
base_2018$math_c = base_2018$median_norm_math_c
base_2018$reading_c = base_2018$median_norm_reading_c

DiD = base_2018[,c('math_c','treat','year', 'reading_c', 'COLE_COD_ICFES')]

DiD <- DiD %>% 
  mutate(after = year >= 2018) %>%
  mutate(treatafter = after*as.numeric(treat) )
reg_m<-lm(math_c ~ treat+treatafter+after   , data = DiD)
reg_r<-lm(reading_c ~ treat+treatafter+after, data = DiD)
summary(reg_m)
summary(reg_r)

reg_m2<-lm(math_c ~ treat+treatafter+after + COLE_COD_ICFES + factor(year) , data = DiD)
reg_r2<-lm(reading_c ~ treat+treatafter+after +COLE_COD_ICFES + factor(year), data = DiD)
(summary(reg_m2)$coefficients)[1:4,]
(summary(reg_r2)$coefficients)[1:4,]


## Efecto del tratamiento
ATT_m = (reg_m$coefficients)[3] /(reg_m$coefficients)[1] 
ATT_r = (reg_r$coefficients)[3] /(reg_r$coefficients)[1] 
ATT_m
ATT_r
 
#### la diferencia del grupo tratado fue de 0.013842
DID_AVG = sqldf("
      SELECT 
      AVG(math_c)*100 math_c,
      year, treat
      FROM DiD
      GROUP BY 2,3
      ")
mt <- ggplot(DID_AVG,aes(x=year, y=math_c, color = (treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2018,lty=15) +
  geom_vline(xintercept=2014,lty=5) +
  labs(title="math results and Time", x="Year", y="math results Rate")
mt

DID_AVG = sqldf("
      SELECT 
      AVG(reading_c)*100 READING_TEST_RESULT,
      year, treat
      FROM DiD
      GROUP BY 2,3
      ")


mt <- ggplot(DID_AVG,aes(x=year, y=READING_TEST_RESULT, color = (treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2018,lty=15) +
  geom_vline(xintercept=2014,lty=5) +
  labs(title="reading results and Time", x="Year", y="reading results Rate")
mt
#####################################################
# Dif and dif con fecha de consecion

sql = "
SELECT DISTINCT    
    ---TREATED_CONS_STARTED, 
    TREATED_ROAD_FINISHED,
    median_norm_math_c, median_norm_reading_c, 
    COLE_COD_ICFES,
    ----Fecha_entrega,
     (    case when Fecha_inicio_consecion =2002 then null
        when  Fecha_inicio_consecion =2006 then null
         when  Fecha_inicio_consecion =2010 then null
         when  Fecha_inicio_consecion =2020 then null else 
         Fecha_inicio_consecion  end) 
     
      Fecha_entrega, 
    ANIO , buffer_km,  COLE_MCPIO_UBICACION

    FROM base
     WHERE 
    (    case when Fecha_inicio_consecion =2002 then 1
        when  Fecha_inicio_consecion =2006 then 1
         when  Fecha_inicio_consecion =2010 then 1
         when  Fecha_inicio_consecion =2020 then 0 else 0 end) = 0
"

base_2018_fini= sqldf(sql)
summary(factor(base_2018_fini$Fecha_entrega) )
base_2018_fini$treat  = base_2018_fini$TREATED_ROAD_FINISHED
base_2018_fini$year  =  base_2018_fini$ANIO
base_2018_fini$math_c = base_2018_fini$median_norm_math_c
base_2018_fini$reading_c = base_2018_fini$median_norm_reading_c
glimpse(base_2018_fini)
DiD_finished = base_2018_fini[ ,c('math_c','treat','year', 'reading_c')]

DiD_finished <- DiD_finished %>% 
  mutate(after = year >= 2018) %>%
  mutate(treatafter = after*treat)

reg_DiD_finished<-lm(reading_c ~ treat+treatafter+after, data = DiD_finished)
summary(reg_DiD_finished)

## Efecto del tratamiento
ATT = (reg$coefficients)[3] /(reg$coefficients)[1] 
ATT

DiD2 = base_2018[,c('math_c','treat','year', 'COLE_COD_ICFES')]


DiD2 <- DiD2 %>% 
  mutate(after = year >= 2018) %>%
  mutate(treatafter = after*treat)

mt <- ggplot(DiD2,aes(x=year, y=math_c, color = factor(treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2018,lty=15) +
  labs(title="math results and Time", x="Year", y="math results Rate")
mt
reg2<-lm(math_c ~ treat+treatafter+after + factor(COLE_COD_ICFES) + factor(year), data = DiD2)
(summary(reg2)$coefficients)[1:4,]

(reg2$coefficients)[3] /(reg2$coefficients)[1] 
#### la diferencia del grupo tratado fue de 0.013842
DID_AVG = sqldf("
      SELECT 
      AVG(math_c)*100 MATH_TEST_RESULT,
      year, treat
      FROM DiD_finished
      GROUP BY 2,3
      ")
mt <- ggplot(DID_AVG,aes(x=year, y=MATH_TEST_RESULT, color = (treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2014,lty=15) +
  labs(title="math results and Time", x="Year", y="math results Rate")
mt
DID_AVG$treat

iplot(
  list(
  feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
        | year # COLE_COD_ICFES+ year
        ,                             ## FEs
        #  cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
        data = base1
  )
  
  ,feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
         #|  year # COLE_COD_ICFES+ year
         ,                             ## FEs
            cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
         data = base1
  )
  ,feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
         |  COLE_COD_ICFES+ year
         ,                             ## FEs
         # cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
         data = base1
  )
  ,feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
         |  COLE_COD_ICFES+ year,                             ## FEs
         cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
         data = base1
  )
  ), 
sep = 0.1, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Math test')

legend(
  "topleft", col = c(1, 2,3,4,5), pch = c(20, 17,18,19,21), 
  legend = c(" sin ef y sin clu", "sin ef", "sin clu", "todo", "todo interact"))



#############################################################33
###  Callaway and Sant’Anna (2021), “Difference-in-Differences with Multiple Time Periods”.
###  https://doi.org/10.1016/j.jeconom.2020.12.001

#install.packages("did")
library(did)

table(base_10p$att)

tabla  = base_10p 
tabla$idname =  as.numeric(tabla$cole_cod_dane)
es <- aggte( ### getting the att stimation for a DiD
              att_gt(yname = "math_c",
                   gname = "year_treated_att",
                   idname = "idname",
                   tname = "year",anticipation = 0,
                   data = subset(tabla, tabla$buffer_km == 1000),
                   allow_unbalanced_panel= T, panel = F),
              ### getting the att stimation for a DiD Dynamic
            type = "dynamic",
            na.rm = TRUE)
Buffer = '1000M'
subject = "Math"





library(dotwhisker)

left_join(table_result_DiD(Buffer = '1000M', 
                           TWFE_result = TWFE_Math_base_10p[1],
                           SA_result = SA_Math_base_10p[1] ), 
          table_result_ATT( ATT_result = es  ),
          by = "Periodo"
          
          ) 
 
table_result_DiD(Buffer = '1000M', 
                 TWFE_result = TWFE_Math_base_10p[1],
                 SA_result = SA_Math_base_10p[1] )

ggdid(es)

MODEL = feols(math_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
              |  cole_cod_dane+ year,                             ## FEs
              cluster = ~ cole_cod_dane,                              ## Clustered SEs
              data = subset(df, df$buffer_km == i ) )
MODEL_SA = feols(math_c ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                 |  cole_cod_dane+ year,                             ## FEs
                 cluster = ~ cole_cod_dane,                         ## Clustered SEs
                 data = subset(df, df$buffer_km == i ) )
iplot(math_twfe_ai)
