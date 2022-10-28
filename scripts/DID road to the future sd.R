# Set your query
################################################################
## si tab no funion  "Alt + Shift + [".
options(scipen=111)
global_path = "C:/Users/USER/Desktop/DID roads/ROAD_TO_THE_FUTURE/"
source(paste0(global_path ,'scripts/Reading_data.R'))


############# Section 1 ############################
#### TWFE ####
###  TWFE for Math
#################################################
#### Train 1
### Median of schools who studies into the fist buffer (1 km)
### Treated group according road operation input 

##### time 0
### 
###### Ultimas transformaciones
 
if (1==1){
  
  base_ic$finished_uni    =     base_ic$Participate_saberpro_sd 
  base_50p$finished_uni   =    base_50p$Participate_saberpro_sd 
  base_10p$finished_uni   =    base_10p$Participate_saberpro_sd  
  base_ai$finished_uni    =    base_ai$Participate_saberpro_sd  
  base_ent$finished_uni   =    base_ent$Participate_saberpro_sd  
 
}
 
  

############# Section 1 ############################
#### TWFE ####
#################################################
if(1==1){
  kilometros = c(1000  ,1500,2000,2500,3000,3500,4000,4500)
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
      print( sprintf(" The model for the treatment time related with:  %s, for the distance: %s is running" , j , i) )
      df = get(tabla)
      
      ######################################## 
      ### TWFE
      MODEL = feols( math_c_sd ~ i(time_to_treat, treat_, ref = reference_time)    ## Our key interaction: time × treatment status
                     | id_name + year   ,                             ## FEs
                     cluster = ~ id_name ,                              ## Clustered SEs
                     data = subset(df, df$buffer_km == i )
      )
      
      ########################################
      ### Sun And Abraham
      table(df$DIVIPOLA_MUN)
      
      MODEL_SA = feols( math_c_sd ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1)   )    ## The only thing that's changed
                        | id_name + year ,                             ## FEs
                        cluster = ~ id_name ,                         ## Clustered SEs
                        data = subset(df, df$buffer_km == i ) )
      summary(MODEL_SA)
      ######################################## 
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
      print( sprintf(" The model for the treatment time related with:  %s, for the distance: %s is running" , j , i) )
      df = get(tabla)
      
      ######################################## 
      ### TWFE
      MODEL = feols( reading_c_sd ~ i(time_to_treat, treat_, ref = reference_time)    ## Our key interaction: time × treatment status
                     | id_name + year,                             ## FEs
                     cluster = ~ id_name ,                             ## Clustered SEs
                     data = subset(df, df$buffer_km == i )
      )
      ########################################
      ### Sun And Abraham
      
      MODEL_SA = feols( reading_c_sd ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) )    ## The only thing that's changed
                        | id_name + year,                           ## FEs
                        cluster = ~ id_name ,                          ## Clustered SEs
                        data = subset(df, df$buffer_km == i ) )
      ######################################## 
      name_in_enviroment = paste0("Score at ", i, " Meters")
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
 

 
fixest::iplot( 
  list(SA_Math_base_ent[["Score at 1000 Meters"]],
       SA_Math_base_ai[["Score at 1000 Meters"]],
       SA_Math_base_10p[["Score at 1000 Meters"]],
       SA_Math_base_ic[["Score at 1000 Meters"]])
)
fixest::iplot( 
  list(SA_Reading_base_10p[["Score at 1000 Meters"]],
       SA_Reading_base_ai[["Score at 1000 Meters"]],
       SA_Reading_base_10p[["Score at 1000 Meters"]],
       SA_Reading_base_ic[["Score at 1000 Meters"]]) 
)


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
      #df$time_to_treat = ifelse(df$time_to_treat<= -8 , -8, df$time_to_treat)
      #df$time_to_treat = ifelse(df$time_to_treat >= 8 , 8, df$time_to_treat)
      
      df = df %>% subset( df$year <= 2013)  ## df$time_to_treat >= -5 &
      ####### Modelos
      MODEL = feols(finished_uni ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                    | id_name + year,                                 ## FEs
                    cluster = ~ id_name ,                        ## Clustered SEs
                    data = subset(df, df$buffer_km == i 
                    ) )
      
      MODEL_SA = feols(finished_uni ~ sunab(year_treated_sa, year , ref.p = c(.F + -2:2, -1)  ) #+ ## The only thing that's changed
                       | id_name + year,                                 ## FEs
                       cluster = ~ id_name ,                       ## Clustered SEs
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
      #df$time_to_treat = ifelse(df$time_to_treat<= -8 , -8, df$time_to_treat)
      #df$time_to_treat = ifelse(df$time_to_treat >= 8 , 8, df$time_to_treat)
      ####### Modelos
      MODEL = feols( (frac_trabaja_sd) ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                     |id_name + year,                                 ## FEs
                     cluster = ~ id_name ,                               ## Clustered SEs
                     data = subset(df, df$buffer_km == i 
                     ) )
      
      MODEL_SA = feols( (frac_trabaja_sd) ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) ) #+ ## The only thing that's changed
                        | id_name + year,                                ## FEs
                        cluster = ~ id_name ,                     ## Clustered SEs
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

if(3==3){
  
  tablas = c( 'base_ai','base_10p', 'base_ic', 'base_ent')
  for (j in tablas) {
    print(j)
    tabla = j
    lista_modelos = paste0('TWFE_total_profes_', j)
    Sun_Abraham_Modelos =   paste0('SA_total_profes_',  j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    
    for (i in kilometros) {
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df <- subset(df, df$year>= 2012)
      tryCatch( {
      #df$time_to_treat = ifelse(df$time_to_treat<= -8 , -8, df$time_to_treat)
      #df$time_to_treat = ifelse(df$time_to_treat >= 8 , 8, df$time_to_treat)
      ####### Modelos
      MODEL = feols( (TOTPROF_100kPROF_COL) ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                     | id_name + year,                                ## FEs
                     cluster = ~ id_name ,                               ## Clustered SEs
                     data = subset(df, df$buffer_km == i 
                     ) )
      
      MODEL_SA = feols( (TOTPROF_100kPROF_COL) ~ sunab(year_treated_sa, year , ref.p = c(.F + -2:2, -1) ) #+ ## The only thing that's changed
                        | id_name + year,                              ## FEs
                        cluster = ~ id_name ,                     ## Clustered SEs
                        data = subset(df, df$buffer_km == i ) )
      
      name_in_enviroment = paste0(  "Score at ", i, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_total_profes_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_total_profes_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_total_profes_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_total_profes_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_total_profes_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_total_profes_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_total_profes_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_total_profes_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
      )
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
    
  }
  
  
  
  for (j in tablas) {
    print(j)
    tabla = j
    lista_modelos = paste0('TWFE_total_profes_preg_', j)
    Sun_Abraham_Modelos =   paste0('SA_total_profes_preg_',  j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    
    for (i in kilometros) {
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df <- subset(df, df$year>= 2012)
      tryCatch( {
      #df$time_to_treat = ifelse(df$time_to_treat<= -8 , -8, df$time_to_treat)
      #df$time_to_treat = ifelse(df$time_to_treat >= 8 , 8, df$time_to_treat)
      ####### Modelos
      MODEL = feols( (TOTPRE_100kPROF_COL) ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                     | id_name + year,                               ## FEs
                     cluster = ~ id_name ,                               ## Clustered SEs
                     data = subset(df, df$buffer_km == i 
                     ) )
      
      MODEL_SA = feols( (TOTPRE_100kPROF_COL) ~ sunab(year_treated_sa, year , ref.p = c(.F + -2:2, -1) ) #+ ## The only thing that's changed
                        | id_name + year,                            ## FEs
                        cluster = ~ id_name ,                     ## Clustered SEs
                        data = subset(df, df$buffer_km == i ) )
      
      name_in_enviroment = paste0(  "Score at ", i, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_total_profes_preg_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_total_profes_preg_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_total_profes_preg_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_total_profes_preg_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_total_profes_preg_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_total_profes_preg_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_total_profes_preg_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_total_profes_preg_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
      )
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
    
  }
}

fixest::iplot( 
  list(SA_total_profes_preg_base_10p[["Score at 1000 Meters"]] 
         ,
         SA_total_profes_preg_base_ai[["Score at 1000 Meters"]] 
       # SA_total_profes_preg_base_ent[["Score at 1000 Meters"]] , 
       # SA_total_profes_preg_base_ic[["Score at 1000 Meters"]]
       ) 
)

fixest::iplot( 
  list(SA_total_profes_base_10p[["Score at 1000 Meters"]] 
       # ,
       # SA_total_profes_base_ai[["Score at 1000 Meters"]],
       # SA_total_profes_base_ent[["Score at 1000 Meters"]] , 
       # SA_total_profes_base_ic[["Score at 1000 Meters"]]
  ) 
)

library(stringr)
#################################################################################################
############## Cross Validation ATT Callawy
#################################################################################################
# library(caret)
# set.seed(123)
# base_10p_1000 =  subset(base_10p, base_10p$buffer_km ==1000)
# random_sample <- createDataPartition( base_10p_1000[[7]], p = 0.5, list = FALSE)
# library(modelr)
# base_10p_1000$reading_c_sd
# cv  <- crossv_kfold(base_10p_1000, k = 5, id = "cole_cod_dane")
# nrow(base_10p_1000)
# X1 = base_10p_1000[cv$train$`1`$idx, ]
# X2 = base_10p_1000[cv$train$`2`$idx, ]
# X3 = base_10p_1000[cv$train$`3`$idx, ]
# X4 = base_10p_1000[cv$train$`4`$idx, ]
# X5 = base_10p_1000[cv$train$`5`$idx, ]
# 
# XV_10p_Reading = Callaway_model(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='reading_c_sd' ) 
# XV_10p_Reading_1 =Callaway_model(buffer = 1000,  tabla= X1 , anticipation =0,  yname ='reading_c_sd' ) 
# XV_10p_Reading_2 =Callaway_model(buffer = 1000,  tabla= X2 , anticipation =0,  yname ='reading_c_sd' ) 
# XV_10p_Reading_3 =Callaway_model(buffer = 1000,  tabla= X3 , anticipation =0,  yname ='reading_c_sd' ) 
# XV_10p_Reading_4 =Callaway_model(buffer = 1000,  tabla= X4 , anticipation =0,  yname ='reading_c_sd' ) 
# XV_10p_Reading_5 =Callaway_model(buffer = 1000,  tabla= X5 , anticipation =0,  yname ='reading_c_sd' ) 
# 
# XV_10p_Math = Callaway_model(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='math_c_sd' ) 
# XV_10p_Math_1 =Callaway_model(buffer = 1000,  tabla= X1 , anticipation =0,  yname ='math_c_sd' ) 
# XV_10p_Math_2 =Callaway_model(buffer = 1000,  tabla= X2 , anticipation =0,  yname ='math_c_sd' ) 
# XV_10p_Math_3 =Callaway_model(buffer = 1000,  tabla= X3 , anticipation =0,  yname ='math_c_sd' ) 
# XV_10p_Math_4 =Callaway_model(buffer = 1000,  tabla= X4 , anticipation =0,  yname ='math_c_sd' ) 
# XV_10p_Math_5 =Callaway_model(buffer = 1000,  tabla= X5 , anticipation =0,  yname ='math_c_sd' ) 
# significancia_un_valor(0.0236               , 0.0075        )
# 
# #######################################################################
# base_10p$reading_c_sd
# out = att_gt(yname = 'math_c_sd',
#        gname = "year_treated_att",
#        idname = "id_name",
#        tname = "year",anticipation = -1,
#        data = subset(base_10p, base_10p$buffer_km == 1000) , 
#        allow_unbalanced_panel= T, panel = F ,   cband = F,
# )
# ggdid(aggte(out, type = "dynamic", na.rm = TRUE, alp = 0.05 , bstrap=F, balance_e = 5))
# 
# ggdid(out)
# aggte(out, type = "simple", na.rm = TRUE)
# aggte(out, type = "dynamic", na.rm = TRUE)
# ggdid(aggte(out, type = "dynamic", na.rm = TRUE))
# 
# 
# ?aggte
