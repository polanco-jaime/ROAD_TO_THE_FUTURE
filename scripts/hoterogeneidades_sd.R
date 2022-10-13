naturaleza = c("PUBL", "PRIV" , "") # unique(base_10p$cole_naturaleza)
reference_time = -1

tablas = c( 'base_ai','base_10p', 'base_ic', 'base_ent')
############### Naturaleza




if (1==1){
  for (j in tablas) {
    print(j)
    tabla = j
    
    Sun_Abraham_Modelos =   paste0('SA_reading_naturaleza_',  j)
    lista_modelos = paste0('TWFE_reading_naturaleza_', j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    
    for (i in naturaleza) {
      
      if (i== "PUBL" ) {
        m = "PRIV"
      } else if (i == "PRIV" ) {
        m = "PUBL"
      } else {
        m = "All sample"
      }
      
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      #df = df %>% subset(df$time_to_treat >= -8)
      ####### Modelos
      MODEL = feols(reading_c_sd ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                    | id_name + year,                               ## FEs
                    cluster = ~ id_name + DIVIPOLA_MUN,                                        ## Clustered SEs
                    data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
      
      MODEL_SA = feols( reading_c_sd ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                        | id_name + year,                               ## FEs
                        cluster = ~ id_name + DIVIPOLA_MUN,                               ## Clustered SEs
                        data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
 
      name_in_enviroment = paste0(  "Score at ", m, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_reading_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_reading_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_reading_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_reading_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_reading_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_reading_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_reading_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_reading_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
    
    
    
  }
  

  
  for (j in tablas) {
    print(j)
    tabla = j
    
    Sun_Abraham_Modelos =   paste0('SA_math_naturaleza_',  j)
    lista_modelos = paste0('TWFE_math_naturaleza_', j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    
    for (i in naturaleza) {
      if (i== "PUBL" ) {
        m = "PRIV"
      } else if (i == "PRIV" ) {
        m = "PUBL"
      } else {
        m = "All sample"
      }
      
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df = df %>% subset(df$time_to_treat >= -8)
      ####### Modelos
      MODEL = feols(math_c_sd ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                    | id_name + year,                               ## FEs
                    cluster = ~ id_name + DIVIPOLA_MUN,                                    ## Clustered SEs
                    data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
      
      MODEL_SA = feols(  math_c_sd ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                         | id_name + year,                               ## FEs
                         cluster = ~ id_name + DIVIPOLA_MUN,                                ## Clustered SEs
                        data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
      
      name_in_enviroment = paste0(  "Score at ", m, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_math_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_math_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_math_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_math_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_math_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_math_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_math_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_math_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
  }
  #####################################
  
  for (j in tablas) {
    print(j)
    tabla = j
    
    Sun_Abraham_Modelos =   paste0('SA_profes_preg_nat_',  j)
    lista_modelos = paste0('TWFE_profes_preg_nat_', j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    for (i in naturaleza) {
      if (i== "PUBL" ) {
        m = "PRIV"
      } else if (i == "PRIV" ) {
        m = "PUBL"
      } else {
        m = "All sample"
      }
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df = df %>% subset(df$time_to_treat >= -8)
      ####### Modelos estu_trabaja 
      MODEL = feols(TOTPRE_100kPROF_COL ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                    | id_name + year,                               ## FEs
                    cluster = ~ id_name + DIVIPOLA_MUN,                                ## Clustered SEs
                    data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
      
      MODEL_SA = feols( TOTPRE_100kPROF_COL ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                        | id_name + year,                               ## FEs
                        cluster = ~ id_name + DIVIPOLA_MUN,                            ## Clustered SEs
                        data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
      
      name_in_enviroment = paste0(  "Score at ", m, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_profes_preg_nat_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_profes_preg_nat_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_profes_preg_nat_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_profes_preg_nat_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_profes_preg_nat_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_profes_preg_nat_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_profes_preg_nat_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_profes_preg_nat_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
  }
  
  
  
  tablas = c( 'base_ai','base_10p', 'base_ic')
  for (j in tablas) {
    print(j)
    tabla = j
    
    Sun_Abraham_Modelos =   paste0('SA_labor_force_',  j)
    lista_modelos = paste0('TWFE_labor_force_', j)
    
    assign( lista_modelos, list(), envir = .GlobalEnv)
    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
    
    for (i in naturaleza) {
      if (i== "PUBL" ) {
        m = "PRIV"
      } else if (i == "PRIV" ) {
        m = "PUBL"
      } else {
        m = "All sample"
      }
      print(Sun_Abraham_Modelos)
      df = get(tabla)
      df = df %>% subset(df$time_to_treat >= -8)
      ####### Modelos estu_trabaja 
      MODEL = feols(frac_trabaja_sd ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                    | id_name + year,                               ## FEs
                    cluster = ~ id_name + DIVIPOLA_MUN,                                ## Clustered SEs
                    data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
      
      MODEL_SA = feols( frac_trabaja_sd ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                        | id_name + year,                               ## FEs
                        cluster = ~ id_name + DIVIPOLA_MUN,                            ## Clustered SEs
                        data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
      
      name_in_enviroment = paste0(  "Score at ", m, " Meters")
      
      #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
      if (j == 'base_10p') {
        TWFE_labor_force_base_10p[[name_in_enviroment]] <- (MODEL) 
        SA_labor_force_base_10p[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ic') {
        TWFE_labor_force_base_ic[[name_in_enviroment]] <- (MODEL)
        SA_labor_force_base_ic[[name_in_enviroment]] <- (MODEL_SA)
      } else if (j == 'base_ent') {
        TWFE_labor_force_base_ent[[name_in_enviroment]] <- (MODEL)  
        SA_labor_force_base_ent[[name_in_enviroment]] <- (MODEL_SA)
      }  else if (j == 'base_ai') {
        TWFE_labor_force_base_ai[[name_in_enviroment]] <- (MODEL)  
        SA_labor_force_base_ai[[name_in_enviroment]] <- (MODEL_SA)
      } else {
        message('no esta en las tabkas')
      }
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }
  }
  
  
}

tablas = c( 'base_ai','base_10p', 'base_ic', 'base_ent')

for (j in tablas) {
  print(j)
  tabla = j
  
  
  Sun_Abraham_Modelos =   paste0('SA_labor_force_naturaleza_',  j)
  lista_modelos = paste0('TWFE_labor_force_naturaleza_', j)
  
  assign( lista_modelos, list(), envir = .GlobalEnv)
  assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
  
  for (i in naturaleza) {
    if (i== "PUBL" ) {
      m = "PRIV"
    } else if (i == "PRIV" ) {
      m = "PUBL"
    } else {
      m = "All sample"
    }
    print(i)
    df = get(tabla)
    df = df %>% subset(df$time_to_treat >= -8)
    ####### Modelos
    base_ai$
    MODEL = feols(estu_trabaja ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                  | id_name + year,                               ## FEs
                  cluster = ~ id_name + DIVIPOLA_MUN,                               ## Clustered SEs
                  data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
    
    MODEL_SA = feols( estu_trabaja ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                      | id_name + year,                               ## FEs
                      cluster = ~ id_name + DIVIPOLA_MUN,                            ## Clustered SEs
                      data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
    
    name_in_enviroment = paste0(  "Score at ", m, " Meters")
    
    #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
    if (j == 'base_10p') {
      TWFE_labor_force_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL) 
      SA_labor_force_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL_SA)
    } else if (j == 'base_ic') {
      TWFE_labor_force_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL)
      SA_labor_force_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL_SA)
    } else if (j == 'base_ent') {
      TWFE_labor_force_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL)  
      SA_labor_force_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL_SA)
    }  else if (j == 'base_ai') {
      TWFE_labor_force_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL)  
      SA_labor_force_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL_SA)
    } else {
      message('no esta en las tabkas')
    }
    #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
  }
  
}

###########################################
time_treat = c('10p','ai','ent', 'ic')
pch_ = c(16,17 ,15, 1,2, 0,20)



for (i in time_treat){
  lista_modelo = list('Private schools' = get(paste0("SA_reading_naturaleza_base_", i))[["Score at PRIV Meters"]] ,
                      'Public schools' = get(paste0("SA_reading_naturaleza_base_", i))[["Score at PUBL Meters"]],
                      'All sample schools' = get(paste0("SA_Reading_base_", i))[["Score at 1000 Meters"]] )
  etable(  lista_modelo )
  
  png(paste0("graph/", "SA_reading_score_by_nature_",i,".png"),  width = 1030, height = 598)
  event_study_plot(results_by_buffer(lista_modelo), seperate = F, TITULO= 'Sun and Abraham (2020):\n Heterogeneities in reading literacy scores ' ) 
 
  dev.off()
  
} 
###########################################

for (i in time_treat){
  lista_modelo = list('Private schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PRIV Meters"]],
                      'Public schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PUBL Meters"]],
                      'All sample schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at All sample Meters"]] )
  
  etable(  lista_modelo )
  
  png(paste0("graph/", "SA_math_score_by_nature_",i,".png"),  width = 1030, height = 598)
  event_study_plot(results_by_buffer(lista_modelo), seperate = F, TITULO= 'Sun and Abraham (2020):\n Heterogeneities in math scores by nature of the school' ) 
 
  dev.off()
  
} 
###########################################

for (i in time_treat){
  tryCatch( {
  lista_modelo = list('Private schools' = get(paste0("SA_labor_force_base_", i))[["Score at PRIV Meters"]],
                      'Public schools' = get(paste0("SA_labor_force_base_", i))[["Score at PUBL Meters"]],
                      'All sample schools' = get(paste0("SA_labor_force_base_", i))[["Score at All sample Meters"]] )
  etable(  lista_modelo )
  
  # png(paste0("graph/", "SA_labor_force_by_nature_",i,".png"),  width = 1030, height = 598)
  event_study_plot(results_by_buffer(lista_modelo), seperate = F,
                   TITULO= 'Sun and Abraham (2020):\n Heterogeneities in labor force by nature of the school' ) 
  
  # dev.off()
  }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
  )
  
} 


###########################################


################################
event_study_plot(results_by_buffer(SA_labor_force_base_10p), 
                 seperate = T, 
                 TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 

##################################
#human capital acum
png(paste0("graph/", "SA_reading_score_by_labor_force_10p.png"),  width = 1030, height = 598)
event_study_plot(results_by_buffer(SA_estu_trabaja_base_10p), 
                 seperate = T, 
                 TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 

dev.off()


event_study_plot(results_by_buffer(SA_estu_trabaja_base_ai), 
                 seperate = T, 
                 TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 

event_study_plot(results_by_buffer(SA_estu_trabaja_base_ic), 
                 seperate = T, 
                 TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 

event_study_plot(results_by_buffer(SA_estu_trabaja_base_ent), 
                 seperate = T, 
                 TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 



#########################################
# LABOR FORCE PARTICIPATION
####
base_10p$frac_trabaja_sd

Callaway = Callaway_table(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='frac_trabaja_sd' )

#Math_Gardner_ic = Gardner_table( yname = 'math_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 
tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_estu_trabaja_base_10p[[1]] ),
                                SA_table(  SA_estu_trabaja_base_10p[[1]] )
                                , Callaway)) 

png(paste0("graph/labor_force_10p",".png"),  width = 1030, height = 598)
event_study_plot(  tabla , seperate = F, TITULO = 'Labor force participation' )   
dev.off() 
########## RESULTS ALL BUFFES AT 10P
png(paste0("graph/SA_labor_force_10p_ALL_BUFFER.png"),  width = 1030, height = 598)
event_study_plot(  results_by_buffer(  SA_estu_trabaja_base_10p   ) , 
                   seperate = T, 
                   TITULO = 'Sun and Abraham (2020)\n Labor force participation' )

dev.off() 
########## RESULTS ALL BUFFES AT AI

png(paste0("graph/SA_reading_score_by_labor_force_ai"),  width = 1030, height = 598)
event_study_plot(  results_by_buffer(  SA_estu_trabaja_base_ai   ) , 
                   seperate = T, 
                   TITULO = 'Sun and Abraham (2020)\n Labor force participation' )

dev.off() 

#########################################

time_treat = c('10p','ai','ent', 'ic')
for (i in time_treat){
  lista_modelo = list('Private schools' = get(paste0("SA_labor_force_base_", i))[["Score at PRIV Meters"]],
                      'Public schools' = get(paste0("SA_labor_force_base_", i))[["Score at PUBL Meters"]],
                      'All sample schools' = get(paste0("SA_estu_trabaja_base_", i))[[1]])
  etable(  lista_modelo )
  
  png(paste0("graph/", "SA_natu_labor_force_base",i,".png"),  width = 1030, height = 598)
  event_study_plot(results_by_buffer(lista_modelo), seperate = F, TITULO= 'Sun and Abraham (2020):\n Heterogeneities in Fraction of students from schools who just to work\n by nature of the school' ) 
  
  
  dev.off()
  
} 





#########################################
# UNIVERSITARIES STUDIES PARTICIPATION
####

########## RESULTS ALL BUFFES AT IC
Callaway_ = Callaway_table(buffer = 1000,  
                           tabla= subset(base_ic,base_ic$time_to_treat >= -8 & base_ic$year <= 2013  ) , 
                           anticipation =0,  yname ='finished_uni' )
Callaway_
#Math_Gardner_ic = Gardner_table( yname = 'math_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 
tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_Participate_saberpro_base_ic[["Score at 1000 Meters"]] ),
                                SA_table(  SA_Participate_saberpro_base_ic[["Score at 1000 Meters"]] )
                                , Callaway_))
png(paste0("graph/Participate_saberpro_ic",".png"),  width = 1030, height = 598)
event_study_plot(  tabla , seperate = F,
                   TITULO = 'Fraction of students who have completed any grade of university' )
dev.off() 

########## RESULTS ALL BUFFES AT AI
#Math_Gardner_ic = Gardner_table( yname = 'math_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 
tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_Participate_saberpro_base_ai[["Score at 1000 Meters"]] ),
                                SA_table(  SA_Participate_saberpro_base_ai[["Score at 1000 Meters"]] )
                                , 
                                Callaway_table(buffer = 1000,  
                                               tabla= subset(base_ai,base_ai$time_to_treat >= -8 & base_ai$year <= 2013  ) , 
                                               anticipation =0,  yname ='finished_uni' )
))
png(paste0("graph/Participate_saberpro_ai",".png"),  width = 1030, height = 598)
event_study_plot(  tabla , seperate = F,
                   TITULO = 'Fraction of students who have completed any grade of university' )
dev.off() 

########## RESULTS ALL BUFFES AT 10P
tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_Participate_saberpro_base_10p[["Score at 1000 Meters"]] ),
                                SA_table(  SA_Participate_saberpro_base_10p[["Score at 1000 Meters"]] )
                                , 
                                Callaway_table(buffer = 1000,  
                                               tabla= subset(base_10p,base_10p$time_to_treat >= -8 & base_10p$year <= 2013  ) , 
                                               anticipation =0,  yname ='finished_uni' )
))
png(paste0("graph/Participate_saberpro_10p",".png"),  width = 1030, height = 598)
event_study_plot(  tabla , seperate = F,
                   TITULO = 'Fraction of students who have completed any grade of university' )
dev.off() 

###
event_study_plot(tabla)
time_treat = c('10p','ai',  'ic')
for (j in time_treat) {
  print(j)
  tabla = j
  
  Sun_Abraham_Modelos =   paste0('SA_natu_Participate_saberpro_base_',  j)
  lista_modelos = paste0('TWFE_natu_Participate_saberpro_base_', j)
  
  assign( lista_modelos, list(), envir = .GlobalEnv)
  assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
  
  for (i in naturaleza) {
    print(Sun_Abraham_Modelos)
    df = get(paste0("base_", tabla))
    #df = get(paste0("base_", "10p"))
    #i="PUBL"
    df = df %>% subset(df$time_to_treat >= -8)
    ####### Modelos
    MODEL = feols(finished_uni ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                  |  cole_cod_dane+ year,                             ## FEs
                  cluster = ~ cole_cod_dane,                              ## Clustered SEs
                  data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i  & df$year <= 2013 ) )
    
    MODEL_SA = feols( finished_uni ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                      |  cole_cod_dane+ year,                             ## FEs
                      cluster = ~ cole_cod_dane,                         ## Clustered SEs
                      data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  & df$year <= 2013) )
    
    name_in_enviroment = paste0(  "Score at ", m, " Meters")
    
    #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
    if (j == '10p') {
      TWFE_natu_Participate_saberpro_base_10p[[name_in_enviroment]] <- (MODEL) 
      SA_natu_Participate_saberpro_base_10p[[name_in_enviroment]] <- (MODEL_SA)
    } else if (j == 'ic') {
      TWFE_natu_Participate_saberpro_base_ic[[name_in_enviroment]] <- (MODEL)
      SA_natu_Participate_saberpro_base_ic[[name_in_enviroment]] <- (MODEL_SA)
    } else if (j == 'ent') {
      warning(pass)
    }  else if (j == 'ai') {
      TWFE_natu_Participate_saberpro_base_ai[[name_in_enviroment]] <- (MODEL)  
      SA_natu_Participate_saberpro_base_ai[[name_in_enviroment]] <- (MODEL_SA)
    } else {
      message('no esta en las tabkas')
    }
    #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
  }
}

png(paste0("graph/SA_Participate_saberpro_base_ic_ALL_BUFFER_",".png"),  width = 1030, height = 598)

event_study_plot(  results_by_buffer(  SA_Participate_saberpro_base_ic   ) , 
                   seperate = T, 
                   TITULO = 'Sun and Abraham (2020)\n Fraction of students who have completed any grade of university' )
dev.off() 


for (i in time_treat){
  
  lista_modelo = list('Private schools' = get(paste0("SA_natu_Participate_saberpro_base_", i))[["Score at PRIV Meters"]],
                      'Public schools' = get(paste0("SA_natu_Participate_saberpro_base_", i))[["Score at PUBL Meters"]],
                      'All sample schools' = get(paste0("SA_Participate_saberpro_base_", i))[[1]])
  etable(  lista_modelo )
  
  png(paste0("graph/", "SA_natu_Participate_saberpro_base_",i,".png"),  width = 1030, height = 598)
  event_study_plot(results_by_buffer(lista_modelo), seperate = F, TITULO= 'Sun and Abraham (2020):\n Heterogeneities in Fraction of students who have completed any grade of university\n by nature of the school' ) 
  
  
  dev.off()
  
} 
time_treat = c('10p','ai','ent', 'ic')



#setwd("C:/Users/USER/Desktop/DID roads/Data/")


dev.off()























