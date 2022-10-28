inrange <- function(tabla, periodo){
  tabla =subset(tabla, tabla[[periodo]] >= -5 )
  tabla =subset(tabla, tabla[[periodo]] <= 10 )
}
#################################################################

########################################################
##################### Results math #####################
########################################################

#################################################################
## Estimadores
CS_Math_base_10p = Callaway_table(buffer = 1000,   tabla= base_10p ,  anticipation=0,    yname ='math_c_sd' )
Math_TWFE_10p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_10p[["Score at 1000 Meters"]])
Math_SA_10p = SA_table( SA_Math_base_10p[["Score at 1000 Meters"]])

 
## Union de los estimadores
Math_10p = inrange( do.call("rbind", list(Math_TWFE_10p, Math_SA_10p, CS_Math_base_10p)) ,   periodo= 'term')
event_study_plot(  Math_10p , seperate = F, )

## Exportar imagen de todos los estimadores
png(paste0("graph/Math_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Math_10p , seperate = F,
                   TITULO = 'Math Score\n 10 % advance of construction' )
dev.off() 
## Exportar imagen de todos los buffer 
png(paste0("graph/SA_Math_base_10p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Math_base_10p ) )
dev.off() 
# rm(SA_math_naturaleza_base_10p)
## Exportar imagen por naturaleza 
 

lista_modelo = list('Private schools' =    (SA_math_naturaleza_base_10p[["Score at PRIV Meters"]] ) ,
                    'Public schools' =   (SA_math_naturaleza_base_10p[["Score at PUBL Meters"]] ),
                    'All sample schools' = ( SA_Math_base_10p[["Score at 1000 Meters"]]) )
 
SA_table(SA_math_naturaleza_base_10p[["Score at PRIV Meters"]] ) 
SA_table(SA_math_naturaleza_base_10p[["Score at PUBL Meters"]] ) 

inrange( results_by_buffer(lista_modelo) ,   periodo= 'term')

png(paste0("graph/", "SA_math_score_by_nature_10p"),  width = 1030, height = 598)
event_study_plot(inrange( results_by_buffer(lista_modelo) ,   periodo= 'term'), seperate = F, 
                 TITULO= 'Sun and Abraham (2020):\n Heterogeneities in Math scores ' ) 

dev.off()
#################################################################
 
#################################################################
#####################     Reading                 ###############
#################################################################

#################################################################

## Estimadores
CS_reading_base_10p = Callaway_table(buffer = 1000,   tabla= base_10p ,  anticipation=0,    yname ='reading_c_sd' )
reading_TWFE_10p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Reading_base_10p[["Score at 1000 Meters"]])
reading_SA_10p = SA_table( SA_Reading_base_10p[["Score at 1000 Meters"]])

## Union de los estimadores
reading_10p = inrange( do.call("rbind", list(reading_TWFE_10p, reading_SA_10p, CS_reading_base_10p)) ,   periodo= 'term')

## Exportar imagen de todos los estimadores
png(paste0("graph/Reading_10p_1000m",".png"),  width = 1030, height = 598)

event_study_plot(  reading_10p , seperate = F,
                   TITULO = 'reading Score\n 10 % advance of construction' )

dev.off() 

## Exportar imagen de todos los buffer 
png(paste0("graph/SA_Reading_base_10p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Reading_base_10p ) )
dev.off() 
## Exportar imagen por naturaleza 

 

lista_modelo = list('Private schools' =    (SA_reading_naturaleza_base_10p[["Score at PRIV Meters"]] ) ,
                    'Public schools' =   (SA_reading_naturaleza_base_10p[["Score at PUBL Meters"]] ),
                    'All sample schools' = ( SA_Reading_base_10p[["Score at 1000 Meters"]]) )


inrange( results_by_buffer(lista_modelo) ,   periodo= 'term')

png(paste0("graph/", "SA_reading_score_by_nature_10p"),  width = 1030, height = 598)
event_study_plot(inrange( results_by_buffer(lista_modelo) ,   periodo= 'term'), seperate = F, 
                 TITULO= 'Sun and Abraham (2020):\n Heterogeneities in reading scores ' ) 

dev.off()
####


#################################################################


lista_modelo = list('Private schools' =    (SA_labor_force_base_10p[["Score at PRIV Meters"]] ) ,
                    'Public schools' =   (SA_labor_force_base_10p[["Score at PUBL Meters"]] ),
                    'All sample schools' = ( SA_labor_force_base_10p[["Score at All sample Meters"]]) )


inrange( results_by_buffer(lista_modelo) ,   periodo= 'term')

png(paste0("graph/", "SA_labor_force_by_nature_10p"),  width = 1030, height = 598)
event_study_plot(inrange( results_by_buffer(lista_modelo) ,   periodo= 'term'), seperate = F, 
                 TITULO= 'Sun and Abraham (2020):\n Heterogeneities in reading scores ' ) 

dev.off()


#################################################################


lista_modelo = list('Private schools' =    (SA_labor_force_base_10p[["Score at PRIV Meters"]] ) ,
                    'Public schools' =   (SA_labor_force_base_10p[["Score at PUBL Meters"]] ),
                    'All sample schools' = ( SA_labor_force_base_10p[["Score at All sample Meters"]]) )


inrange( results_by_buffer(lista_modelo) ,   periodo= 'term')

png(paste0("graph/", "SA_labor_force_by_nature_10p"),  width = 1030, height = 598)
event_study_plot(inrange( results_by_buffer(lista_modelo) ,   periodo= 'term'), seperate = F, 
                 TITULO= 'Sun and Abraham (2020):\n Heterogeneities in reading scores ' ) 

dev.off()

####
#################################################################
#####################     Teachers                 ###############
#################################################################

#################################################################

library(readr)
Training_teachers <- read_csv("Training_teachers.csv", 
                              col_types = cols(COD_INST = col_character()))


base = sqldf::sqldf("
                    SELECT * FROM 
                    base_10p
                    INNER JOIN Training_teachers
                    ON COD_INST = cole_cod_dane AND ANO_PROC = YEAR
                    ")

CS_teachers_base_10p = Callaway_table(buffer = 1000,   tabla= base ,  
                                      anticipation=0,    yname ='FRAC_PREGRADO' )


####### Modelos

teachers_SA_10p = feols( FRAC_PREGRADO ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                           |  cole_cod_dane+ year,                             ## FEs
                           cluster = ~ cole_cod_dane,                         ## Clustered SEs
                           data = subset(base, base$buffer_km == 1000 ) )
 

teachers_TWFE_10p = feols( (FRAC_PREGRADO) ~ i(time_to_treat, treat_, ref = 0) #+ ## The only thing that's changed
                  |  cole_cod_dane+ year,                             ## FEs
                  cluster = ~ cole_cod_dane,                         ## Clustered SEs
                  data = subset(base, base$buffer_km == 1000 ) )


teachers_TWFE_10p = TWFE_table(estimator = 'TWFE', 
                               MODELO = teachers_TWFE_10p)
teachers_SA_10p = SA_table(teachers_SA_10p)

## Union de los estimadores
teachers_10p =  do.call("rbind", list(teachers_TWFE_10p, teachers_SA_10p
                                      #, CS_teachers_base_10p
                                      ))

## Exportar imagen de todos los estimadores
#png(paste0("graph/Reading_10p_1000m",".png"),  width = 1030, height = 598)

event_study_plot(  inrange(teachers_10p ,   periodo= 'term') , seperate = F,
                   TITULO = 'teachers Score\n 10 % advance of construction' )

tablas = c(  'base')

#############################################################33
naturaleza = unique(base$cole_naturaleza)

for (j in tablas) {
  print(j)
  tabla = j
  
  Sun_Abraham_Modelos =   paste0('SA_teachers_naturaleza_',  j)
  lista_modelos = paste0('TWFE_teachers_naturaleza_', j)
  
  assign( lista_modelos, list(), envir = .GlobalEnv)
  assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
  
  
  for (i in naturaleza) {
    print(Sun_Abraham_Modelos)
    df = get(tabla)
    #df = df %>% subset(df$time_to_treat >= -8)
    ####### Modelos
    MODEL = feols(FRAC_PREGRADO ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
                  |  cole_cod_dane+ year,                             ## FEs
                  cluster = ~ cole_cod_dane,                              ## Clustered SEs
                  data = subset(df, df$buffer_km==1000  & df$cole_naturaleza == i  ) )
    
    MODEL_SA = feols( FRAC_PREGRADO ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                      |  cole_cod_dane+ year,                             ## FEs
                      cluster = ~ cole_cod_dane,                         ## Clustered SEs
                      data = subset(df,df$buffer_km==1000  & df$cole_naturaleza == i ) )
    
    name_in_enviroment = paste0(  "Score at ", i, " Meters")
    
    #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
    if (j == 'base') {
      TWFE_teachers_naturaleza_base[[name_in_enviroment]] <- (MODEL) 
      SA_teachers_naturaleza_base[[name_in_enviroment]] <- (MODEL_SA)
    }  else {
      message('no esta en las tabkas')
    }
    #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
  }
}






lista_modelo = list('Private schools' =    (SA_teachers_naturaleza_base[["Score at PRIV Meters"]] ) ,
                    'Public schools' =   (SA_teachers_naturaleza_base[["Score at PUBL Meters"]] ),
                    'All sample schools' = ( teachers_SA_10p) )


inrange( results_by_buffer(lista_modelo) ,   periodo= 'term')

#png(paste0("graph/", "SA_reading_score_by_nature_10p"),  width = 1030, height = 598)
event_study_plot(inrange( results_by_buffer(lista_modelo) ,   periodo= 'term'), seperate = F, 
                 TITULO= 'Sun and Abraham (2020):\n Heterogeneities in reading scores ' ) 

#dev.off()