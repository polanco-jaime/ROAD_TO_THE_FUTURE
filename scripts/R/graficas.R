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
Math_TWFE_ai = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_ai[["Score at 1000 Meters"]])
Math_SA_ai = SA_table( SA_Math_base_ai[["Score at 1000 Meters"]])


## Union de los estimadores
Math_ai = inrange( do.call("rbind", list(Math_TWFE_ai, Math_SA_ai #, CS_Math_base_ai
)) ,   periodo= 'term')
event_study_plot(  Math_ai , seperate = F, )

# CS_Math_base_10p = Callaway_table(buffer = 1000,   tabla= base_10p ,  anticipation=0,    yname ='math_c_sd' )
Math_TWFE_10p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_10p[["Score at 1000 Meters"]])
Math_SA_10p = SA_table( SA_Math_base_10p[["Score at 1000 Meters"]])

fixest::etable(list(SA_Math_base_10p[["Score at 1000 Meters"]], 
                    SA_Math_base_50p[["Score at 1000 Meters"]],
                    SA_Math_base_ent[["Score at 1000 Meters"]]), tex = TRUE)
 
## Union de los estimadores
Math_10p = inrange( do.call("rbind", list(Math_TWFE_10p, Math_SA_10p #, CS_Math_base_10p
                                          )) ,   periodo= 'term')
event_study_plot(  Math_10p , seperate = F, )

## Exportar imagen de todos los estimadores
png(paste0("graph/Math_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Math_10p , seperate = F, TITULO = '' )
dev.off() 
###########################################################################################
## Estimadores
CS_Math_base_50p = Callaway_table(buffer = 1000,   tabla= base_50p ,  anticipation=0,    yname ='math_c_sd' )
Math_TWFE_50p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_50p[["Score at 1000 Meters"]] )
Math_SA_50p = SA_table( SA_Math_base_50p[["Score at 1000 Meters"]] )


## Union de los estimadores
Math_50p = inrange( do.call("rbind", list(Math_TWFE_50p, Math_SA_50p #, CS_Math_base_50p
                                          ) ) ,   periodo= 'term')
event_study_plot(  Math_50p , seperate = F, )

## Exportar imagen de todos los estimadores
png(paste0("graph/Math_50p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Math_50p , seperate = F,
                   TITULO = '' )
dev.off() 


# CS_Math_base_ent = Callaway_table(buffer = 1000,   tabla= base_ent ,  anticipation=0,    yname ='math_c_sd' )
Math_TWFE_ent = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_ent[["Score at 1000 Meters"]] )
Math_SA_ent = SA_table( SA_Math_base_ent[["Score at 1000 Meters"]] )


## Union de los estimadores
Math_ent = inrange( do.call("rbind", list(Math_TWFE_ent, Math_SA_ent #, CS_Math_base_ent 
)  ) ,   periodo= 'term')
event_study_plot(  Math_ent , seperate = F, )

## Exportar imagen de todos los estimadores
png(paste0("graph/Math_100p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  Math_ent , seperate = F,
                   TITULO = '' )
dev.off() 
###########################################################################################


####################################################################

## Exportar imagen de todos los buffer 
png(paste0("graph/SA_Math_base_10p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Math_base_10p[1:6] ) )
dev.off() 

png(paste0("graph/SA_Math_base_50p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Math_base_50p[1:6] ) )
dev.off() 

png(paste0("graph/SA_Math_base_100p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Math_base_ent[1:6] ) )
dev.off() 
etable(SA_Math_base_ent[1:6]) # , tex = TRUE
# rm(SA_math_naturaleza_base_10p)

####################################################################

## Exportar imagen de todos los buffer 
png(paste0("graph/SA_Reading_base_10p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Reading_base_10p[1:6] ) )
 
dev.off() 

png(paste0("graph/SA_Reading_base_50p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Reading_base_50p[1:6] ) )
dev.off() 

png(paste0("graph/SA_Reading_base_100p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Reading_base_ent[1:6] ) )
dev.off() 
etable(SA_Reading_base_ent[1:6]) # , tex = TRUE
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


fixest::etable(list(SA_Reading_base_10p[["Score at 1000 Meters"]], 
                    SA_Reading_base_50p[["Score at 1000 Meters"]],
                    SA_Reading_base_ent[["Score at 1000 Meters"]]), tex = TRUE)

## Estimadores
CS_reading_base_10p = Callaway_table(buffer = 1000,   tabla= base_10p ,  anticipation=0,    yname ='reading_c_sd' )
CS_reading_base_50p = Callaway_table(buffer = 1000,   tabla= base_50p ,  anticipation=0,    yname ='reading_c_sd' )
CS_reading_base_100p = Callaway_table(buffer = 1000,   tabla= base_ent ,  anticipation=0,    yname ='reading_c_sd' )
reading_TWFE_10p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Reading_base_10p[["Score at 1000 Meters"]])
reading_TWFE_50p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Reading_base_50p[["Score at 1000 Meters"]])
reading_TWFE_100p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Reading_base_ent[["Score at 1000 Meters"]])

reading_SA_10p = SA_table( SA_Reading_base_10p[["Score at 1000 Meters"]])
reading_SA_50p = SA_table( SA_Reading_base_50p[["Score at 1000 Meters"]])
reading_SA_100p = SA_table( SA_Reading_base_ent[["Score at 1000 Meters"]])

## Union de los estimadores
reading_10p = inrange( do.call("rbind", list(reading_TWFE_10p, reading_SA_10p, CS_reading_base_10p)) ,   periodo= 'term')
reading_50p = inrange( do.call("rbind", list(reading_TWFE_50p, reading_SA_50p, CS_reading_base_50p)) ,   periodo= 'term')
reading_100p = inrange( do.call("rbind", list(reading_TWFE_100p, reading_SA_100p, CS_reading_base_100p)) ,   periodo= 'term')

## Exportar imagen de todos los estimadores
png(paste0("graph/Reading_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  reading_10p , seperate = F  )
dev.off() 

png(paste0("graph/Reading_50p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  reading_50p , seperate = F  )
dev.off() 

png(paste0("graph/Reading_100p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  reading_100p , seperate = F  )
dev.off() 

#################################################################
#####################     Mathematics                 ###############
#################################################################


fixest::etable(list(SA_Reading_base_10p[["Score at 1000 Meters"]], 
                    SA_Reading_base_50p[["Score at 1000 Meters"]],
                    SA_Reading_base_ent[["Score at 1000 Meters"]]), tex = TRUE)

## Estimadores
CS_reading_base_10p = Callaway_table(buffer = 1000,   tabla= base_10p ,  anticipation=0,    yname ='reading_c_sd' )
CS_reading_base_50p = Callaway_table(buffer = 1000,   tabla= base_50p ,  anticipation=0,    yname ='reading_c_sd' )
CS_reading_base_100p = Callaway_table(buffer = 1000,   tabla= base_ent ,  anticipation=0,    yname ='reading_c_sd' )
reading_TWFE_10p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Reading_base_10p[["Score at 1000 Meters"]])
reading_TWFE_50p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Reading_base_50p[["Score at 1000 Meters"]])
reading_TWFE_100p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Reading_base_ent[["Score at 1000 Meters"]])

reading_SA_10p = SA_table( SA_Reading_base_10p[["Score at 1000 Meters"]])
reading_SA_50p = SA_table( SA_Reading_base_50p[["Score at 1000 Meters"]])
reading_SA_100p = SA_table( SA_Reading_base_ent[["Score at 1000 Meters"]])

## Union de los estimadores
reading_10p = inrange( do.call("rbind", list(reading_TWFE_10p, reading_SA_10p, CS_reading_base_10p)) ,   periodo= 'term')
reading_50p = inrange( do.call("rbind", list(reading_TWFE_50p, reading_SA_50p, CS_reading_base_50p)) ,   periodo= 'term')
reading_100p = inrange( do.call("rbind", list(reading_TWFE_100p, reading_SA_100p, CS_reading_base_100p)) ,   periodo= 'term')

## Exportar imagen de todos los estimadores
png(paste0("graph/Reading_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  reading_10p , seperate = F  )
dev.off() 

png(paste0("graph/Reading_50p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  reading_50p , seperate = F  )
dev.off() 

png(paste0("graph/Reading_100p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  reading_100p , seperate = F  )
dev.off() 

#################################################################
###########          Labor Force Part                 ##########
#################################################################

fixest::etable(list(SA_estu_trabaja_base_10p[["Score at 1000 Meters"]], 
                    SA_estu_trabaja_base_50p[["Score at 1000 Meters"]],
                    SA_estu_trabaja_base_ent[["Score at 1000 Meters"]]), tex = TRUE)

## Estimadores
CS_labor_force_base_10p = Callaway_table(buffer = 1000,   tabla= base_10p ,  anticipation=0,    yname ='frac_trabaja_sd' )
CS_labor_force_base_50p = Callaway_table(buffer = 1000,   tabla= base_50p ,  anticipation=0,    yname ='frac_trabaja_sd' )
CS_labor_force_base_100p = Callaway_table(buffer = 1000,   tabla= base_ent ,  anticipation=0,    yname ='frac_trabaja_sd' )
labor_force_TWFE_10p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_estu_trabaja_base_10p[["Score at 1000 Meters"]])
labor_force_TWFE_50p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_estu_trabaja_base_50p[["Score at 1000 Meters"]])
labor_force_TWFE_100p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_estu_trabaja_base_ent[["Score at 1000 Meters"]])

labor_force_SA_10p = SA_table( SA_estu_trabaja_base_10p[["Score at 1000 Meters"]])
labor_force_SA_50p = SA_table( SA_estu_trabaja_base_50p[["Score at 1000 Meters"]])
labor_force_SA_100p = SA_table( SA_estu_trabaja_base_ent[["Score at 1000 Meters"]])

## Union de los estimadores
labor_force_10p = inrange( do.call("rbind", list(labor_force_TWFE_10p, labor_force_SA_10p, CS_labor_force_base_10p)) ,   periodo= 'term')
labor_force_50p = inrange( do.call("rbind", list(labor_force_TWFE_50p, labor_force_SA_50p, CS_labor_force_base_50p)) ,   periodo= 'term')
labor_force_100p = inrange( do.call("rbind", list(labor_force_TWFE_100p, labor_force_SA_100p, CS_labor_force_base_100p)) ,   periodo= 'term')

## Exportar imagen de todos los estimadores
png(paste0("graph/labor_force_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  labor_force_10p , seperate = F  )
dev.off() 

png(paste0("graph/labor_force_50p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  labor_force_50p , seperate = F  )
dev.off() 

png(paste0("graph/labor_force_100p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  labor_force_100p , seperate = F  )
dev.off() 

#### buffers
png(paste0("graph/SA_estu_trabaja_base_10p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_estu_trabaja_base_10p[0:6] ) )
dev.off() 

png(paste0("graph/SA_estu_trabaja_base_50p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_estu_trabaja_base_50p[0:6] ) )
dev.off() 

png(paste0("graph/SA_estu_trabaja_base_100p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_estu_trabaja_base_ent[0:6] ) )
dev.off() 
### Labor Force

time_treat = c('50p','10p','ai', 'ic')
for (i in time_treat){
  # tryCatch( {
  lista_modelo = list('Private schools' = get(paste0("SA_labor_force_base_", i))[["Score at PRIV Meters"]],
                      'Public schools' = get(paste0("SA_labor_force_base_", i))[["Score at PUBL Meters"]],
                      'All sample schools' = get(paste0("SA_labor_force_base_", i))[["Score at All sample Meters"]])  
  
  etable(  lista_modelo )
  
  png(paste0("graph/", "SA_labor_force_by_nature_",i,".png"),  width = 1030, height = 598)
  event_study_plot(results_by_buffer(lista_modelo), seperate = F  ) 
  
  dev.off()
  # }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")} )
} 


#################################################################
############    Participate in uiversity               ##########
#################################################################
SA_Participate_saberpro_

fixest::etable(list(SA_Participate_saberpro_base_10p[["Score at 1000 Meters"]], 
                    SA_Participate_saberpro_base_50p[["Score at 1000 Meters"]] ), tex = TRUE)






#################################################################
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
