
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
#####################     Mathematics                 ###########
#################################################################

math_c_sd
fixest::etable(list(SA_Math_base_10p[["Score at 1000 Meters"]], 
                    SA_Math_base_50p[["Score at 1000 Meters"]],
                    SA_Math_base_ent[["Score at 1000 Meters"]]), tex = TRUE)

## Estimadores
CS_math_base_10p = Callaway_table(buffer = 1000,   tabla= base_10p ,  anticipation=0,    yname ='math_c_sd' )
CS_math_base_50p = Callaway_table(buffer = 1000,   tabla= base_50p ,  anticipation=0,    yname ='math_c_sd' )
CS_math_base_100p = Callaway_table(buffer = 1000,   tabla= base_ent ,  anticipation=0,    yname ='math_c_sd' )
math_TWFE_10p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_10p[["Score at 1000 Meters"]])
math_TWFE_50p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_50p[["Score at 1000 Meters"]])
math_TWFE_100p = TWFE_table(estimator = 'TWFE', MODELO = TWFE_Math_base_ent[["Score at 1000 Meters"]])

math_SA_10p = SA_table( SA_Math_base_10p[["Score at 1000 Meters"]])
math_SA_50p = SA_table( SA_Math_base_50p[["Score at 1000 Meters"]])
math_SA_100p = SA_table( SA_Math_base_ent[["Score at 1000 Meters"]])

## Union de los estimadores
math_10p = inrange( do.call("rbind", list(math_TWFE_10p, math_SA_10p, CS_math_base_10p)) ,   periodo= 'term')
math_50p = inrange( do.call("rbind", list(math_TWFE_50p, math_SA_50p, CS_math_base_50p)) ,   periodo= 'term')
math_100p = inrange( do.call("rbind", list(math_TWFE_100p, math_SA_100p, CS_math_base_100p)) ,   periodo= 'term')

## Exportar imagen de todos los estimadores
png(paste0("graph/math_10p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  math_10p , seperate = F  )
dev.off() 

png(paste0("graph/math_50p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  math_50p , seperate = F  )
dev.off() 

png(paste0("graph/math_100p_1000m",".png"),  width = 1030, height = 598)
event_study_plot(  math_100p , seperate = F  )
dev.off() 


#### buffers
png(paste0("graph/SA_math_base_10p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Math_base_10p[0:6] ) )
dev.off() 

png(paste0("graph/SA_math_base_50p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Math_base_50p[0:6] ) )
dev.off() 

png(paste0("graph/SA_math_base_100p_buffers",".png"),  width = 1030, height = 598)
event_study_plot( results_by_buffer(SA_Math_base_ent[0:6] ) )
dev.off() 

etable(SA_Math_base_ent[0:6] , tex = TRUE)
etable(SA_Math_base_10p[0:6] , tex = TRUE)

etable(SA_Reading_base_ent[0:6] , tex = TRUE) 

etable(SA_Reading_base_10p[0:6], tex = TRUE ) 




time_treat = c('50p','10p','ai', 'ic', 'ent')


for (i in time_treat){
  print(i)
  lista_modelo = list('Private schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PRIV Meters"]] ,
                      'Public schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PUBL Meters"]],
                      'All sample schools' = get(paste0("SA_Math_base_", i))[["Score at 1000 Meters"]])
  # etable(  lista_modelo , tex = TRUE)
  print(etable(  lista_modelo  ))
  
  
} 


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
