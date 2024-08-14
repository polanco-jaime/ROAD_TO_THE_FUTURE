

#################################
# Callaway, Brantly and Pedro H.C. Sant'Anna 2021
#################################
##################################################
# Reading
##################################################
sd_math_cs_ic = Call_San(data= data_ic,
                 outcome ="sd_math_i",
                 referemce_time = 'year' ,
                 time_name = "year_treated_ic",
                 id_name = "cole_cod_dane_institucion" ,
                 panel = F  )

 
print( paste0( round(sd_math_cs_ic[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_ic[[2]][["overall.se"]], 4), ')'))

sd_math_cs_ai = Call_San(data= data_ai,
                 outcome ="sd_math_i",
                 referemce_time = 'year' ,
                 time_name = "year_treated_ai",
                 id_name = "cole_cod_dane_institucion" ,
                 panel = F  )

print( paste0( round(sd_math_cs_ai[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_ai[[2]][["overall.se"]], 4), ')'))


sd_math_cs_10p = Call_San(data= data_10p,
                  outcome ="sd_math_i",
                  referemce_time = 'year' ,
                  time_name = "year_treated_10p",
                  id_name = "cole_cod_dane_institucion" ,
                  panel = F  )

print( paste0( round(sd_math_cs_10p[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_10p[[2]][["overall.se"]], 4), ')'))


sd_math_cs_50p = Call_San(data= data_50p,
                  outcome ="sd_math_i",
                  referemce_time = 'year' ,
                  time_name = "year_treated_50p",
                  id_name = "cole_cod_dane_institucion" ,
                  panel = F  )

print( paste0( round(sd_math_cs_50p[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_50p[[2]][["overall.se"]], 4), ')'))


sd_math_cs_100p = Call_San(data= data_100p,
                          outcome ="sd_math_i",
                          referemce_time = 'year' ,
                          time_name = "year_treated_100p",
                          id_name = "cole_cod_dane_institucion" ,
                          panel = F  )
print( paste0( round(sd_math_cs_100p[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_100p[[2]][["overall.se"]], 4), ')'))


#> Overall summary of ATT's based on event-study/dynamic aggregation:  

print( paste0( round(sd_math_cs_ai[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_ai[[2]][["overall.se"]], 4), ')'))

print( paste0( round(sd_math_cs_10p[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_10p[[2]][["overall.se"]], 4), ')'))

print( paste0( round(sd_math_cs_50p[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_50p[[2]][["overall.se"]], 4), ')'))

print( paste0( round(sd_math_cs_100p[[2]][["overall.att"]], 4), ' (', round(sd_math_cs_100p[[2]][["overall.se"]], 4), ')'))

##################################################
# Reading
##################################################


sd_reading_cs_ic = Call_San(data= data_ic,
                         outcome ="sd_reading_i",
                         referemce_time = 'year' ,
                         time_name = "year_treated_ic",
                         id_name = "cole_cod_dane_institucion" ,
                         panel = F  )


print( paste0( round(sd_reading_cs_ic[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_ic[[2]][["overall.se"]], 4), ')'))

sd_reading_cs_ai = Call_San(data= data_ai,
                         outcome ="sd_reading_i",
                         referemce_time = 'year' ,
                         time_name = "year_treated_ai",
                         id_name = "cole_cod_dane_institucion" ,
                         panel = F  )

print( paste0( round(sd_reading_cs_ai[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_ai[[2]][["overall.se"]], 4), ')'))


sd_reading_cs_10p = Call_San(data= data_10p,
                          outcome ="sd_reading_i",
                          referemce_time = 'year' ,
                          time_name = "year_treated_10p",
                          id_name = "cole_cod_dane_institucion" ,
                          panel = F  )

print( paste0( round(sd_reading_cs_10p[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_10p[[2]][["overall.se"]], 4), ')'))


sd_reading_cs_50p = Call_San(data= data_50p,
                          outcome ="sd_reading_i",
                          referemce_time = 'year' ,
                          time_name = "year_treated_50p",
                          id_name = "cole_cod_dane_institucion" ,
                          panel = F  )

print( paste0( round(sd_reading_cs_50p[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_50p[[2]][["overall.se"]], 4), ')'))


sd_reading_cs_100p = Call_San(data= data_100p,
                           outcome ="sd_reading_i",
                           referemce_time = 'year' ,
                           time_name = "year_treated_100p",
                           id_name = "cole_cod_dane_institucion" ,
                           panel = F  )
print( paste0( round(sd_reading_cs_100p[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_100p[[2]][["overall.se"]], 4), ')'))


#> Overall summary of ATT's based on event-study/dynamic aggregation:  

print( paste0( round(sd_reading_cs_ai[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_ai[[2]][["overall.se"]], 4), ')'))

print( paste0( round(sd_reading_cs_10p[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_10p[[2]][["overall.se"]], 4), ')'))

print( paste0( round(sd_reading_cs_50p[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_50p[[2]][["overall.se"]], 4), ')'))

print( paste0( round(sd_reading_cs_100p[[2]][["overall.att"]], 4), ' (', round(sd_reading_cs_100p[[2]][["overall.se"]], 4), ')'))





##################################################
 
##################################################

# Resumen de los resultados
summary(out)
ggdid(out)

# agg.simple <- aggte(out, type = "simple", na.rm = TRUE)
# summary(agg.simple)

agg.es <- aggte(out, type = "dynamic", na.rm = TRUE)
summary(agg.es)
ggdid(agg.es)


summary(data_ic$estu_edad)
out <- att_gt(
  yname = "sd_math_i", # Nombre de la variable de resultado
  tname = "year",    # Nombre de la variable de tiempo
  idname = "cole_cod_dane_institucion",     # Nombre de la variable de identificación
  gname = "year_treated_10p", # Nombre de la variable de tiempo de tratamiento
  data = data_10p,   # Datos
  control_group = "notyettreated",
  # xformla= ~estu_genero+estu_edad,
  est_method = "dr",  # Método de estimación (Doubly Robust en este caso)
  panel = FALSE
)
# Resumen de los resultados
summary(out)
ggdid(out)

agg.simple <- aggte(out, type = "simple", na.rm = TRUE)
summary(agg.simple)

agg.es <- aggte(out, type = "dynamic", na.rm = TRUE)
summary(agg.es)
ggdid(agg.es)
