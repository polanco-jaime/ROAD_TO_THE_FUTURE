

#################################
# Callaway, Brantly and Pedro H.C. Sant'Anna 2021
#################################


##################################################
sd_math_cs_ic = Call_San(data= data_ic,
                 outcome ="sd_math_i",
                 referemce_time = 'year' ,
                 time_name = "year_treated_ic",
                 id_name = "cole_cod_dane_institucion" ,
                 panel = F  )

cs_ai = Call_San(data= data_ai,
                 outcome ="sd_math_i",
                 referemce_time = 'year' ,
                 time_name = "year_treated_ai",
                 id_name = "cole_cod_dane_institucion" ,
                 panel = F  )


sd_math_cs_10p = Call_San(data= data_10p,
                  outcome ="sd_math_i",
                  referemce_time = 'year' ,
                  time_name = "year_treated_10p",
                  id_name = "cole_cod_dane_institucion" ,
                  panel = F  )

 

sd_math_cs_50p = Call_San(data= data_50p,
                  outcome ="sd_math_i",
                  referemce_time = 'year' ,
                  time_name = "year_treated_50p",
                  id_name = "cole_cod_dane_institucion" ,
                  panel = F  )
 
# # Ejecutar el análisis de diferencias en diferencias
Call_San <- function(data , outcome ="outcome: string",
                     referemce_time = '' ,
                     time_name = "time name: string",
                     id_name = "",
                     panel = T   ){
  out <- att_gt(
    yname = outcome, # Nombre de la variable de resultado
    tname = referemce_time,    # Nombre de referencia de la variable de tiempo
    idname = outcome,     # Nombre de la variable de identificación
    gname = time_name, # Nombre de la variable de tiempo de tratamiento
    data = data,   # Datos
    control_group = "notyettreated",
    est_method = "dr",  # Método de estimación (Doubly Robust en este caso)
    panel = panel, 
    # xformla=~DISTANCE
  )
  print(summary(out))
  print(ggdid(out))
  agg.simple <- aggte(out, type = "simple", na.rm = TRUE)
  print(summary(agg.simple))
  agg.es <- aggte(out, type = "dynamic", na.rm = TRUE)
  summary(agg.es)
  print(ggdid(agg.es))
  return(list(out, agg.es))
}


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
