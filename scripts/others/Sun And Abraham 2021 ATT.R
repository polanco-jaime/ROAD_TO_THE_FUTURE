### Sun And Abraham 2021 ATT
tablas = c( 'base_ai','base_10p', 'base_50p', 'base_ic', 'base_ent')
colnames(base_ai)
Var_Y = c(   "math_c"          ,            "reading_c",                   "Participate_saberpro",       
  "estu_trabaja",                "estudiantes_prom",               "math_c_sd",                  
 "reading_c_sd",                "Participate_saberpro_sd",     "frac_trabaja_sd",            
 "estudiantes_prom_sd",         "math_c_sd_nat"             ,  "reading_c_sd_nat",
 "Participate_saberpro_sd_nat", "frac_trabaja_sd_nat",         "estudiantes_prom_sd_nat" ,                    
 "TOTPRE",  "TOTPROF",                     "PROF_TOT",                    "TOTPROF_100kPROF_COL"       ,
 "TOTPRE_100kPROF_COL",              "finished_uni"      )

buffer = 1000

 
Resumen_att = data.frame()
for (j  in Var_Y) {
  tryCatch( {
   for (i in tablas) {
    temp_att = ATT_SA(tabla = i, buffer = 1000, Y_var = j, heterogenities = F)
    Resumen_att = rbind(Resumen_att,temp_att )
    # print(Resumen_att)
  }
  
  
  for (i in tablas) {
    temp_att = ATT_SA(tabla = i, buffer = 1000, Y_var = j, heterogenities = 'cole_naturaleza')
    Resumen_att = rbind(Resumen_att,temp_att )
    # print(Resumen_att)
  }
  }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}  )
}

getwd()
write.csv2(Resumen_att, "C:/Users/USER/Desktop/DID roads/ROAD_TO_THE_FUTURE/Data/Resumen_att.csv")
