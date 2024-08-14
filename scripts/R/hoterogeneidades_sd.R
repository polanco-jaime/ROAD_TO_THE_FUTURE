##Reading data ##

data = arrow::read_parquet('Data/road_to_the_future.parquet')
data$delta_0p_100p = as.numeric(data$Fecha_entrega)-data$Fecha_Acta_de_inicio
data$delta_0p_10p = as.numeric(data$Fecha_al_10_)-data$Fecha_Acta_de_inicio
data$delta_10p_100p = as.numeric(data$Fecha_entrega)-as.numeric( data$Fecha_al_10_ )
data$delta_0p_50p = as.numeric(data$Fecha_al_50_)-data$Fecha_Acta_de_inicio
data$delta_50p_100p = as.numeric(data$Fecha_entrega)-as.numeric(data$Fecha_al_50_ )

##########################################
# data$time_to_treat = as.numeric(data$ANIO) -  as.numeric(data$Fecha_inicio_consecion)
# data$treat_relative = ifelse( data$time_to_treat>=0 , 1,0  )
# Como es el caso de que sea balanceado 
data = subset(data, data$Fecha_inicio_consecion !=1994)
# data = subset(data, data$Fecha_al_10_ !=2014)
# data = subset(data, data$Fecha_al_10_ !=2021)
data = subset(data, as.numeric(data$ANIO) <=2021)



#
sqldf::sqldf("SELECT   cole_naturaleza , COUNT(DISTINCT cole_cod_d) TOT FROM data group by 1")
sqldf::sqldf("SELECT   cole_naturaleza , COUNT(DISTINCT estu_consecutivo_sbtyt) TOT FROM data group by 1")

data = subset(data, data$cole_naturaleza =="PUBL")

sqldf::sqldf("SELECT   cole_naturaleza , COUNT(DISTINCT cole_cod_d) TOT FROM data group by 1")


sqldf::sqldf("SELECT AVG(delta_10p_100p) AVERAGE FROM (
             SELECT DISTINCT delta_10p_100p,   NOMBRE  TOT FROM data WHERE delta_10p_100p IS NOT NULL
             ) ") 
########################################
table(data$Fecha_inicio_consecion)
data$year_treated_ic = ifelse(is.na(as.numeric(data$Fecha_inicio_consecion))==F, as.numeric(data$Fecha_inicio_consecion), NaN)
table(data$year_treated_ic)
########################################
table(data$Fecha_Acta_de_inicio)
data$year_treated_ai = ifelse(is.na(as.numeric(data$Fecha_Acta_de_inicio))==F, as.numeric(data$Fecha_Acta_de_inicio), NaN)
table(data$year_treated_ai)
########################################
table(data$Fecha_al_10_)
data$year_treated_10p = ifelse(is.na(as.numeric(data$Fecha_al_10_))==F, as.numeric(data$Fecha_al_10_), NaN)
table(data$year_treated_10p)

########################################
table(data$Fecha_al_50_)
data$year_treated_50p = ifelse(is.na(as.numeric(data$Fecha_al_50_))==F, as.numeric(data$Fecha_al_50_), NaN)
table(data$year_treated_50p)

########################################
table(data$Fecha_entrega)
data$year_treated_100p = ifelse(is.na(as.numeric(data$Fecha_entrega))==F, as.numeric(data$Fecha_entrega), NaN)
table(data$year_treated_100p)
########################################
data$year = as.numeric(data$ANIO)   
data = subset(data, is.na(data$year)==F) 
# 
# 
data$time_to_treat_ic = data$year -  data$year_treated_ic
data$time_to_treat_ai = data$year -  data$year_treated_ai
data$time_to_treat_10p = data$year -  data$year_treated_10p
data$time_to_treat_50p = data$year -  data$year_treated_50p
data$time_to_treat_100p = data$year -  data$year_treated_100p
# Crear variables de tratamiento binarias
data$treat_ic <- ifelse(data$year >= data$year_treated_ic, 1, 0)
data$treat_ai <- ifelse(data$year >= data$year_treated_ai, 1, 0)
data$treat_10p <- ifelse(data$year >= data$year_treated_10p, 1, 0)
data$treat_50p <- ifelse(data$year >= data$year_treated_50p, 1, 0)
data$treat_100p <- ifelse(data$year >= data$year_treated_100p, 1, 0)
table(is.na(data$treat_ic))

# summary( na.omit(data[  data$time_to_treat_100p <= max_min, ]$delta_0p_100p ))

# Filtrar los datos según la ventana de tiempo especificada
max_min = 20
data_ic <- data[data$time_to_treat_ic >= -max_min & data$time_to_treat_ic <= max_min, ]
data_ic <- data_ic[is.na(data_ic$year_treated_ic) == F, ]
table(data_ic$time_to_treat_ic)
table(is.na(data_ic$year_treated_ic) )

data_ai <- data[data$time_to_treat_ai >= -max_min & data$time_to_treat_ai <= max_min, ]
data_ai <- data_ai[is.na(data_ai$year_treated_ai)==F, ]
table(data_ai$time_to_treat_ai)
table(is.na(data_ai$year_treated_ai) )

# data_10p <-data[is.na(data$year_treated_10p)==F, ]
data_10p <- data[data$time_to_treat_10p >= -max_min & data$time_to_treat_10p <= max_min, ]
data_10p <-data_10p[is.na(data_10p$year_treated_10p)==F, ]
table(data_10p$time_to_treat_10p)
table(is.na(data_10p$year_treated_10p) )

data_50p <- data[data$time_to_treat_50p >= -max_min & data$time_to_treat_50p <= max_min, ]
data_50p <- data_50p[is.na(data_50p$year_treated_50p)==F,]
table(data_50p$time_to_treat_50p)
table(is.na(data_50p$year_treated_50p) )

# data_100p <- data[data$time_to_treat_100p >= -max_min & data$time_to_treat_100p <= max_min, ]
data_100p <- data[  data$time_to_treat_100p <= max_min, ]
data_100p <- data_100p[is.na(data_100p$year_treated_100p)==F,]
table(data_100p$time_to_treat_100p)
table(is.na(data_100p$year_treated_100p) )
######


tablas = c( 'data_ai','data_10p','data_50p', 'data_100p')
################# Distances

buffers = c(500,1000,1500,2000,2500)#, unique(data$buffer_km)
buffer_heterogenities = data.frame()
for (j in tablas) {
    print(j)
  
    tabla_ = get(j)
    # Determine the value of m based on the suffix
    if (grepl("ai$", j)) {
      tabla_$year_treated = tabla_[['year_treated_ai']]
      ref_point =  c(  -1:10 ) 
    } else if (grepl("10p$", j)) {
      tabla_$year_treated = tabla_[['year_treated_10p']]
      ref_point =  c(  -5:7 ) 
    } else if (grepl("50p$", j)) {
      tabla_$year_treated = tabla_[['year_treated_50p']]
      ref_point =  c(  -6:6 ) 
    } else if (grepl("100p$", j)) {
      tabla_$year_treated = tabla_[['year_treated_100p']]
      ref_point =  c(  -11:0 ) 
    }  
   

    Sun_Abraham_Modelos =   paste0('SA_reading_naturaleza_',  j)
    

    assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)

    for (i in buffers) {
 
      print(i)
      print(Sun_Abraham_Modelos)
      df = tabla_
      temo_tabla_buffer = subset(df,df$buffer_km==i    )
      number_schools = length(unique(temo_tabla_buffer$cole_cod_dane_institucion))
      avg_num_estudents_by_school = (length(unique(temo_tabla_buffer$ESTU_CONSECUTIVO)) / length(unique(temo_tabla_buffer$ANIO))) / number_schools
      ####### Modelos
      summary(subset(df,df$buffer_km==i    )$DISTANCE) 
      MODEL_SA_math = feols(sd_math_i  ~  sunab(year_treated, year  ,  ref.p = ref_point ) #+ ## The only thing that's changed
                        | cole_cod_dane_institucion+  year,
                        cluster = ~cole_cod_dane_institucion+NOMBRE,                            ## Clustered SEs
                        data = temo_tabla_buffer )
      MODEL_SA_reading = feols(sd_reading_i  ~  sunab(year_treated, year  ,  ref.p = ref_point ) #+ ## The only thing that's changed
                            | cole_cod_dane_institucion+  year,
                            cluster = ~cole_cod_dane_institucion+NOMBRE,                            ## Clustered SEs
                            data = temo_tabla_buffer )
      ###print SA######################
      est_1 =   paste0('SA_', i,  '_',summary(MODEL_SA_math)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
      est_2 =   paste0('SA_', i, '_', summary(MODEL_SA_reading)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
      print(paste0('buffer ', i))
      # print( etable(MODEL_SA_math,MODEL_SA_reading ,tex = F ) )
      temp = data.frame(
        'tabla' = j, 
        'buffer' = i,
        'math_att_estimation' =  round(summary(MODEL_SA_math, agg = "ATT")[["coeftable"]][1, 1], 4), 
        'math_att_sd' =  round(summary(MODEL_SA_math, agg = "ATT")[["coeftable"]][1, 2], 4), 
        'math_att_pv' =  round(summary(MODEL_SA_math, agg = "ATT")[["coeftable"]][1, 4], 4), 
        'reading_att_estimation' =  round(summary(MODEL_SA_reading, agg = "ATT")[["coeftable"]][1, 1], 4), 
        'reading_att_sd' =  round(summary(MODEL_SA_reading, agg = "ATT")[["coeftable"]][1, 2], 4),
        'reading_att_pv' =  round(summary(MODEL_SA_reading, agg = "ATT")[["coeftable"]][1, 4], 4),
        'Number_schools' = number_schools,
        'Avg_num_students_by_school' = avg_num_estudents_by_school
      )
      
      buffer_heterogenities = rbind(buffer_heterogenities ,temp)
       
      print(est_1)
      print(summary(MODEL_SA_math, agg = "ATT") )
      print(est_2)
      print(summary(MODEL_SA_reading, agg = "ATT") )
      
      #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
    }

    }

head(buffer_heterogenities, 40)


tabla = buffer_heterogenities
# Convert buffer to a factor with levels to maintain order
tabla$buffer <- factor(as.numeric(tabla$buffer), levels = sort( unique(as.numeric(tabla$buffer))) )
# tabla$buffer <- factor(tabla$buffer, levels = sort(unique(tabla$buffer)))

# Convert tabla values to percentage completion and set order
tabla <- tabla %>%
  mutate(`Percentage of Road Completion` = factor(
    case_when(
      tabla == "data_ai"   ~ "0% completion",
      tabla == "data_10p"  ~ "10% completion",
      tabla == "data_50p"  ~ "50% completion",
      tabla == "data_100p" ~ "100% completion"
    ),
    levels = c("0% completion", "10% completion", "50% completion", "100% completion")  # Order of the facets
  ))

head(tabla, 40)

# Scatter plot with error bars
ggplot(tabla, aes(x = buffer, y = math_att_estimation, color = `Percentage of Road Completion`, shape = `Percentage of Road Completion`)) +
  geom_point() +
  geom_errorbar(aes(ymin = math_att_estimation - math_att_sd, ymax = math_att_estimation + math_att_sd), width = 0.2) +
  geom_hline(yintercept = 0,linetype = "dashed", size = 0.5) +
  facet_wrap(~tabla, scales = "free") +  # Use this to create separate plots for math and reading
  labs(title = "ATT Estimations with Standard Deviations",
       x = "Buffer (meters)",
       y = "ATT Estimation") + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_line(color = "grey", size = 0.2),
    panel.grid.minor = element_blank()
  ) 
 
 
est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
#       png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
#       event_study_plot( SA_table( MODEL_SA  )   )
#      
png(paste0("Graph/math_att_heterogenities_by_distance.png"),  width = 1030, height = 598)
# Scatter plot with error bars and annotations
ggplot(tabla, aes(x = buffer, y = math_att_estimation, color = `Percentage of Road Completion`, shape = `Percentage of Road Completion`)) +
  geom_point() +
  geom_errorbar(aes(ymin = math_att_estimation - math_att_sd, ymax = math_att_estimation + math_att_sd), width = 0.03) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  facet_wrap(~ `Percentage of Road Completion`, scales = "free") +
  labs(title = "ATT Estimations with Standard Deviations",
       x = "Buffer (meters)",
       y = "ATT Estimation") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_line(color = "grey", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.position = "none" # Hide the legend
  ) +
  geom_text(aes(label = paste("Schools:", Number_schools, "\nStudents per School:", round(Avg_num_students_by_school, 0))),
            size = 4, vjust = 0.4, hjust = 0.5) # Adjust vjust to place text below x-axis


dev.off() 
#########   
png(paste0("Graph/reading_att_heterogenities_by_distance.png"),  width = 1030, height = 598)
# Scatter plot with error bars and annotations
ggplot(tabla, aes(x = buffer, y = reading_att_estimation, color = `Percentage of Road Completion`, shape = `Percentage of Road Completion`)) +
  geom_point() +
  geom_errorbar(aes(ymin = reading_att_estimation - reading_att_sd, ymax = reading_att_estimation + reading_att_sd), width = 0.03) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  facet_wrap(~ `Percentage of Road Completion`, scales = "free") +
  labs(title = "ATT Estimations with Standard Deviations",
       x = "Buffer (meters)",
       y = "ATT Estimation") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_line(color = "grey", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.position = "none" # Hide the legend
  ) +
  geom_text(aes(label = paste("Schools:", Number_schools, "\nStudents per School:", round(Avg_num_students_by_school, 0))),
            size = 4, vjust = 0.4, hjust = 0.5) # Adjust vjust to place text below x-axis


dev.off() 
#############################################

#############################################
# # ?geom_vline
# # Line plot for math ATT estimation
# # Faceted plot for both math and reading ATT estimations
# ggplot(tabla, aes(x = buffer, y = math_att_estimation, color = tabla)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = math_att_estimation - math_att_sd, ymax = math_att_estimation + math_att_sd), width = 0.2) +
#   facet_wrap(~ tabla, scales = "free_y") +
#   labs(title = "Math ATT Estimation by Data Group",
#        x = "Buffer (meters)",
#        y = "ATT Estimation") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# # ############### Naturaleza
# # 
# # 
# # 
# # 
# if (1==1){
# #   for (j in tablas) {
# #     print(j)
# #     tabla = j
# #     
# #     Sun_Abraham_Modelos =   paste0('SA_reading_naturaleza_',  j)
# #     lista_modelos = paste0('TWFE_reading_naturaleza_', j)
# #     
# #     assign( lista_modelos, list(), envir = .GlobalEnv)
# #     assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
# #     
# #     for (i in naturaleza) {
# #       
# #       if (i== "PUBL" ) {
# #         m = "PRIV"
# #       } else if (i == "PRIV" ) {
# #         m = "PUBL"
# #       } else {
# #         m = "All sample"
# #       }
# #       
# #       print(Sun_Abraham_Modelos)
# #       df = get(tabla)
# #       #df = df %>% subset(df$time_to_treat >= -8)
# #       ####### Modelos
# #       
# #       MODEL = feols(reading_c_sd_nat ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
# #                     | id_name + year,                               ## FEs
# #                     cluster = ~ id_name ,                                        ## Clustered SEs
# #                     data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
# #       
# #       MODEL_SA = feols( reading_c_sd_nat ~  sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1)) #+ ## The only thing that's changed
# #                         | id_name + year,                               ## FEs
# #                         cluster = ~ id_name ,                               ## Clustered SEs
# #                         data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
# #       ###print SA######################
# #       est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
# #       png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
# #       event_study_plot( SA_table( MODEL_SA  )   )
# #       dev.off() 
# #       #################################
# #  
# #       name_in_enviroment = paste0(  "Score at ", m, " Meters")
# #       
# #       #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
# #       if (j == 'base_10p') {
# #         TWFE_reading_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL) 
# #         SA_reading_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_50p') {
# #         TWFE_reading_naturaleza_base_50p[[name_in_enviroment]] <- (MODEL)
# #         SA_reading_naturaleza_base_50p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ic') {
# #         TWFE_reading_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL)
# #         SA_reading_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ent') {
# #         TWFE_reading_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL)  
# #         SA_reading_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL_SA)
# #       }  else if (j == 'base_ai') {
# #         TWFE_reading_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL)  
# #         SA_reading_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL_SA)
# #       } else {
# #         message('no esta en las tabkas')
# #       }
# #       #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
# #     }
# #     
# #     }
# #   
# # 
# #   
# #   for (j in tablas) {
# #     print(j)
# #     tabla = j
# #     
# #     Sun_Abraham_Modelos =   paste0('SA_math_naturaleza_',  j)
# #     lista_modelos = paste0('TWFE_math_naturaleza_', j)
# #     
# #     assign( lista_modelos, list(), envir = .GlobalEnv)
# #     assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
# #     
# #     
# #     for (i in naturaleza) {
# #       if (i== "PUBL" ) {
# #         m = "PRIV"
# #       } else if (i == "PRIV" ) {
# #         m = "PUBL"
# #       } else {
# #         m = "All sample"
# #       }
# #       
# #       print(Sun_Abraham_Modelos)
# #       df = get(tabla)
# #       df = df %>% subset(df$time_to_treat >= -8)
# #       ####### Modelos
# #       MODEL = feols(math_c_sd_nat ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
# #                     | id_name + year,                               ## FEs
# #                     cluster = ~ id_name ,                                    ## Clustered SEs
# #                     data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
# #       
# #       MODEL_SA = feols(  math_c_sd_nat ~  sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) ) #+ ## The only thing that's changed
# #                          | id_name + year,                               ## FEs
# #                          cluster = ~ id_name ,                                ## Clustered SEs
# #                         data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
# #       ###print SA######################
# #       est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
# #       png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
# #       event_study_plot( SA_table( MODEL_SA  )   )
# #       dev.off() 
# #       #################################
# #       
# #       name_in_enviroment = paste0(  "Score at ", m, " Meters")
# #       
# #       #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
# #       if (j == 'base_10p') {
# #         TWFE_math_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL) 
# #         SA_math_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_50p') {
# #         TWFE_math_naturaleza_base_50p[[name_in_enviroment]] <- (MODEL)
# #         SA_math_naturaleza_base_50p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ic') {
# #         TWFE_math_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL)
# #         SA_math_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ent') {
# #         TWFE_math_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL)  
# #         SA_math_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL_SA)
# #       }  else if (j == 'base_ai') {
# #         TWFE_math_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL)  
# #         SA_math_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL_SA)
# #       } else {
# #         message('no esta en las tabkas')
# #       }
# #       #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
# #     }
# #   }
# #   #####################################
# #   
# #   for (j in tablas) {
# #     print(j)
# #     tabla = j
# #     
# #     Sun_Abraham_Modelos =   paste0('SA_profes_preg_nat_',  j)
# #     lista_modelos = paste0('TWFE_profes_preg_nat_', j)
# #     
# #     assign( lista_modelos, list(), envir = .GlobalEnv)
# #     assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
# #     
# #     for (i in naturaleza) {
# #       if (i== "PUBL" ) {
# #         m = "PRIV"
# #       } else if (i == "PRIV" ) {
# #         m = "PUBL"
# #       } else {
# #         m = "All sample"
# #       }
# #       print(Sun_Abraham_Modelos)
# #       df = get(tabla)
# #       df = df %>% subset(df$time_to_treat >= -8)
# #       ####### Modelos estu_trabaja 
# #       MODEL = feols(TOTPRE_100kPROF_COL ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
# #                     | id_name + year,                               ## FEs
# #                     cluster = ~ id_name ,                                ## Clustered SEs
# #                     data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
# #       
# #       MODEL_SA = feols( TOTPRE_100kPROF_COL ~  sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1)) #+ ## The only thing that's changed
# #                         | id_name + year,                               ## FEs
# #                         cluster = ~ id_name ,                            ## Clustered SEs
# #                         data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
# #       ###print SA######################
# #       est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
# #       png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
# #       event_study_plot( SA_table( MODEL_SA  )   )
# #       dev.off() 
# #       #################################
# #       
# #       name_in_enviroment = paste0(  "Score at ", m, " Meters")
# #       
# #       #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
# #       if (j == 'base_10p') {
# #         TWFE_profes_preg_nat_base_10p[[name_in_enviroment]] <- (MODEL) 
# #         SA_profes_preg_nat_base_10p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ic') {
# #         TWFE_profes_preg_nat_base_ic[[name_in_enviroment]] <- (MODEL)
# #         SA_profes_preg_nat_base_ic[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_50p') {
# #         TWFE_profes_preg_nat_base_50p[[name_in_enviroment]] <- (MODEL)
# #         SA_profes_preg_nat_base_50p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ent') {
# #         TWFE_profes_preg_nat_base_ent[[name_in_enviroment]] <- (MODEL)  
# #         SA_profes_preg_nat_base_ent[[name_in_enviroment]] <- (MODEL_SA)
# #       }  else if (j == 'base_ai') {
# #         TWFE_profes_preg_nat_base_ai[[name_in_enviroment]] <- (MODEL)  
# #         SA_profes_preg_nat_base_ai[[name_in_enviroment]] <- (MODEL_SA)
# #       } else {
# #         message('no esta en las tabkas')
# #       }
# #       #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
# #     }
# #   }
# #   
# #   #####################################
# #   
# #   for (j in tablas) {
# #     print(j)
# #     tabla = j
# #     
# #     Sun_Abraham_Modelos =   paste0('SA_profes_nat_',  j)
# #     lista_modelos = paste0('TWFE_profes_nat_', j)
# #     
# #     assign( lista_modelos, list(), envir = .GlobalEnv)
# #     assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
# #     
# #     for (i in naturaleza) {
# #       if (i== "PUBL" ) {
# #         m = "PRIV"
# #       } else if (i == "PRIV" ) {
# #         m = "PUBL"
# #       } else {
# #         m = "All sample"
# #       }
# #       print(Sun_Abraham_Modelos)
# #       df = get(tabla)
# #       df = df %>% subset(df$time_to_treat >= -8)
# #       ####### Modelos estu_trabaja 
# #        
# #       MODEL = feols(TOTPROF_100kPROF_COL ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
# #                     | id_name + year,                               ## FEs
# #                     cluster = ~ id_name ,                                ## Clustered SEs
# #                     data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
# #       
# #       MODEL_SA = feols( TOTPROF_100kPROF_COL ~  sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1)) #+ ## The only thing that's changed
# #                         | id_name + year,                               ## FEs
# #                         cluster = ~ id_name ,                            ## Clustered SEs
# #                         data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
# #       ###print SA######################
# #       est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
# #       png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
# #       event_study_plot( SA_table( MODEL_SA  )   )
# #       dev.off() 
# #       #################################
# #       
# #       name_in_enviroment = paste0(  "Score at ", m, " Meters")
# #       
# #       #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
# #       if (j == 'base_10p') {
# #         TWFE_profes_nat_base_10p[[name_in_enviroment]] <- (MODEL) 
# #         SA_profes_nat_base_10p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ic') {
# #         TWFE_profes_nat_base_ic[[name_in_enviroment]] <- (MODEL)
# #         SA_profes_nat_base_ic[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_50p') {
# #         TWFE_profes_nat_base_50p[[name_in_enviroment]] <- (MODEL)
# #         SA_profes_nat_base_50p[[name_in_enviroment]] <- (MODEL_SA)
# #       } else if (j == 'base_ent') {
# #         TWFE_profes_nat_base_ent[[name_in_enviroment]] <- (MODEL)  
# #         SA_profes_nat_base_ent[[name_in_enviroment]] <- (MODEL_SA)
# #       }  else if (j == 'base_ai') {
# #         TWFE_profes_nat_base_ai[[name_in_enviroment]] <- (MODEL)  
# #         SA_profes_nat_base_ai[[name_in_enviroment]] <- (MODEL_SA)
# #       } else {
# #         message('no esta en las tabkas')
# #       }
# #       #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
# #     }
# #   }
# #   
# #    ###################################################
#   
#   tablas = c( 'base_ai','base_10p', 'base_50p', 'base_ic')
#   for (j in tablas) {
#     print(j)
#     tabla = j
#     
#     Sun_Abraham_Modelos =   paste0('SA_labor_force_',  j)
#     lista_modelos = paste0('TWFE_labor_force_', j)
#     
#     assign( lista_modelos, list(), envir = .GlobalEnv)
#     assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
#     
#     for (i in naturaleza) {
#       if (i== "PUBL" ) {
#         m = "PRIV"
#       } else if (i == "PRIV" ) {
#         m = "PUBL"
#       } else {
#         m = "All sample"
#       }
#       print(Sun_Abraham_Modelos)
#       df = get(tabla)
#       df = df %>% subset(df$time_to_treat >= -8)
#       ####### Modelos estu_trabaja 
#       MODEL = feols(frac_trabaja_sd ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
#                     | id_name + year,                               ## FEs
#                     cluster = ~ id_name ,                                ## Clustered SEs
#                     data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
#       
#       MODEL_SA = feols( frac_trabaja_sd ~  sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) ) #+ ## The only thing that's changed
#                         | id_name + year,                               ## FEs
#                         cluster = ~ id_name ,                            ## Clustered SEs
#                         data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
#       
#       ###print SA######################
#       est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
#       png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
#       event_study_plot( SA_table( MODEL_SA  )   )
#       dev.off() 
#       #################################
#       
#       name_in_enviroment = paste0(  "Score at ", m, " Meters")
#       
#       #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
#       if (j == 'base_10p') {
#         TWFE_labor_force_base_10p[[name_in_enviroment]] <- (MODEL) 
#         SA_labor_force_base_10p[[name_in_enviroment]] <- (MODEL_SA)
#       } else if (j == 'base_50p') {
#         TWFE_labor_force_base_50p[[name_in_enviroment]] <- (MODEL)
#         SA_labor_force_base_50p[[name_in_enviroment]] <- (MODEL_SA)
#       } else if (j == 'base_ic') {
#         TWFE_labor_force_base_ic[[name_in_enviroment]] <- (MODEL)
#         SA_labor_force_base_ic[[name_in_enviroment]] <- (MODEL_SA)
#       } else if (j == 'base_ent') {
#         TWFE_labor_force_base_ent[[name_in_enviroment]] <- (MODEL)  
#         SA_labor_force_base_ent[[name_in_enviroment]] <- (MODEL_SA)
#       }  else if (j == 'base_ai') {
#         TWFE_labor_force_base_ai[[name_in_enviroment]] <- (MODEL)  
#         SA_labor_force_base_ai[[name_in_enviroment]] <- (MODEL_SA)
#       } else {
#         message('no esta en las tabkas')
#       }
#       #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
#     }
#     
#   }
#   
#   
# }
# # 
# # tablas = c( 'base_ai','base_10p', 'base_50p', 'base_ic', 'base_ent')
# # 
# # for (j in tablas) {
# #   print(j)
# #   tabla = j
# #   
# #   
# #   Sun_Abraham_Modelos =   paste0('SA_labor_force_naturaleza_',  j)
# #   lista_modelos = paste0('TWFE_labor_force_naturaleza_', j)
# #   
# #   assign( lista_modelos, list(), envir = .GlobalEnv)
# #   assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
# #   
# #   for (i in naturaleza) {
# #     
# #     if (i== "PUBL" ) {
# #       m = "PRIV"
# #     } else if (i == "PRIV" ) {
# #       m = "PUBL"
# #     } else {
# #       m = "All sample"
# #     }
# #     print(i)
# #     df = get(tabla)
# #     df = df %>% subset(df$time_to_treat >= -8)
# #     ####### Modelos
# #     
# #     MODEL = feols(estu_trabaja ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
# #                   | id_name + year,                               ## FEs
# #                   cluster = ~ id_name ,                               ## Clustered SEs
# #                   data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i   ) )
# #     
# #     MODEL_SA = feols( estu_trabaja ~  sunab(year_treated_sa, year , ref.p = c(.F + -2:2, -1) ) #+ ## The only thing that's changed
# #                       | id_name + year,                               ## FEs
# #                       cluster = ~ id_name ,                            ## Clustered SEs
# #                       data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  ) )
# #     
# #     ###print SA######################
# #     est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
# #     png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
# #     event_study_plot( SA_table( MODEL_SA  )   )
# #     dev.off() 
# #     #################################
# #     
# #     name_in_enviroment = paste0(  "Score at ", m, " Meters")
# #     
# #     #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
# #     if (j == 'base_10p') {
# #       TWFE_labor_force_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL) 
# #       SA_labor_force_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL_SA)
# #     } else if (j == 'base_50p') {
# #       TWFE_labor_force_naturaleza_base_50p[[name_in_enviroment]] <- (MODEL)
# #       SA_labor_force_naturaleza_base_50p[[name_in_enviroment]] <- (MODEL_SA)
# #     } else if (j == 'base_ic') {
# #       TWFE_labor_force_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL)
# #       SA_labor_force_naturaleza_base_ic[[name_in_enviroment]] <- (MODEL_SA)
# #     } else if (j == 'base_ent') {
# #       TWFE_labor_force_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL)  
# #       SA_labor_force_naturaleza_base_ent[[name_in_enviroment]] <- (MODEL_SA)
# #     }  else if (j == 'base_ai') {
# #       TWFE_labor_force_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL)  
# #       SA_labor_force_naturaleza_base_ai[[name_in_enviroment]] <- (MODEL_SA)
# #     } else {
# #       message('no esta en las tabkas')
# #     }
# #     #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
# #   }
# #   
# # }
# # 
# # ###########################################
# # time_treat = c('50p','10p','ai', 'ic', 'ent')
# # 
# # for (i in time_treat){
# #   print(i)
# #   lista_modelo = list('Private schools' = get(paste0("SA_reading_naturaleza_base_", i))[["Score at PRIV Meters"]] ,
# #                       'Public schools' = get(paste0("SA_reading_naturaleza_base_", i))[["Score at PUBL Meters"]],
# #                       'All sample schools' = get(paste0("SA_Reading_base_", i))[["Score at 1000 Meters"]])
# #   # etable(  lista_modelo , tex = TRUE)
# #   print(etable(  lista_modelo  ))
# #  
# #   
# # } 
# # 
# # for (i in time_treat){
# #   print(i)
# #   lista_modelo = list('Private schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PRIV Meters"]] ,
# #                       'Public schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PUBL Meters"]],
# #                       'All sample schools' = get(paste0("SA_Math_base_", i))[["Score at 1000 Meters"]])
# #   # etable(  lista_modelo , tex = TRUE)
# #   print(etable(  lista_modelo  ))
# #   
# #   
# # } 
# 
# ##############################
# time_treat = c('50p','10p','ai', 'ic', 'ent')
# 
#  
# 
# for (i in time_treat){
#   lista_modelo = list('Private schools' = get(paste0("SA_reading_naturaleza_base_", i))[["Score at PRIV Meters"]] ,
#                       'Public schools' = get(paste0("SA_reading_naturaleza_base_", i))[["Score at PUBL Meters"]],
#                       'All sample schools' = get(paste0("SA_Reading_base_", i))[["Score at 1000 Meters"]])
#   etable(  lista_modelo , tex = TRUE)
#   etable(  lista_modelo  )
#   png(paste0("graph/", "SA_reading_score_by_nature_",i,".png"),  width = 1030, height = 598)
#   event_study_plot(results_by_buffer(lista_modelo), seperate = F  ) 
#   dev.off()
#   
# } 
# 
# ###########################################
# time_treat = c('50p','10p','ai', 'ic', 'ent')
# for (i in time_treat){
#   # tryCatch( {
#    lista_modelo = list('Private schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PRIV Meters"]],
#                       'Public schools' = get(paste0("SA_math_naturaleza_base_", i))[["Score at PUBL Meters"]],
#                       'All sample schools' = get(paste0("SA_Math_base_", i))[["Score at 1000 Meters"]] )
#   
#   etable(  lista_modelo )
#   
#   png(paste0("graph/", "SA_math_score_by_nature_",i,".png"),  width = 1030, height = 598)
#   event_study_plot(results_by_buffer(lista_modelo), seperate = F  ) 
#  
#   dev.off()
#   # }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")} )
# } 
# ###########################################
#  
# for (i in time_treat){
#   tryCatch( {
#   lista_modelo = list('Private schools' = get(paste0("SA_labor_force_base_", i))[["Score at PRIV Meters"]],
#                       'Public schools' = get(paste0("SA_labor_force_base_", i))[["Score at PUBL Meters"]],
#                       'All sample schools' = get(paste0("SA_labor_force_base_", i))[["Score at All sample Meters"]] )
#   etable(  lista_modelo )
#   
#    png(paste0("graph/", "SA_labor_force_by_nature_",i,".png"),  width = 1030, height = 598)
#   event_study_plot(results_by_buffer(lista_modelo), seperate = F,
#                    TITULO= 'Sun and Abraham (2020):\n Heterogeneities in labor force by nature of the school' ) 
#   
#    dev.off()
#   }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
#   )
#   
# } 
# 
# ###########################################
# time_treat = c('50p','10p','ai', 'ic', 'ent')
#  
# for (i in time_treat){
#   tryCatch( {
#     lista_modelo = list('Private schools' = get(paste0("SA_profes_preg_nat_base_", i))[["Score at PRIV Meters"]],
#                         'Public schools' = get(paste0("SA_profes_preg_nat_base_", i))[["Score at PUBL Meters"]],
#                         'All sample schools' = get(paste0("SA_profes_preg_nat_base_", i))[["Score at All sample Meters"]] )
#     etable(  lista_modelo )
#     
#     png(paste0("graph/", "SA_profes_preg_by_nature_",i,".png"),  width = 1030, height = 598)
#     event_study_plot(results_by_buffer(lista_modelo), seperate = F,
#                      TITULO= 'Sun and Abraham (2020):\n Heterogeneities in the fraction of teachers with some university education' ) 
#     
#     dev.off()
#   }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
#   )
#   
# } 
# 
# for (i in time_treat){
#   tryCatch( {
#     lista_modelo = list('Private schools' = get(paste0("SA_profes_nat_base_", i))[["Score at PRIV Meters"]],
#                         'Public schools' = get(paste0("SA_profes_nat_base_", i))[["Score at PUBL Meters"]],
#                         'All sample schools' = get(paste0("SA_profes_nat_base_", i))[["Score at All sample Meters"]] )
#     etable(  lista_modelo )
#     
#     # png(paste0("graph/", "SA_profes_preg_by_nature_",i,".png"),  width = 1030, height = 598)
#     event_study_plot(results_by_buffer(lista_modelo), seperate = F,
#                      TITULO= 'Sun and Abraham (2020):\n Heterogeneities in the fraction of teachers in a school \n in relation to all teachers in Colombia' ) 
#     
#     # dev.off()
#   }, error=function(e){cat("ERROR CATCH: ",conditionMessage(e), "\n")}
#   )
#   
# } 
# ###########################################
# 
# 
# ################################
# event_study_plot(results_by_buffer(SA_labor_force_base_10p), 
#                  seperate = T, 
#                  TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 
# 
# ######################################################################################################
# #human capital acum
# png(paste0("graph/", "SA_estu_trabaja_base_10p.png"),  width = 1030, height = 598)
# event_study_plot(results_by_buffer(SA_estu_trabaja_base_10p[1:6]), 
#                  seperate = T, 
#                  TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 
# 
# dev.off()
# 
# png(paste0("graph/", "SA_estu_trabaja_base_50p.png"),  width = 1030, height = 598)
# event_study_plot(results_by_buffer(SA_estu_trabaja_base_50p[1:6]), 
#                  seperate = T, 
#                  TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 
# 
# dev.off()
# 
# 
# png(paste0("graph/", "SA_estu_trabaja_base_ai.png"),  width = 1030, height = 598)
# event_study_plot(results_by_buffer(SA_estu_trabaja_base_ai[1:6]), 
#                  seperate = T, 
#                  TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 
# dev.off()
# 
# ###################
# 
# png(paste0("graph/", "SA_estu_trabaja_base_ic.png"),  width = 1030, height = 598)
# event_study_plot(results_by_buffer(SA_estu_trabaja_base_ic[1:6]), 
#                  seperate = T, 
#                  TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 
# dev.off()
# 
# png(paste0("graph/", "SA_estu_trabaja_base_ent.png"),  width = 1030, height = 598)
# event_study_plot(results_by_buffer(SA_estu_trabaja_base_ent[1:6]), 
#                  seperate = T, 
#                  TITULO= 'Sun and Abraham (2020):\n Labor force participation ' ) 
# 
# dev.off()
# 
# ###########################################################################################################################
# # LABOR FORCE PARTICIPATION
# ####
# base_10p$frac_trabaja_sd
# 
# Callaway = Callaway_table(buffer = 1000,  tabla= base_10p , anticipation =0,  yname ='frac_trabaja_sd' )
# 
# #Math_Gardner_ic = Gardner_table( yname = 'math_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 
# tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_estu_trabaja_base_10p[[1]] ),
#                                 SA_table(  SA_estu_trabaja_base_10p[[1]] )
#                                 , Callaway)) 
# 
# png(paste0("graph/labor_force_10p",".png"),  width = 1030, height = 598)
# event_study_plot(  tabla , seperate = F, TITULO = 'Labor force participation' )   
# dev.off() 
# ########## RESULTS ALL BUFFES AT 10P
# png(paste0("graph/SA_labor_force_10p_ALL_BUFFER.png"),  width = 1030, height = 598)
# event_study_plot(  results_by_buffer(  SA_estu_trabaja_base_10p   ) , 
#                    seperate = T, 
#                    TITULO = 'Sun and Abraham (2020)\n Labor force participation' )
# 
# dev.off() 
# ########## RESULTS ALL BUFFES AT AI
# 
# png(paste0("graph/SA_reading_score_by_labor_force_ai"),  width = 1030, height = 598)
# event_study_plot(  results_by_buffer(  SA_estu_trabaja_base_ai   ) , 
#                    seperate = T, 
#                    TITULO = 'Sun and Abraham (2020)\n Labor force participation' )
# 
# dev.off() 
# 
# #########################################
# 
# time_treat = c('10p','ai','ent', 'ic')
# for (i in time_treat){
#   lista_modelo = list('Private schools' = get(paste0("SA_labor_force_base_", i))[["Score at PRIV Meters"]],
#                       'Public schools' = get(paste0("SA_labor_force_base_", i))[["Score at PUBL Meters"]],
#                       'All sample schools' = get(paste0("SA_estu_trabaja_base_", i))[[1]])
#   etable(  lista_modelo )
#   
#   png(paste0("graph/", "SA_natu_labor_force_base",i,".png"),  width = 1030, height = 598)
#   event_study_plot(results_by_buffer(lista_modelo), seperate = F, TITULO= 'Sun and Abraham (2020):\n Heterogeneities in Fraction of students from schools who just to work\n by nature of the school' ) 
#   
#   
#   dev.off()
#   
# } 
# 
# 
# 
# 
# 
# #########################################
# # UNIVERSITARIES STUDIES PARTICIPATION
# ####
# 
# ########## RESULTS ALL BUFFES AT IC
# Callaway_ = Callaway_table(buffer = 1000,  
#                            tabla= subset(base_ic,base_ic$time_to_treat >= -8 & base_ic$year <= 2013  ) , 
#                            anticipation =0,  yname ='finished_uni' )
# Callaway_
# #Math_Gardner_ic = Gardner_table( yname = 'math_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 
# tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_Participate_saberpro_base_ic[["Score at 1000 Meters"]] ),
#                                 SA_table(  SA_Participate_saberpro_base_ic[["Score at 1000 Meters"]] )
#                                 , Callaway_))
# png(paste0("graph/Participate_saberpro_ic",".png"),  width = 1030, height = 598)
# event_study_plot(  tabla , seperate = F,
#                    TITULO = 'Fraction of students who have completed any grade of university' )
# dev.off() 
# 
# ########## RESULTS ALL BUFFES AT AI
# #Math_Gardner_ic = Gardner_table( yname = 'math_c', df = subset(base_ic, base_ic$buffer_km == 1000) ) 
# tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_Participate_saberpro_base_ai[["Score at 1000 Meters"]] ),
#                                 SA_table(  SA_Participate_saberpro_base_ai[["Score at 1000 Meters"]] )
#                                 , 
#                                 Callaway_table(buffer = 1000,  
#                                                tabla= subset(base_ai,base_ai$time_to_treat >= -8 & base_ai$year <= 2013  ) , 
#                                                anticipation =0,  yname ='finished_uni' )
# ))
# png(paste0("graph/Participate_saberpro_ai",".png"),  width = 1030, height = 598)
# event_study_plot(  tabla , seperate = F,
#                    TITULO = 'Fraction of students who have completed any grade of university' )
# dev.off() 
# 
# ########## RESULTS ALL BUFFES AT 10P
# tabla =  do.call("rbind", list( TWFE_table('TWFE' , TWFE_Participate_saberpro_base_10p[["Score at 1000 Meters"]] ),
#                                 SA_table(  SA_Participate_saberpro_base_10p[["Score at 1000 Meters"]] )
#                                 , 
#                                 Callaway_table(buffer = 1000,  
#                                                tabla= subset(base_10p,base_10p$time_to_treat >= -8 & base_10p$year <= 2013  ) , 
#                                                anticipation =0,  yname ='finished_uni' )
# ))
# png(paste0("graph/Participate_saberpro_10p",".png"),  width = 1030, height = 598)
# event_study_plot(  tabla , seperate = F,
#                    TITULO = 'Fraction of students who have completed any grade of university' )
# dev.off() 
# 
# ###
# event_study_plot(tabla)
# time_treat = c('10p','ai',  'ic')
# for (j in time_treat) {
#   print(j)
#   tabla = j
#   
#   Sun_Abraham_Modelos =   paste0('SA_natu_Participate_saberpro_base_',  j)
#   lista_modelos = paste0('TWFE_natu_Participate_saberpro_base_', j)
#   
#   assign( lista_modelos, list(), envir = .GlobalEnv)
#   assign( Sun_Abraham_Modelos, list(), envir = .GlobalEnv)
#   
#   for (i in naturaleza) {
#     print(Sun_Abraham_Modelos)
#     df = get(paste0("base_", tabla))
#     #df = get(paste0("base_", "10p"))
#     #i="PUBL"
#     df = df %>% subset(df$time_to_treat >= -8)
#     ####### Modelos
#     MODEL = feols(finished_uni ~ i(time_to_treat, treat_, ref = reference_time) # + ## Our key interaction: time × treatment status
#                   |  cole_cod_dane+ year,                             ## FEs
#                   cluster = ~ cole_cod_dane,                              ## Clustered SEs
#                   data = subset(df, df$buffer_km==1000  & df$cole_naturaleza != i  & df$year <= 2013 ) )
#     
#     MODEL_SA = feols( finished_uni ~  sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1)) #+ ## The only thing that's changed
#                       |  cole_cod_dane+ year,                             ## FEs
#                       cluster = ~ cole_cod_dane,                         ## Clustered SEs
#                       data = subset(df,df$buffer_km==1000  & df$cole_naturaleza != i  & df$year <= 2013) )
#     
#     ###print SA######################
#     est =   paste0('SA_', i, summary(MODEL_SA)[["call"]][["fml"]][[2]]  , "_Score at ", i, " Meters")
#     png(paste0("graph/",tabla,'_',est,".png"),  width = 1030, height = 598)
#     event_study_plot( SA_table( MODEL_SA  )   )
#     dev.off() 
#     #################################
#     
#     name_in_enviroment = paste0(  "Score at ", m, " Meters")
#     
#     #assign(name_in_enviroment, (MODEL)  , envir = .GlobalEnv)
#     if (j == '10p') {
#       TWFE_natu_Participate_saberpro_base_10p[[name_in_enviroment]] <- (MODEL) 
#       SA_natu_Participate_saberpro_base_10p[[name_in_enviroment]] <- (MODEL_SA)
#     } else if (j == 'ic') {
#       TWFE_natu_Participate_saberpro_base_ic[[name_in_enviroment]] <- (MODEL)
#       SA_natu_Participate_saberpro_base_ic[[name_in_enviroment]] <- (MODEL_SA)
#     } else if (j == 'ent') {
#       warning(pass)
#     }  else if (j == 'ai') {
#       TWFE_natu_Participate_saberpro_base_ai[[name_in_enviroment]] <- (MODEL)  
#       SA_natu_Participate_saberpro_base_ai[[name_in_enviroment]] <- (MODEL_SA)
#     } else {
#       message('no esta en las tabkas')
#     }
#     #assign(paste0(lista_modelos,'[[',name_in_enviroment,']]') , list(MODEL)  , envir = .GlobalEnv)
#   }
# }
# 
# png(paste0("graph/SA_Participate_saberpro_base_ic_ALL_BUFFER_",".png"),  width = 1030, height = 598)
# 
# event_study_plot(  results_by_buffer(  SA_Participate_saberpro_base_ic   ) , 
#                    seperate = T, 
#                    TITULO = 'Sun and Abraham (2020)\n Fraction of students who have completed any grade of university' )
# dev.off() 
# 
# 
# for (i in time_treat){
#   
#   lista_modelo = list('Private schools' = get(paste0("SA_natu_Participate_saberpro_base_", i))[["Score at PRIV Meters"]],
#                       'Public schools' = get(paste0("SA_natu_Participate_saberpro_base_", i))[["Score at PUBL Meters"]],
#                       'All sample schools' = get(paste0("SA_Participate_saberpro_base_", i))[[1]])
#   etable(  lista_modelo )
#   
#   png(paste0("graph/", "SA_natu_Participate_saberpro_base_",i,".png"),  width = 1030, height = 598)
#   event_study_plot(results_by_buffer(lista_modelo), seperate = F, TITULO= 'Sun and Abraham (2020):\n Heterogeneities in Fraction of students who have completed any grade of university\n by nature of the school' ) 
#   
#   
#   dev.off()
#   
# } 
# time_treat = c('10p','ai','ent', 'ic')
# 
# 
# 
# #setwd("C:/Users/USER/Desktop/DID roads/Data/")
# 
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# 
















