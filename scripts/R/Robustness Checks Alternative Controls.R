options(scipen=999)
# ########################## loading full data ################################## 
# bq_auth(token = STEP1)
project_id <- "ph-jabri"
dataset_id <- "01_road_to_the_future"
table_id <- "BASE_ROADS_TO_THE_FUTURE"

# SELECT * FROM 
# where SB_PRO =  1 or SB_TYT = 1

# data = bigrquery::bq_table_download(
#   as_bq_table(
#   paste0(  project_id, '.' ,  dataset_id   , '.' ,table_id  )
#    ),
#   n_max  = Inf )
# 
# data = data[data$SB_PRO ==1 | data$SB_TYT ==1, ]

# arrow::write_parquet(data, 'Data/road_to_the_future.parquet')

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
data$treat = ifelse(data$DISTANCE <=1500, 1 ,  ifelse(data$DISTANCE >=3000, 0, NA  ) ) 
table()

data = subset(data, is.na(data$treat ) == F)
hist(data$DISTANCE)
data = subset(data, data$Fecha_inicio_consecion !=1994)
data = subset(data, as.numeric(data$ANIO) <=2021)
data = subset(data, data$cole_naturaleza =="PUBL")

sqldf::sqldf("SELECT   cole_naturaleza , COUNT(DISTINCT cole_cod_d) TOT FROM data group by 1")


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


############################################################
# \subsubsection{Alternative Control Groups}
############################################################

 



# Define the list of tables and their corresponding suffixes
tablas = c('data_ai', 'data_10p', 'data_50p', 'data_100p')
modelos = c()
tables_list <- list()

for (j in tablas) {
  print(j)
  
  # Get the data frame from the global environment
  tabla_ <- get(j)
  # tabla_ <- subset(tabla_, is.na(as.numeric(tabla_$ESTU_INSE_INDIVIDUAL))==F   ) #tabla_[as.numeric(tabla_$ANIO)<=2014 , ]
  tabla_$school_id =tabla_$cole_cod_dane_institucion 
  tabla_$road_id =tabla_$NOMBRE
  
  # Determine the value of m and reference point based on the suffix
  if (grepl("ai$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_ai']]
    tabla_$time_to_treat <- tabla_[['time_to_treat_ai']]
    ref_point <- c(-1:10)
    suffix <- "0p"
    completion = '0%'
  } else if (grepl("10p$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_10p']]
    tabla_$time_to_treat <- tabla_[['time_to_treat_10p']]
    ref_point <- c(-5:7)
    suffix <- "10p"
    completion = '10%'
  } else if (grepl("50p$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_50p']]
    tabla_$time_to_treat <- tabla_[['time_to_treat_50p']]
    ref_point <- c(-6:6)
    suffix <- "50p"
    completion = '50%'
  } else if (grepl("100p$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_100p']]
    
    tabla_$time_to_treat <- tabla_[['time_to_treat_100p']]
    ref_point <- c(-11:0)
    suffix <- "100p"
    completion = '100%'
  }  

  
  tabla_$year_treated <- ifelse(tabla_$treat == 0, 10000, tabla_$year_treated)
  tabla_$time_to_treat <- ifelse(tabla_$treat == 0, -1000, tabla_$time_to_treat)
  
 
  
  sunab_inse <- feols(sd_math_i ~   
                        sunab(year_treated, year, ref.p =ref_point) |
                        school_id + year,
                      cluster = ~school_id + road_id,
                      data = tabla_)
  # Save the model in the global environment with a dynamic name
  model_name <- paste0("SA_inse_", suffix)
  print(model_name)
  title3 <- paste0("Impact of ",completion ," Road Concession Progress on\n Math Score\n (Alternative Controls)")
  png(paste0("Graph/", generate_file_name(title3), ".png"), width = 1030, height = 598)
  event_study_plot_sunab_2021(SA_Result_table(sunab_inse), horizon = NULL, TITULO = title3)
  dev.off()
  
  modelos = c(modelos, paste0("SA_inse_", suffix))
  assign(model_name, sunab_inse, envir = .GlobalEnv)
  table = sunab_att_table(model_name)
  # Rename the value column based on the corresponding completion percentage
  table = gsub(pattern = "& Value ", replacement = paste0(completions[i], " Completion"), table)
  
  tables_list[[j]] <- table
  # Print summary of the model
  print(summary(sunab_inse, agg = "ATT"))
}

etable(SA_inse_0p, SA_inse_10p, SA_inse_50p, SA_inse_100p)
file_path = paste0('Tables/', 'att_sunab_robustness_alter_controls_math', '.tex')

latex_table = (create_unified_table_sunab(tables_list, caption = "", label = "") )

# Write the table to a .tex file
sink(file_path)
cat(latex_table)
sink()





# Define the list of tables and their corresponding suffixes
tablas = c('data_ai', 'data_10p', 'data_50p', 'data_100p')
modelos = c()
tables_list <- list()

for (j in tablas) {
  print(j)
  
  # Get the data frame from the global environment
  tabla_ <- get(j)
  # tabla_ <- subset(tabla_, is.na(as.numeric(tabla_$ESTU_INSE_INDIVIDUAL))==F   ) #tabla_[as.numeric(tabla_$ANIO)<=2014 , ]
  tabla_$school_id =tabla_$cole_cod_dane_institucion 
  tabla_$road_id =tabla_$NOMBRE
  
  # Determine the value of m and reference point based on the suffix
  if (grepl("ai$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_ai']]
    tabla_$time_to_treat <- tabla_[['time_to_treat_ai']]
    ref_point <- c(-1:10)
    suffix <- "0p"
    completion = '0%'
  } else if (grepl("10p$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_10p']]
    tabla_$time_to_treat <- tabla_[['time_to_treat_10p']]
    ref_point <- c(-5:7)
    suffix <- "10p"
    completion = '10%'
  } else if (grepl("50p$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_50p']]
    tabla_$time_to_treat <- tabla_[['time_to_treat_50p']]
    ref_point <- c(-6:6)
    suffix <- "50p"
    completion = '50%'
  } else if (grepl("100p$", j)) {
    tabla_$year_treated <- tabla_[['year_treated_100p']]
    
    tabla_$time_to_treat <- tabla_[['time_to_treat_100p']]
    ref_point <- c(-11:0)
    suffix <- "100p"
    completion = '100%'
  }  
  
  
  tabla_$year_treated <- ifelse(tabla_$treat == 0, 10000, tabla_$year_treated)
  tabla_$time_to_treat <- ifelse(tabla_$treat == 0, -1000, tabla_$time_to_treat)
  
  
  
  sunab_inse <- feols(sd_reading_i ~   
                        sunab(year_treated, year, ref.p =ref_point) |
                        school_id + year,
                      cluster = ~school_id + road_id,
                      data = tabla_)
  # Save the model in the global environment with a dynamic name
  model_name <- paste0("SA_inse_", suffix)
  print(model_name)
  title3 <- paste0("Impact of ",completion ," Road Concession Progress on\n Reading Score\n (Alternative Controls)")
  png(paste0("Graph/", generate_file_name(title3), ".png"), width = 1030, height = 598)
  event_study_plot_sunab_2021(SA_Result_table(sunab_inse), horizon = NULL, TITULO = title3)
  dev.off()
  
  modelos = c(modelos, paste0("SA_inse_", suffix))
  assign(model_name, sunab_inse, envir = .GlobalEnv)
  table = sunab_att_table(model_name)
  # Rename the value column based on the corresponding completion percentage
  table = gsub(pattern = "& Value ", replacement = paste0(completions[i], " Completion"), table)
  
  tables_list[[j]] <- table
  # Print summary of the model
  print(summary(sunab_inse, agg = "ATT"))
}

etable(SA_inse_0p, SA_inse_10p, SA_inse_50p, SA_inse_100p)
file_path = paste0('Tables/', 'att_sunab_robustness_alter_controls_reading', '.tex')

latex_table = (create_unified_table_sunab(tables_list, caption = "", label = "") )

# Write the table to a .tex file
sink(file_path)
cat(latex_table)
sink()
 