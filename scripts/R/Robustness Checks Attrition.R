
############################################################
# \subsubsection{Attrition: Analyzing Changes in Student Enrollment}
############################################################

 

# Define the list of tables and their corresponding suffixes
tablas = c('data_ai', 'data_10p', 'data_50p', 'data_100p')
modelos = c()

data_ref = data_ai[data_ai$time_to_treat_ai ==-2, ]

data_ref_stagg <- data_ref %>%
  group_by(cole_cod_dane_institucion) %>%
  summarise( 
    tot_estu_pre = n_distinct(ESTU_CONSECUTIVO) 
  )
data_ref_stagg$sd_tot_estu_pre =  sd(  data_ref_stagg$tot_estu_pre )
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
  tabla_ <- subset(tabla_, is.na(as.numeric(tabla_$fami_estrato_vivienda))==F   )
  tabla_$estrato_1_2 = ifelse((tabla_$fami_estrato_vivienda) == 1 |tabla_$fami_estrato_vivienda == 2 , 1,0)
  
  tabla_$estrato_g3 = ifelse((tabla_$fami_estrato_vivienda) >= 3  , 1,0)
  
  # tabla_ <- subset(tabla_, as.numeric(tabla_$year_treated)<=2018  ) 
  base_stagg <- tabla_ %>%
    group_by(cole_cod_dane_institucion, ANIO, year_treated, time_to_treat, NOMBRE) %>%
    summarise(
      estrato_1_2 = sum(estrato_1_2),
      estrato_g3 =  sum(estrato_g3) ,
      Distance = mean(DISTANCE, na.rm = T),
      tot_estu = n_distinct(ESTU_CONSECUTIVO) 
      
    ) %>%
    rename(
      school_id = cole_cod_dane_institucion,
      year = ANIO,
      year_treated = year_treated,
      time_to_treatment = time_to_treat, 
      road_id = NOMBRE
    )
  base_stagg = sqldf::sqldf("SELECT *, (tot_estu-tot_estu_pre)/sd_tot_estu_pre as sd_tot_estu_dif, (tot_estu-tot_estu_pre)/tot_estu_pre enrollment_delta FROM base_stagg
               INNER JOIN data_ref_stagg
               ON cole_cod_dane_institucion = school_id")
  base_stagg$inse_index = ((base_stagg$estrato_1_2 / base_stagg$tot_estu) - base_stagg$sd_tot_estu_dif  ) / ((base_stagg$estrato_1_2 / base_stagg$tot_estu) + base_stagg$sd_tot_estu_dif  )
  # Fit the model
  sunab_inse <- feols(enrollment_delta ~  Distance+
                        sunab(year_treated, year, ref.p =ref_point) |
                        school_id + year,
                      cluster = ~school_id + road_id,
                      data = base_stagg)
  # Save the model in the global environment with a dynamic name
  model_name <- paste0("SA_inse_", suffix)
  print(model_name)
  title3 <- paste0("Impact of ",completion ," Road Concession Progress\n on Enrolment")
  # png(paste0("Graph/", generate_file_name(title3), ".png"), width = 1030, height = 598)
  event_study_plot_sunab_2021(SA_Result_table(sunab_inse), horizon = NULL, TITULO = title3)
  # dev.off()
  
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
file_path = paste0('Tables/', 'att_sunab_robustness_attrition', '.tex')

latex_table = (create_unified_table_sunab(tables_list, caption = "", label = "") )

# Write the table to a .tex file
sink(file_path)
cat(latex_table)
sink()

