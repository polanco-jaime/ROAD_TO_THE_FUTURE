
#\subsection{Impact on Additional Educational Outcomes}
colnames(data_ai$ESTU_CONSECUTIVO)
library(dplyr)

library(dplyr)
library(fixest) # Assuming you are using the `fixest` package for the `feols` function

############################################################
#\subsubsection{Child Labor}
###########################################################

# Define the list of tables and their corresponding suffixes
tablas = c('data_ai', 'data_10p', 'data_50p', 'data_100p')
modelos = c()
tables_list <- list()
for (j in tablas) {
  print(j)
  
  # Get the data frame from the global environment
  tabla_ <- get(j)
   
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
  
  # Prepare the base data for the model
  base_stagg <- tabla_ %>%
    group_by(cole_cod_dane_institucion, ANIO, year_treated, time_to_treat, NOMBRE) %>%
    summarise(
      estu_trabaja = sum(ifelse(is.na(estu_trabaja), 0, estu_trabaja)),
      tot_estu = n_distinct(ESTU_CONSECUTIVO),
      child_labor_index = estu_trabaja / tot_estu
    ) %>%
    rename(
      school_id = cole_cod_dane_institucion,
      year = ANIO,
      year_treated = year_treated,
      time_to_treatment = time_to_treat, 
      road_id = NOMBRE
    )
  
  # Fit the model
  sunab_child_labor <- feols(child_labor_index ~  
                               sunab(year_treated, year, ref.p = ref_point) |
                               school_id + year,
                             cluster = ~school_id + road_id,
                             data = base_stagg)
  
  # Save the model in the global environment with a dynamic name
  model_name <- paste0("SA_cl_", suffix)
  
  title3 <- paste0("Impact of ",completion ," Road Concession Progress\n on Child Labor Index")
  png(paste0("Graph/", generate_file_name(title3), ".png"), width = 1030, height = 598)
  event_study_plot_sunab_2021(SA_Result_table(sunab_child_labor), horizon = NULL, TITULO = title3)
  dev.off()
  
  modelos = c(modelos, paste0("SA_cl_", suffix))
  assign(model_name, sunab_child_labor, envir = .GlobalEnv)
  table = sunab_att_table(model_name)
  # Rename the value column based on the corresponding completion percentage
  table = gsub(pattern = "& Value ", replacement = paste0(completions[i], " Completion"), table)
  
  tables_list[[j]] <- table
  # Print summary of the model
  print(summary(sunab_child_labor, agg = "ATT"))
}

 
file_path = paste0('Tables/', 'att_sunab_child_labor', '.tex')
 
latex_table = (create_unified_table_sunab(tables_list, caption = "", label = "") )

# Write the table to a .tex file
sink(file_path)
cat(latex_table)
sink() 

############################################################
#\subsubsection{Human Capital Accumulation}
############################################################


# Define the list of tables and their corresponding suffixes
tablas = c('data_ai', 'data_10p', 'data_50p', 'data_100p')
modelos = c()
table(tabla_$year_treated)
tables_list <- list()
for (j in tablas) {
  print(j)
  
  # Get the data frame from the global environment
  tabla_ <- get(j)
  # tabla_ <- subset(tabla_, as.numeric(tabla_$ANIO)<=2016  ) #tabla_[as.numeric(tabla_$ANIO)<=2014 , ]
  tabla_$TERCIARY  = ifelse( ifelse(is.na(tabla_$SB_PRO)==T, 0, tabla_$SB_PRO )  + ifelse(is.na(tabla_$SB_TYT)==T, 0, tabla_$SB_TYT ) >=1, 1,0)
  table(tabla_$TERCIARY)
  table(tabla_$SB_PRO)
  table(tabla_$SB_TYT)
  
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
  
  tabla_ <- subset(tabla_, as.numeric(tabla_$year_treated)<=2018  ) 
  # Prepare the base data for the model
  base_stagg <- tabla_ %>%
    group_by(cole_cod_dane_institucion, ANIO, year_treated, time_to_treat, NOMBRE) %>%
    summarise(
      SB_PRO = sum(ifelse(is.na(SB_PRO), 0, SB_PRO)),
      TERCIARY =  sum(ifelse(is.na(TERCIARY), 0, TERCIARY)),
      SB_TYT =  sum(ifelse(is.na(SB_TYT), 0, SB_TYT)),
      tot_estu = n_distinct(ESTU_CONSECUTIVO),
      huma_capital_index = sum(ifelse(is.na(TERCIARY), 0, TERCIARY)) /  n_distinct(ESTU_CONSECUTIVO),
      huma_capital_index_sb_pro = sum(ifelse(is.na(SB_PRO), 0, SB_PRO)) /  n_distinct(ESTU_CONSECUTIVO),
      huma_capital_index_sb_tyt = sum(ifelse(is.na(SB_TYT), 0, SB_TYT)) /  n_distinct(ESTU_CONSECUTIVO)
    ) %>%
    rename(
      school_id = cole_cod_dane_institucion,
      year = ANIO,
      year_treated = year_treated,
      time_to_treatment = time_to_treat, 
      road_id = NOMBRE
    )
  
  # Fit the model
  sunab_hca <- feols(huma_capital_index ~  
                               sunab(year_treated, year, ref.p = ref_point) |
                               school_id + year,
                             cluster = ~school_id + road_id,
                             data = base_stagg)
  # Save the model in the global environment with a dynamic name
  model_name <- paste0("SA_hca_", suffix)
  
  title3 <- paste0("Impact of ",completion ," Road Concession Progress\n on Human Capital Accumulation Index")
  png(paste0("Graph/", generate_file_name(title3), ".png"), width = 1030, height = 598)
  event_study_plot_sunab_2021(SA_Result_table(sunab_hca), horizon = NULL, TITULO = title3)
  dev.off()
  
  modelos = c(modelos, paste0("SA_hca_", suffix))
  assign(model_name, sunab_hca, envir = .GlobalEnv)
  table = sunab_att_table(model_name)
  # Rename the value column based on the corresponding completion percentage
  table = gsub(pattern = "& Value ", replacement = paste0(completions[i], " Completion"), table)
  
  tables_list[[j]] <- table
  # Print summary of the model
  print(summary(sunab_hca, agg = "ATT"))
}

etable(SA_hca_0p, SA_hca_10p, SA_hca_50p, SA_hca_100p)
file_path = paste0('Tables/', 'att_sunab_human_capita_accumulation', '.tex')
 
latex_table = (create_unified_table_sunab(tables_list, caption = "", label = "") )

# Write the table to a .tex file
sink(file_path)
cat(latex_table)
sink()