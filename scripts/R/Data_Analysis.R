


## Biased ATT
hist(data_$sd_math_i)
hist(data_$math_i)
summary(data_$sd_math_i)

hist(data_[data_$treat_==1, ]$sd_math_i)
hist(data_[data_$treat_==1, ]$math_i)
# summary(data_[data_$treat_==1, ]$sd_math_i)
# 
# summary(lm(data=data_,sd_math_i~ treat_) )
# 
# summary(lm(data=data_,sd_reading_i~ treat_) )
##########
# schools
##########
# colnames(data_)

# data_ =data_[ , c("sd_reading_i", "reading_i", "sd_math_i", "math_i"   , "treat_" , 
#                   'year_treated_100p', 'year_treated_50p', 'year_treated_10p', 'year_treated_ai', 'year_treated_ic',  "year" ,
#                   "cole_cod_d", "estu_trabaja",  "cole_naturaleza"   , "buffer_km", "DISTANCE"  )]


# Calcular el promedio por cada cole_cod_d y variables de tratamiento
promedios <- data_ %>%
  group_by(cole_cod_d, year_treated_10p  #,year_treated_100p, year_treated_50p, year_treated_ai, year_treated_ic
           , year) %>%
  summarise(
    sd_reading_i = mean(sd_reading_i, na.rm = TRUE),
    reading_i = mean(reading_i, na.rm = TRUE),
    mediana_reading_i = median(sd_reading_i, na.rm = TRUE),
    p10_reading_i = quantile(sd_reading_i, 0.10, na.rm = TRUE),
    p90_reading_i = quantile(sd_reading_i, 0.90, na.rm = TRUE),
    sd_math_i = mean(sd_math_i, na.rm = TRUE),
    math_i = mean(math_i, na.rm = TRUE),
    mediana_math_i = median(sd_math_i, na.rm = TRUE),
    p10_math_i = quantile(sd_math_i, 0.10, na.rm = TRUE),
    p90_math_i = quantile(sd_math_i, 0.90, na.rm = TRUE),
    sum_estu_trabaja = sum(estu_trabaja, na.rm = TRUE),
    sum_estu = n()
  )
# print(promedios)
# Crear el histograma y densidad de promedio_sd_math_i faceteado por año
ggplot(data_, aes(x = sd_math_i)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(fill = "red", alpha = 0.5) +
  facet_wrap(~ year, scales = "free") +
  labs(title = "Histograma y Densidad de Promedio SD Math por Año",
       x = "Promedio SD Math",
       y = "Densidad") +
  theme_minimal()

 

# Crear el histograma y densidad de promedio_sd_math_i faceteado por año
ggplot(data_, aes(x = sd_reading_i)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(fill = "red", alpha = 0.5) +
  facet_wrap(~ year, scales = "free") +
  labs(title = "Histograma y Densidad de Promedio SD Math por Año",
       x = "Promedio SD Math",
       y = "Densidad") +
  theme_minimal()
 
###### 
# Cohortes
######

plot_cohort_means(data = data_, year_treated = "year_treated_10p")
plot_cohort_means(data = data_, year_treated = "year_treated_100p")
plot_cohort_means(data = data_, year_treated = "year_treated_50p")
unique(data_$year_treated_ic)
plot_cohort_means(data = data_, year_treated = "year_treated_ic")
plot_cohort_means(data = data_, year_treated = "year_treated_ai")

plot_cohort_means(data = data_2, year_treated = "year_treated")
 
#####
# General Summary
#####
# Resumen estadístico de variables específicas
summary(select(data_, sd_math_i,sd_reading_i, year, cole_cod_dane_institucion, year_treated_10p))

# Histograma de la variable de resultado
# Conteo de observaciones por año
data_ %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Número de Observaciones por Año", x = "Año", y = "Número de Observaciones")

 # Accumulative  treatment by period
# Contar el número de colegios tratados por acta de inicio
if (1==1) {
  
  colegios_tratados_por_ano <- data_ %>%
    filter(!is.na(year_treated_10p)) %>%  # Filtrar para eliminar las filas donde no hay tratamiento
    group_by(year_treated_10p) %>%  # Agrupar por año de tratamiento
    summarise(num_colegios = n_distinct(cole_cod_dane_institucion))  # Contar colegios únicos
  
  colegios_tratados_por_ano = rbind(colegios_tratados_por_ano,
                                    data.frame("year_treated_10p" = c(2010,2013,2015,2018) ,
                                               "num_colegios" = c(0,0,0,0)  
                                    ))
  
  # colegios_tratados_por_ano = rbind(colegios_tratados_por_ano,
  #                                   data.frame("year_treated_ai" = c(2003,2005,2006,2009,2012,2013,2017) ,
  #                                              "num_colegios" = c(0,0,0,0,0,0,0)  
  #                                   ))

  # Calcular el conteo acumulado
  colegios_tratados_por_ano <- colegios_tratados_por_ano %>%
    arrange(year_treated_10p) %>%  # Ordenar por año de tratamiento
    mutate(cumulative_colegios = cumsum(num_colegios))  # Calcular la suma acumulada
  
  # Mostrar el resultado
  print(colegios_tratados_por_ano)
  colegios_tratados_por_ano$Treated_rate = colegios_tratados_por_ano$cumulative_colegios/179
  # Visualizar Staggered
  ggplot(colegios_tratados_por_ano, aes(x = year_treated_10p, y = num_colegios)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Number of Schools Treated per Year", x = "Year of Treatment", y = "Number of Schools Treated")
  
  ggplot(colegios_tratados_por_ano, aes(x = year_treated_10p, y = cumulative_colegios)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Cumulative Number of Schools Treated per Year", x = "Year of Treatment", y = "Cumulative Number of Schools Treated")
  
  
  # Visualizar la gráfica de volumen
  ggplot(colegios_tratados_por_ano, aes(x = year_treated_10p, y = Treated_rate)) +
    geom_bar(stat = "identity", fill = "white", color = "black") +
    geom_bar(aes(y = Treated_rate), stat = "identity", fill = "lightblue", color = "black") +
    labs(title = "Cumulative Treatment Progress of Schools by Year", x = "Year of Treatment", y = "Cumulative Treated Rate") +
    theme_minimal()
  
  png(paste0("Graph/cumulative_treatment_progress_school_10p.png"),  width = 1030, height = 598)
  ggplot(colegios_tratados_por_ano, aes(x = year_treated_10p)) +
    geom_bar(aes(y = 1), stat = "identity", fill = "white", color = "gray", width = 0.8) +
    geom_bar(aes(y = Treated_rate, fill = "Treated"), stat = "identity", color = "gray", width = 0.8) +
    labs(title = "Cumulative Treatment Progress of Schools by Year\n (Reference Date: 10% of the Concession Progress)",
         x = "Year of Treatment",
         y = "Cumulative Treated Rate",
         fill = "Legend") +
    scale_fill_manual(name = "Legend", values = c("white" = "white", "Treated" = "lightblue"),
                      labels = c("Treated", "Treated")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey", size = 0.2),
      panel.grid.minor = element_blank()
    )
  dev.off() 
}
#ALL
# Contar el número de estudiantes de colegios tratados por cada año



# IC
m_twfe_ic <- feols(sd_math_i ~ i(time_to_treat_ic, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_ic)
r_twfe_ic <- feols(sd_reading_i ~ i(time_to_treat_ic, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_ic)

fixest::etable(m_twfe_ic, r_twfe_ic)
fixest::iplot(m_twfe_ic)
fixest::iplot(r_twfe_ic)
# AI
m_twfe_ai <- feols(sd_math_i ~ i(time_to_treat_ai, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_ai)
r_twfe_ai <- feols(sd_reading_i ~ i(time_to_treat_ai, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_ai)
fixest::etable(m_twfe_ai,r_twfe_ai)
 
# 10P
m_twfe_10p <- feols(sd_math_i ~ i(time_to_treat_10p, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_10p)
r_twfe_10p <- feols(sd_reading_i ~ i(time_to_treat_10p, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_10p)
fixest::etable(m_twfe_10p,r_twfe_10p)
fixest::iplot(m_twfe_10p)
# 50P
m_twfe_50p <- feols(sd_math_i ~ i(time_to_treat_50p, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_50p)
r_twfe_50p <- feols(sd_reading_i ~ i(time_to_treat_50p, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_50p)
fixest::etable(m_twfe_50p,r_twfe_50p)
# 100P
m_twfe_100p <- feols(sd_math_i ~ i(time_to_treat_100p, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_100p)
r_twfe_100p <- feols(sd_reading_i ~ i(time_to_treat_100p, ref = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_100p)
fixest::etable(m_twfe_100p, r_twfe_100p)

#### TWFE segun IC
fixest::etable(m_twfe_ic, r_twfe_ic, m_twfe_ai, r_twfe_ai, m_twfe_10p, r_twfe_10p, m_twfe_50p, r_twfe_50p, m_twfe_100p, r_twfe_100p)
#################################
# Sun And Abraham 2021
#################################

m_sa_ic <- feols(sd_math_i ~ sunab(year_treated_ic, year, -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_ic)
r_sa_ic <- feols(sd_reading_i ~ sunab(year_treated_ic, year, -1)  | cole_cod_d + year, cluster = ~ cole_cod_d,  data = data_ic)
fixest::etable(m_sa_ic, r_sa_ic)

#10 P
m_sa_10p <- feols(sd_math_i ~ sunab(year_treated_10p, year, ref.p = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_10p)
r_sa_10p <- feols(sd_reading_i ~ sunab(year_treated_10p, year, ref.p = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_10p)
fixest::etable(m_sa_10p, r_sa_10p)

# 100P
m_sa_100p <- feols(sd_math_i ~ sunab(year_treated_100p, year, ref.p = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_100p)
r_sa_100p <- feols(sd_reading_i ~ sunab(year_treated_100p, year, ref.p = -1) | cole_cod_d + year, cluster = ~ cole_cod_d, data = data_100p)
fixest::etable(m_sa_100p, r_sa_100p)



panelview(sd_math_i ~ treat_relative, data = balanced_data[is.na(balanced_data$sd_math_i)==F, ], 
          index = c("cole_cod_d","ANIO"), 
          xlab = "Year", ylab = "Unit", display.all = T,
          gridOff = TRUE, by.timing = TRUE)
  


#################################
# Callaway y Santana 
#################################
 
# # Ejecutar el análisis de diferencias en diferencias
Call_San <- function(outcome ="outcome: string", time_name = "time name: string",
                     id_name = "",  ){
  out <- att_gt(
    yname = outcome, # Nombre de la variable de resultado
    tname = time_name,    # Nombre de la variable de tiempo
    idname = "cole_cod_dane_institucion",     # Nombre de la variable de identificación
    gname = "year_treated_10p", # Nombre de la variable de tiempo de tratamiento
    data = data_,   # Datos
    control_group = "notyettreated",
    est_method = "dr",  # Método de estimación (Doubly Robust en este caso)
    panel = FALSE
  )
  
}


# Resumen de los resultados
summary(out)
ggdid(out)

# agg.simple <- aggte(out, type = "simple", na.rm = TRUE)
# summary(agg.simple)

agg.es <- aggte(out, type = "dynamic", na.rm = TRUE)
summary(agg.es)
ggdid(agg.es)


out <- att_gt(
  yname = "sd_math_i", # Nombre de la variable de resultado
  tname = "year",    # Nombre de la variable de tiempo
  idname = "cole_cod_dane_institucion",     # Nombre de la variable de identificación
  gname = "year_treated_10p", # Nombre de la variable de tiempo de tratamiento
  data = data_,   # Datos
  control_group = "notyettreated",
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

agg.simple <- aggte(out, type = "simple", na.rm = TRUE)
summary(agg.simple)
