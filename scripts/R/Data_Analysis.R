


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
library(ggplot2)
plot_histogram_output(data_10p[data_10p$time_to_treat_10p==-2 | data_10p$time_to_treat_10p==2, ], 
                      output = 'sd_math_i','time_to_treat_10p',
                      plot_title = "Histogram and Density of Average SD Math") 

plot_histogram_output(data_10p[data_10p$time_to_treat_10p==-2 | data_10p$time_to_treat_10p==2, ], 
                      output = 'sd_reading_i','time_to_treat_10p',
                      plot_title = "Histogram and Density of Average SD Math") 
#-----

plot_histogram_output(data_50p[data_50p$time_to_treat_50p==-2 | data_50p$time_to_treat_50p==2, ], 
                      output = 'sd_math_i','time_to_treat_50p',
                      plot_title = "Histogram and Density of Average SD Math") 

plot_histogram_output(data_50p[data_50p$time_to_treat_50p==-2 | data_50p$time_to_treat_50p==2, ], 
                      output = 'sd_reading_i','time_to_treat_50p',
                      plot_title = "Histogram and Density of Average SD Math") 
#-----

plot_histogram_output(data_100p[data_100p$time_to_treat_100p==-2 | data_100p$time_to_treat_100p==2, ], 
                      output = 'sd_math_i','time_to_treat_100p',
                      plot_title = "Histogram and Density of Average SD Math") 

plot_histogram_output(data_100p[data_100p$time_to_treat_10p==-2 | data_100p$time_to_treat_10p==2, ], 
                      output = 'sd_reading_i','time_to_treat_10p',
                      plot_title = "Histogram and Density of Average SD Math")

###### 
# Cohortes
######

plot_cohort_means(data = data_10p, year_treated = "year_treated_10p")
plot_cohort_means(data = data_100p, year_treated = "year_treated_100p")
plot_cohort_means(data = data_50p, year_treated = "year_treated_50p")
plot_cohort_means(data = data_ic, year_treated = "year_treated_ic")
plot_cohort_means(data = data_ai, year_treated = "year_treated_ai")

plot_cohort_means(data = data_2, year_treated = "year_treated")
 
#####
# General Summary
#####
# Accumulative  treatment by period
png(paste0("Graph/cumulative_treatment_progress_school_0p.png"),  width = 1030, height = 598)
acumulative_treatment(data_ai,'year_treated_ai', 'cole_cod_dane_institucion',   '0% of the Concession Progress') 
dev.off() 

png(paste0("Graph/cumulative_treatment_progress_school_10p.png"),  width = 1030, height = 598)
acumulative_treatment(data_10p,'year_treated_10p', 'cole_cod_dane_institucion',   '10% of the Concession Progress') 
dev.off() 

png(paste0("Graph/cumulative_treatment_progress_school_50p.png"),  width = 1030, height = 598)
acumulative_treatment(data_50p,'year_treated_50p', 'cole_cod_dane_institucion',   '50% of the Concession Progress') 
dev.off() 

png(paste0("Graph/cumulative_treatment_progress_school_100p.png"),  width = 1030, height = 598)
acumulative_treatment(data_100p,'year_treated_100p', 'cole_cod_dane_institucion',   '100% of the Concession Progress') 
dev.off() 
#ALL
# Contar el número de estudiantes de colegios tratados por cada año


 

panelview(sd_math_i ~ treat_relative, data = balanced_data[is.na(balanced_data$sd_math_i)==F, ], 
          index = c("cole_cod_d","ANIO"), 
          xlab = "Year", ylab = "Unit", display.all = T,
          gridOff = TRUE, by.timing = TRUE)
  
