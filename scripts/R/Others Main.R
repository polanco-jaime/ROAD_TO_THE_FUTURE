# Cargar las librerías necesarias
library(dplyr)
library(tidyr)

# Crear funciones para calcular estadísticas descriptivas
calculate_summary <- function(x) {
  stats <- summary(x)
  as.data.frame(t(stats))
}
#Dummies of time to treat
data$time_to_treat = data$year -  data$year_treated
# Calcular las estadísticas descriptivas para cada grupo
never_treated_summary <- calculate_summary(data[data$treat_ == 0, ]$math_c)
treated_at_some_point_summary <- calculate_summary(data[data$treat_ == 1, ]$math_c)
already_treated_summary <- calculate_summary(data[data$treat_ == 1 & data$time_to_treat >= 0, ]$math_c)
not_yet_treated_summary <- calculate_summary(data[data$treat_ == 1 & data$time_to_treat < 0, ]$math_c)

# Añadir nombres a las filas para identificar los grupos
never_treated_summary$Group <- "Never treated"
treated_at_some_point_summary$Group <- "Treated at some point"
already_treated_summary$Group <- "Already treated"
not_yet_treated_summary$Group <- "Not yet treated"

# Combinar todas las estadísticas en una sola tabla
descriptive_statistics <- bind_rows(
  never_treated_summary,
  treated_at_some_point_summary,
  already_treated_summary,
  not_yet_treated_summary
)

# Reorganizar las columnas para poner el grupo primero
descriptive_statistics <- descriptive_statistics %>%
  select(Group, everything())

# Convertir Var1 a numérico si es posible
# descriptive_statistics$Var1 <- as.numeric(as.character(descriptive_statistics$Var1))

# Añadir una columna para las estadísticas
descriptive_statistics <- descriptive_statistics %>%
  pivot_longer(cols = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."), 
               names_to = "Statistic", values_to = "Value")

# Transformar la tabla a formato ancho
descriptive_statistics_wide <- descriptive_statistics %>%
  pivot_wider(names_from = "Var2", values_from = "Freq")

# Mostrar la tabla de estadísticas descriptivas en formato ancho
print(descriptive_statistics_wide)


data_ai$time_to_treat_ai

data$SB_PRO
child_post$Stu_higher_education/ child_post$TOTAL
hca_pre = sqldf::sqldf("
SELECT  * 
    FROM (
  SELECT  cole_cod_dane_institucion, 
  COUNT(DISTINCT ESTU_CONSECUTIVO) TOTAL, 
  ifnull(sum(SB_TYT), 0) + ifnull(sum(SB_PRO), 0) Stu_higher_education,
  (ifnull(sum(SB_TYT), 0) + ifnull(sum(SB_PRO), 0) ) /  COUNT(DISTINCT ESTU_CONSECUTIVO) as Human_Capital_Accumulation
  FROM 
    data_ai
  WHERE
    time_to_treat_ai < 0 and estu_trabaja is not null
  GROUP BY 1
    )
")
mean(hca_pre$Stu_higher_education / hca_pre$TOTAL )
sd(hca_pre$Stu_higher_education / hca_pre$TOTAL )

