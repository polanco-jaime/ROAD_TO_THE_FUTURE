options(scipen=999)
# ########################## loading full data ################################## 
# bq_auth(token = STEP1)
# project_id <- "ph-jabri"
# dataset_id <- "01_road_to_the_future"
# table_id <- "BASE_ROADS_TO_THE_FUTURE"
# 
# data = bigrquery::bq_table_download(
#   as_bq_table(
#   paste0(  project_id, '.' ,  dataset_id   , '.' ,table_id  )
#    ),
#   n_max  = Inf )
# 
# arrow::write_parquet(data, 'Data/road_to_the_future.parquet')

data = arrow::read_parquet('Data/road_to_the_future.parquet')
 
##########################################
# data$time_to_treat = as.numeric(data$ANIO) -  as.numeric(data$Fecha_inicio_consecion)
# data$treat_relative = ifelse( data$time_to_treat>=0 , 1,0  )
# Como es el caso de que sea balanceado
# Paso 1: Identificar los IDs con datos completos
complete_ids <- data %>%
  group_by(cole_cod_dane_institucion) %>%
  summarise(n_years = n_distinct(ANIO)) %>%
  filter(n_years == length(unique(data$ANIO))) %>%
  pull(cole_cod_dane_institucion)

# Paso 2: Filtrar el dataframe para incluir solo estos IDs
filtered_data <- data %>%
  filter(cole_cod_dane_institucion %in% complete_ids)

# Paso 3: Crear todas las combinaciones posibles de cole_cod_dane y var para los IDs filtrados
all_combinations <- expand.grid(
  cole_cod_dane_institucion = unique(filtered_data$cole_cod_dane_institucion),
  ANIO = unique(filtered_data$ANIO)
)
# Unir las combinaciones con el dataframe filtrado para crear el panel balanceado
data <- all_combinations %>%
  left_join(filtered_data, by = c("cole_cod_dane_institucion", "ANIO"))
# Como es el tratamiento escalnoado en un anel desbalanceado



#
data_ = subset(data, data$DISTANCE <=1500)
data_ = subset(data, data$cole_naturaleza =="PUBL")
sqldf::sqldf("SELECT   cole_naturaleza , COUNT(DISTINCT cole_cod_d) TOT FROM data group by 1")
sqldf::sqldf("SELECT   cole_naturaleza , COUNT(DISTINCT cole_cod_d) TOT FROM data_ group by 1")
 
########################################
table(data_$Fecha_inicio_consecion)
data_$year_treated_ic = ifelse(is.na(as.numeric(data_$Fecha_inicio_consecion))==F, as.numeric(data_$Fecha_inicio_consecion), NaN)
table(data_$year_treated_ic)
########################################
table(data_$Fecha_Acta_de_inicio)
data_$year_treated_ai = ifelse(is.na(as.numeric(data_$Fecha_Acta_de_inicio))==F, as.numeric(data_$Fecha_Acta_de_inicio), NaN)
table(data_$year_treated_ai)
########################################
table(data_$Fecha_al_10_)
data_$year_treated_10p = ifelse(is.na(as.numeric(data_$Fecha_al_10_))==F, as.numeric(data_$Fecha_al_10_), NaN)
table(data_$year_treated_10p)

########################################
table(data_$Fecha_al_50_)
data_$year_treated_50p = ifelse(is.na(as.numeric(data_$Fecha_al_50_))==F, as.numeric(data_$Fecha_al_50_), NaN)
table(data_$year_treated_50p)

########################################
table(data_$Fecha_entrega)
data_$year_treated_100p = ifelse(is.na(as.numeric(data_$Fecha_entrega))==F, as.numeric(data_$Fecha_entrega), NaN)
table(data_$year_treated_100p)
########################################
data_$year = as.numeric(data_$ANIO)   

# 
# 
data_$time_to_treat_ic = data_$year -  data_$year_treated_ic
data_$time_to_treat_ai = data_$year -  data_$year_treated_ai
data_$time_to_treat_10p = data_$year -  data_$year_treated_10p
data_$time_to_treat_50p = data_$year -  data_$year_treated_50p
data_$time_to_treat_100p = data_$year -  data_$year_treated_100p
# Crear variables de tratamiento binarias
data_$treat_ic <- ifelse(data_$year >= data_$year_treated_ic, 1, 0)
data_$treat_ai <- ifelse(data_$year >= data_$year_treated_ai, 1, 0)
data_$treat_10p <- ifelse(data_$year >= data_$year_treated_10p, 1, 0)
data_$treat_50p <- ifelse(data_$year >= data_$year_treated_50p, 1, 0)
data_$treat_100p <- ifelse(data_$year >= data_$year_treated_100p, 1, 0)

# Filtrar los datos según la ventana de tiempo especificada
max_min = 10
data_ic <- data_[data_$time_to_treat_ic >= -max_min & data_$time_to_treat_ic <= max_min, ]
data_ai <- data_[data_$time_to_treat_ai >= -max_min & data_$time_to_treat_ai <= max_min, ]
data_10p <- data_[data_$time_to_treat_10p >= -max_min & data_$time_to_treat_10p <= max_min, ]
data_50p <- data_[data_$time_to_treat_50p >= -max_min & data_$time_to_treat_50p <= max_min, ]
data_100p <- data_[data_$time_to_treat_100p >= -max_min & data_$time_to_treat_100p <= max_min, ]

 

data_$X <-round( ( data_$year_treated_ai + data_$year_treated_100p )/2 , 0)
data_2 <- sqldf::sqldf(" 
             SELECT *, 
             --
             CASE WHEN X>= year 
                              AND year NOT BETWEEN year_treated_ai AND year_treated_100p
                         THEN X+time_to_treat_ai
             ---
             WHEN year>=X
                        AND  year NOT BETWEEN year_treated_ai AND year_treated_100p
                        THEN X+time_to_treat_100p
             ELSE X END year_colapsed , X AS year_treated
             FROM data_
 
             ")

 
data_2$time_to_treat = data_2$year_colapsed -  data_2$year_treated
########

