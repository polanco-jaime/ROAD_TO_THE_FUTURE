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
data = subset(data, data$DISTANCE <=1500)
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

# 
# # Balancing data
# data_ic =  balancing_data(data= data_ic, id = 'cole_cod_dane_institucion', time_references = 'ANIO' )
# data_ic$time_to_treat = data_ic$time_to_treat_ic
# 
# data_ai =  balancing_data(data= data_ai, id = 'cole_cod_dane_institucion', time_references = 'ANIO')
# data_ai$time_to_treat = data_ai$time_to_treat_ai
# 
# data_10p =  balancing_data(data= data_10p, id = 'cole_cod_dane_institucion', time_references = 'ANIO')
# data_10p$time_to_treat = data_10p$time_to_treat_10p
# 
# data_50p =   balancing_data(data= data_50p, id = 'cole_cod_dane_institucion', time_references = 'ANIO')
# data_50p$time_to_treat = data_50p$time_to_treat_50p
# 
# data_100p = balancing_data(data= data_100p, id = 'cole_cod_dane_institucion', time_references = 'ANIO')
# data_100p$time_to_treat = data_100p$time_to_treat_100p

# data_$X <-round( ( data_$year_treated_ai + data_$year_treated_100p )/2 , 0)
# data_2 <- sqldf::sqldf(" 
#              SELECT *, 
#              --
#              CASE WHEN X>= year 
#                               AND year NOT BETWEEN year_treated_ai AND year_treated_100p
#                          THEN X+time_to_treat_ai
#              ---
#              WHEN year>=X
#                         AND  year NOT BETWEEN year_treated_ai AND year_treated_100p
#                         THEN X+time_to_treat_100p
#              ELSE X END year_colapsed , X AS year_treated
#              FROM data_
#  
#              ")
# 
#  
# data_2$time_to_treat = data_2$year_colapsed -  data_2$year_treated
# ########

