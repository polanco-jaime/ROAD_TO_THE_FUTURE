###############################################################################################
##################################################
################## otros outcomes ############# 
#################################################
###############################################################################################



sql = "
SELECT DISTINCT    TREATED_CONS_STARTED, --TREATED_ROAD_FINISHED
    AVG(median_norm_math_c) median_norm_math_c,
    AVG(median_norm_reading_c)  median_norm_reading_c, 
    COLE_COD_ICFES, Fecha_entrega,
     -- Fecha_inicio_consecion, 
    ANIO , buffer_km,  COLE_MCPIO_UBICACION
--       CASE 
--           WHEN TREATED_CONS_STARTED = 1  
--               THEN  SAFE_CAST(ANIO AS INT64) - SAFE_CAST(Fecha_entrega AS INT64) 
--           ELSE 0 END ,
--       ifnull(Fecha_entrega, 0) > SAFE_CAST(ANIO AS INT64)
    
    FROM base
    WHERE 
    (    case when Fecha_entrega =2016 then 1
        when  Fecha_entrega =2021 then 1
        when  Fecha_entrega is null then 0 else 0 end) = 0
    GROUP BY 1,4,5,6,7,8
        
"
base_2018= sqldf(sql)
base_2018$treat  = as.factor(ifelse(is.na(base_2018$Fecha_entrega), 0,1)) # as.factor(base_2018$TREATED_CONS_STARTED)
base_2018$year  =  base_2018$ANIO
base_2018$math_c = base_2018$median_norm_math_c
base_2018$reading_c = base_2018$median_norm_reading_c

DiD = base_2018[,c('math_c','treat','year', 'reading_c', 'COLE_COD_ICFES')]

DiD <- DiD %>% 
  mutate(after = year >= 2018) %>%
  mutate(treatafter = after*as.numeric(treat) )
reg_m<-lm(math_c ~ treat+treatafter+after   , data = DiD)
reg_r<-lm(reading_c ~ treat+treatafter+after, data = DiD)
summary(reg_m)
summary(reg_r)

reg_m2<-lm(math_c ~ treat+treatafter+after + COLE_COD_ICFES + factor(year) , data = DiD)
reg_r2<-lm(reading_c ~ treat+treatafter+after +COLE_COD_ICFES + factor(year), data = DiD)
(summary(reg_m2)$coefficients)[1:4,]
(summary(reg_r2)$coefficients)[1:4,]


## Efecto del tratamiento
ATT_m = (reg_m$coefficients)[3] /(reg_m$coefficients)[1] 
ATT_r = (reg_r$coefficients)[3] /(reg_r$coefficients)[1] 
ATT_m
ATT_r

#### la diferencia del grupo tratado fue de 0.013842
DID_AVG = sqldf("
      SELECT 
      AVG(math_c)*100 math_c,
      year, treat
      FROM DiD
      GROUP BY 2,3
      ")
mt <- ggplot(DID_AVG,aes(x=year, y=math_c, color = (treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2018,lty=15) +
  geom_vline(xintercept=2014,lty=5) +
  labs(title="math results and Time", x="Year", y="math results Rate")
mt

DID_AVG = sqldf("
      SELECT 
      AVG(reading_c)*100 READING_TEST_RESULT,
      year, treat
      FROM DiD
      GROUP BY 2,3
      ")


mt <- ggplot(DID_AVG,aes(x=year, y=READING_TEST_RESULT, color = (treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2018,lty=15) +
  geom_vline(xintercept=2014,lty=5) +
  labs(title="reading results and Time", x="Year", y="reading results Rate")
mt
#####################################################
# Dif and dif con fecha de consecion

sql = "
SELECT DISTINCT    
    ---TREATED_CONS_STARTED, 
    TREATED_ROAD_FINISHED,
    median_norm_math_c, median_norm_reading_c, 
    COLE_COD_ICFES,
    ----Fecha_entrega,
     (    case when Fecha_inicio_consecion =2002 then null
        when  Fecha_inicio_consecion =2006 then null
         when  Fecha_inicio_consecion =2010 then null
         when  Fecha_inicio_consecion =2020 then null else 
         Fecha_inicio_consecion  end) 
     
      Fecha_entrega, 
    ANIO , buffer_km,  COLE_MCPIO_UBICACION

    FROM base
     WHERE 
    (    case when Fecha_inicio_consecion =2002 then 1
        when  Fecha_inicio_consecion =2006 then 1
         when  Fecha_inicio_consecion =2010 then 1
         when  Fecha_inicio_consecion =2020 then 0 else 0 end) = 0
"

base_2018_fini= sqldf(sql)
summary(factor(base_2018_fini$Fecha_entrega) )
base_2018_fini$treat  = base_2018_fini$TREATED_ROAD_FINISHED
base_2018_fini$year  =  base_2018_fini$ANIO
base_2018_fini$math_c = base_2018_fini$median_norm_math_c
base_2018_fini$reading_c = base_2018_fini$median_norm_reading_c
glimpse(base_2018_fini)
DiD_finished = base_2018_fini[ ,c('math_c','treat','year', 'reading_c')]

DiD_finished <- DiD_finished %>% 
  mutate(after = year >= 2018) %>%
  mutate(treatafter = after*treat)

reg_DiD_finished<-lm(reading_c ~ treat+treatafter+after, data = DiD_finished)
summary(reg_DiD_finished)

## Efecto del tratamiento
ATT = (reg$coefficients)[3] /(reg$coefficients)[1] 
ATT

DiD2 = base_2018[,c('math_c','treat','year', 'COLE_COD_ICFES')]


DiD2 <- DiD2 %>% 
  mutate(after = year >= 2018) %>%
  mutate(treatafter = after*treat)

mt <- ggplot(DiD2,aes(x=year, y=math_c, color = factor(treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2018,lty=15) +
  labs(title="math results and Time", x="Year", y="math results Rate")
mt
reg2<-lm(math_c ~ treat+treatafter+after + factor(COLE_COD_ICFES) + factor(year), data = DiD2)
(summary(reg2)$coefficients)[1:4,]

(reg2$coefficients)[3] /(reg2$coefficients)[1] 
#### la diferencia del grupo tratado fue de 0.013842
DID_AVG = sqldf("
      SELECT 
      AVG(math_c)*100 MATH_TEST_RESULT,
      year, treat
      FROM DiD_finished
      GROUP BY 2,3
      ")
mt <- ggplot(DID_AVG,aes(x=year, y=MATH_TEST_RESULT, color = (treat))) +
  geom_point(size=2)+geom_line() +
  geom_vline(xintercept=2014,lty=15) +
  labs(title="math results and Time", x="Year", y="math results Rate")
mt
DID_AVG$treat

iplot(
  list(
    feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
          | year # COLE_COD_ICFES+ year
          ,                             ## FEs
          #  cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
          data = base1
    )
    
    ,feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
           #|  year # COLE_COD_ICFES+ year
           ,                             ## FEs
           cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
           data = base1
    )
    ,feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
           |  COLE_COD_ICFES+ year
           ,                             ## FEs
           # cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
           data = base1
    )
    ,feols(reading_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
           |  COLE_COD_ICFES+ year,                             ## FEs
           cluster = ~ COLE_COD_ICFES,                              ## Clustered SEs
           data = base1
    )
  ), 
  sep = 0.1, ref.line = -1,
  xlab = 'Time to treatment',
  main = 'Math test')

legend(
  "topleft", col = c(1, 2,3,4,5), pch = c(20, 17,18,19,21), 
  legend = c(" sin ef y sin clu", "sin ef", "sin clu", "todo", "todo interact"))



#############################################################33
###  Callaway and Sant’Anna (2021), “Difference-in-Differences with Multiple Time Periods”.
###  https://doi.org/10.1016/j.jeconom.2020.12.001

#install.packages("did")
library(did)

table(base_10p$att)

tabla  = base_10p 
tabla$idname =  as.numeric(tabla$cole_cod_dane)
es <- aggte( ### getting the att stimation for a DiD
  att_gt(yname = "math_c",
         gname = "year_treated_att",
         idname = "idname",
         tname = "year",anticipation = 0,
         data = subset(tabla, tabla$buffer_km == 1000),
         allow_unbalanced_panel= T, panel = F),
  ### getting the att stimation for a DiD Dynamic
  type = "dynamic",
  na.rm = TRUE)
Buffer = '1000M'
subject = "Math"





library(dotwhisker)

left_join(table_result_DiD(Buffer = '1000M', 
                           TWFE_result = TWFE_Math_base_10p[1],
                           SA_result = SA_Math_base_10p[1] ), 
          table_result_ATT( ATT_result = es  ),
          by = "Periodo"
          
) 

table_result_DiD(Buffer = '1000M', 
                 TWFE_result = TWFE_Math_base_10p[1],
                 SA_result = SA_Math_base_10p[1] )

ggdid(es)

MODEL = feols(math_c ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
              |  cole_cod_dane+ year,                             ## FEs
              cluster = ~ cole_cod_dane,                              ## Clustered SEs
              data = subset(df, df$buffer_km == i ) )
MODEL_SA = feols(math_c ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                 |  cole_cod_dane+ year,                             ## FEs
                 cluster = ~ cole_cod_dane,                         ## Clustered SEs
                 data = subset(df, df$buffer_km == i ) )
iplot(math_twfe_ai)
