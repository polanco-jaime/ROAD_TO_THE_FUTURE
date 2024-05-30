base   = base_1

my_list = colnames(base)
my_list =  lapply(my_list, toupper)
colnames(base) = my_list

base$EMPLYMENT_TOTAL = 100 - base$UNEMPLOYMENT_ILO_MODEL_ESTIMATE 
lista =  c("CONSUMPTION_ENERGY", "GOV_EFFECTIVENESS", "EMPLYMENT_TOTAL", 
           "GDP_CAPITA", "GDP_CURRENT", "GDP_GROWTH")
library(dplyr)
#base = base %>% dplyr::select(lista)
 
##################################################################

if 1==1{
  esta_desc = data.frame()
  esta_desc_heterogenities = data.frame()
  for (j in unique(base$TREAT)  ) {
   
    TEMPORAL =  base %>% subset(base$TREAT == j)
    treated = ifelse(j == 1 , 'With Carbon Tax' , 'Without Carbon Tax' )
    
    for (i in lista ) {
   
      temp = data.frame("variable" = i, 
                        "mean"  =  mean( TEMPORAL [[i]], na.rm = T )  ,
                        "median" = median(TEMPORAL[[i]], na.rm = T ) ,
                        "sd" = sd(TEMPORAL[[i]], na.rm = T ),
                        'N_countries' = length(unique(TEMPORAL$COUNTRY)),
                        "Observaciones" = length(TEMPORAL$COUNTRY) ,
                        "treat" = treated,
                        "BRECHA_TEC" = 'All sample'
      )
      esta_desc = rbind(temp, esta_desc)
    }
   
  }
  
  esta_desc_heterogenities = data.frame()
  for (j in unique(base$TREAT)  ) {
    TEMPORAL =  base %>% subset(base$TREAT == j)
    treated = ifelse(j == 1 , 'With Carbon Tax' , 'Without Carbon Tax' )
  
    for (k in unique(TEMPORAL$BRECHA_TEC) ) {
      TEMPORAL_2=  TEMPORAL %>% subset( BRECHA_TEC == k)   
      
      for (l in lista ) {
        temp2 = data.frame("variable" = l, 
                           "mean"  =  mean( TEMPORAL_2 [[l]], na.rm = T )  ,
                           "median" = median(TEMPORAL_2[[l]], na.rm = T ) ,
                           "sd" = sd(TEMPORAL_2[[l]], na.rm = T ),
                           'N_countries' = length(unique(TEMPORAL_2$COUNTRY)),
                           "Observaciones" = length(TEMPORAL_2$COUNTRY) ,
                           "treat" = treated, 
                           "BRECHA_TEC" = k
        )
        esta_desc_heterogenities = rbind(temp2, esta_desc_heterogenities)
      }
      
    }
  }

estadisticas_descriptivas = rbind(esta_desc, esta_desc_heterogenities)
}

writexl::write_xlsx(estadisticas_descriptivas, 'estadisticas_descriptivas.xlsx')
