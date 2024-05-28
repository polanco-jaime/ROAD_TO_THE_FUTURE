if (Sys.info()["nodename"] == "CLOUD37") {
  General_path = "C:/Users/USER/Desktop/01-with-the-boys/"
  data_dir <- paste0(General_path , "Data/")
  graphs_dir <-  paste0(General_path , "Graph/") 
  tables_dir <- paste0(General_path , "Tables/")  
}  else if (Sys.info()["nodename"] ==  "Jaimes-MacBook-Pro.local" ){
  General_path = "/Users/jaimepolanco-jimenez/Library/CloudStorage/OneDrive-PontificiaUniversidadJaveriana/01_research/ROAD_TO_THE_FUTURE/" 
  data_dir <- paste0(General_path , "Data/")
  graphs_dir <-  paste0(General_path , "Graph/") 
  tables_dir <- paste0(General_path , "Tables/")   
}

#load("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
lista = c('readr','readxl','sqldf','plyr', 
          'did' , 'arrow',  'plyr', 'ggplot2',
          'dplyr','fixest' , 'gargle' , 'stringr', 'broom', 
          "panelView", "bacondecomp", "paneltools","fect","PanelMatch"
          #, 'bigrquery' 
)

 

for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}
# Define the function
plot_model_effects <- function(model) {
  # Tidy the model output
  model_tidy <- tidy(model, conf.int = TRUE)
  
  # Filter out the intercept if desired
  model_tidy <- model_tidy %>% filter(term != "(Intercept)")
  model_tidy <- model_tidy %>% filter(grepl("factor\\(", term))
  model_tidy <- model_tidy %>% filter(!grepl("factor\\(cole_cod_dane\\)", term))
  model_tidy <- model_tidy %>% filter(!grepl("factor\\(year\\)", term))
  # Clean up the term column by removing 'factor(time_to_treat)' and convert to numeric
  model_tidy$term <-  (gsub("factor\\(time_to_treat\\)", "", model_tidy$term))
  model_tidy$term <- (gsub("time_to_treat_l", "-", model_tidy$term))
  model_tidy$term <- as.numeric(gsub("time_to_treat_", "", model_tidy$term))
  # Create the plot
  ggplot(model_tidy, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_vline(xintercept = -1, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
    labs(
      title = "Effect of Time to Treat on Math Scores",
      x = "Time to Treat",
      y = "Estimated Effect (math_c)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

setwd(paste0(General_path ,'Data'))
temp = list.files("./",pattern="*.parquet")


list2env(
  lapply(setNames(temp, make.names(gsub("*.parquet$", "", temp))), 
         arrow::read_parquet), envir = .GlobalEnv  )


##################################################
# Analsis de datos
# calculate the means
library(data.table)
library(ggplot2)
data = base_ai[ , c("math_c",  "math_c_sd"   , "treat_" ,  "year_treated"  ,  "year" ,"cole_cod_dane", "estu_trabaja",  "cole_naturaleza"   , "buffer_km"  )]
# data = na.omit(data)
# Calculate the means using dplyr
cohort_means <- data %>%
  group_by(year_treated, year) %>%
  summarize(math_c = mean(math_c, na.rm = TRUE), .groups = 'drop')

# plot the means
ggplot(data=cohort_means,aes(x=year,y=math_c,colour=factor(year_treated))) + geom_line() + 
  labs(x = "Year", y = "Outcome", color = "Cohort (Following Sun et al)") + theme_bw(base_size=16)


#
summary(data[data$treat_==0 ,]$math_c ) # Never treated

summary(data[data$treat_==1 ,]$math_c ) # Treated at some point


###################################################
# TWFE With first period
# OLS Version
###################################################


 #Dummies of time to treat
data$time_to_treat = data$year -  data$year_treated
table(data$time_to_treat)
# data = data[data$time_to_treat<=8 , ]

#
summary(data[data$treat_==0 ,]$math_c ) # Never treated

summary(data[data$treat_==1 ,]$math_c ) # Treated at some point

summary(data[data$treat_==1 & data$time_to_treat >=0 ,]$math_c ) # Already treated

summary(data[data$treat_==1 & data$time_to_treat <0 ,]$math_c ) # Not yet treated


data$treat_relative = ifelse(data$treat_==1 & data$time_to_treat>=0 , 1,0  )

# Mostrar la tabla de estadísticas descriptivas en formato ancho
print(descriptive_statistics_wide)


table(data$treat_relative)

data_avg <- data %>%
  group_by(time_to_treat, treat_) %>%
  summarise(math_c_avg = mean(math_c, na.rm = TRUE))

# Por que es dificil valuar tendencias paralelas? 
# En esencia los no tratados no tienen tiempo relativo al tratamiento. 
ggplot(data_avg, aes(x = time_to_treat, y = math_c_avg, col = factor(treat_))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 4.5, lty = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Time variable", y = "Outcome variable (Average)", col = "Treatment")

# Como es el tratamiento escalnoado en un anel desbalanceado
panelview(math_c ~ treat_relative, data = data[is.na(data$math_c)==F, ], index = c("cole_cod_dane","year"), 
          xlab = "Year", ylab = "Unit", display.all = T,
          gridOff = TRUE, by.timing = TRUE)
# Como es el caso de que sea balanceado
# Paso 1: Identificar los IDs con datos completos
complete_ids <- data %>%
  group_by(cole_cod_dane) %>%
  summarise(n_years = n_distinct(year)) %>%
  filter(n_years == length(unique(data$year))) %>%
  pull(cole_cod_dane)

# Paso 2: Filtrar el dataframe para incluir solo estos IDs
filtered_data <- data %>%
  filter(cole_cod_dane %in% complete_ids)

# Paso 3: Crear todas las combinaciones posibles de cole_cod_dane y var para los IDs filtrados
all_combinations <- expand.grid(
  cole_cod_dane = unique(filtered_data$cole_cod_dane),
  year = unique(filtered_data$year)
)
# Unir las combinaciones con el dataframe filtrado para crear el panel balanceado
balanced_data <- all_combinations %>%
  left_join(filtered_data, by = c("cole_cod_dane", "year"))

# Como es el tratamiento escalnoado en un anel desbalanceado
panelview(math_c ~ treat_relative, data = balanced_data[is.na(balanced_data$math_c)==F, ], index = c("cole_cod_dane","year"), 
          xlab = "Year", ylab = "Unit", display.all = T,
          gridOff = TRUE, by.timing = TRUE)

# Calculate the means using dplyr
cohort_means <- balanced_data %>%
  group_by(year_treated, year) %>%
  summarize(math_c = mean(math_c, na.rm = TRUE), .groups = 'drop')

# plot the means
ggplot(data=cohort_means,aes(x=year,y=math_c,colour=factor(year_treated))) + geom_line() + 
  labs(x = "Year", y = "Outcome", color = "Cohort (Following Sun et al)") + theme_bw(base_size=16)
# Por que es dificil valuar tendencias paralelas? 
 

# Crear variables dummy para los efectos fijos de tiempo y de individuo
data$year_factor <- factor(data$year)
data$id_factor <- factor(data$cole_cod_dane)
# model <- lm(math_c ~ treat_ + year_factor + id_factor, data = data)
# summary(model)


summary( lm(data = data, math_c~  factor(time_to_treat)  ) )
plot_model_effects(lm(data = data, math_c~  factor(time_to_treat)  ))

data_ <- data %>% filter(time_to_treat != -1)
 
 
plot_model_effects(lm(data = data_, math_c~  factor(time_to_treat)  ))
#### Los resultados son consistentes en TWFE?
mod_twfe1 = feols(math_c ~ i(time_to_treat, treat_, ref = -1) + ## Our key interaction: time × treatment status
                   cole_cod_dane + year,                             ## FEs
                 cluster = ~cole_cod_dane,                          ## Clustered SEs
                 data = data)
fixest::iplot(mod_twfe1)
MODEL_SA = feols( math_c_sd ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1)   )    ## The only thing that's changed
                          | id_name + year ,                             ## FEs
                          cluster = ~ id_name ,                         ## Clustered SEs
                          data = subset(df, df$buffer_km == i ) )
fixest::iplot(mod_twfe2)
 
etable(mod_twfe1, mod_twfe2)[0:26,]

#### Los resultados son consistentes en TWFE?
mod_twfe1 = feols(math_c ~ i(time_to_treat, treat_, ref = -1) + ## Our key interaction: time × treatment status
                    cole_cod_dane + year,                             ## FEs
                  cluster = ~cole_cod_dane,                          ## Clustered SEs
                  data = balanced_data)
fixest::iplot(mod_twfe1)
mod_twfe2 = feols(math_c ~ i(time_to_treat, treat_, ref = -1) + ## Our key interaction: time × treatment status
                    cole_naturaleza + estu_trabaja + buffer_km |                    ## Other controls
                    cole_cod_dane + year,                             ## FEs
                  cluster = ~cole_cod_dane,                          ## Clustered SEs
                  data = balanced_data)
fixest::iplot(mod_twfe2)

etable(mod_twfe1, mod_twfe2)[0:26,]


 # Que dice Goodman-Bacon
# 
# 
# Calculate the means using dplyr
balanced_data$treat_cohort = ifelse( is.na(balanced_data$year_treated)==T, 2020, (balanced_data$year_treated))

cohort_means <- balanced_data %>%
  group_by(year_treated, year,treat_relative, treat_cohort) %>%
  summarize(math_c = mean(math_c, na.rm = TRUE), .groups = 'drop')
# balanced_data
data.complete <- cohort_means[which(!is.na(cohort_means$math_c)),] # bacon requires no missingness in the data
 
df_bacon <- bacon(math_c~treat_relative,
                  data = cohort_means,
                  id_var = "treat_cohort",
                  time_var = "year")

ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type), color = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color = 'Type') +
  geom_point()

#######
print(aggregate(df_bacon$estimate * df_bacon$weight, list(df_bacon$type), FUN=sum))

"La descomposición del estimador TWFE en un entorno de adopción escalonada realizada por Goodman-Bacon (2021) 
muestra que las estimaciones de los DID que comparan  cohortes alguna vez tratadas que cambiaron al tratamiento y otras cohortes alguna vez tratadas 
que todavía están en sus períodos previos al tratamiento (puntos rojos) contribuyen al la mayor parte a la estimación TWFE. 
Segudo por los tratados tarde con los siempre tratados.
En tercer lugar las comparaciones prohibidas entre cohortes alguna vez tratadas y las siempre tratadas tratados
(triángulos verdes etiquetados “Más tarde versus siempre tratados) ocupan el segundo y tercer lugar en términos de su contribución. 
Los DID “prohibidos” que comparan cohortes alguna vez tratadas que cambiaron a tratamiento y otras cohortes alguna vez tratadas que ya están tratadas 
(los cuadrados azules etiquetados “Tratados más tarde o más temprano”) son los que menos contribuyen a la estimación de TWFE."
