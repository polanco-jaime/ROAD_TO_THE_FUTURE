# Libraries DiD
lista = c('did2s', 
          'did' ,    'didimputation',  
          'DIDmultiplegt' , 'fixest',
          'staggered', 'DRDID','bacondecomp', 'TwoWayFEWeights'
)

for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}
remotes::install_github("jonathandroth/pretrends")
#install.packages("remotes") # if remotes package not installed
#Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
#remotes::install_github("asheshrambachan/HonestDiD", force = TRUE)
# Implements 
# Gardner (2021), Borusyak et al (2021), Sun  and Abraham (2020), 
# Callaway and Sant'Anna (2021), Roth and Sant'Anna (2021)

# Heterogeneity Robust Estimators for Staggered Timing 



#######################
library('did2s') # Implements Gardner (2021), Borusyak et al (2021), Sun and Abraham (2020), Callaway and Sant'Anna (2021), Roth and Sant'Anna (2021)
base_10p$math_c
?event_study

df = base_ai
table(base_10p$id_name)

kilometros = c(1000,1500,2000,2500,3000,3500,4000,4500)
table(df$year_treated)
i = 1
df = base_ent
df_hom = subset(df, df$buffer_km == 1000 )

did2s::event_study( estimator = c(  "impute"),
                    data = df_hom,
                    yname  = 'math_c',
                    idname = "id_name",
                    gname = 'year_treated',
                    tname = 'year' )
 

did_imputation(data = df_hom, yname = "math_c", gname = "year_treated",
               tname = "year", idname = "id_name",
               first_stage = ~ 0 | id_name + year,
               horizon=TRUE, pretrends = F)
df = get('base_ent')
df = df %>% subset(df$time_to_treat >= -8 & df$year <= 2014)
####### Modelos
MODEL = feols(Participate_saberpro ~ i(time_to_treat, treat, ref = reference_time) # + ## Our key interaction: time × treatment status
              |  cole_cod_dane+ year,                             ## FEs
              cluster = ~ cole_cod_dane,                              ## Clustered SEs
              data = subset(df, df$buffer_km == i 
              ) )

MODEL_SA = feols(Participate_saberpro ~ sunab(year_treated_sa, year) #+ ## The only thing that's changed
                 |  cole_cod_dane+ year,                             ## FEs
                 cluster = ~ cole_cod_dane,                         ## Clustered SEs
                 data = subset(df, df$buffer_km == i ) )
plot_event_study(Math_10p, seperate = TRUE, horizon = NULL)

library('did') # Implements Callaway and Sant'Anna (2021)

library('didimputation') #  Implements Borusyak, Jaravel, and Spiess (2021)

library('DIDmultiplegt') # Implements de Chaisemartin and D'Haultfoeuille (2020)

library("wooldridge")
Y = "lwage"
G = "nr"
T = "year"
D = "union"
controls = c("hours")

did_multiplegt(wagepan, Y, G, T, D, controls)

placebo = 1
dynamic = 1

# calculate placebo and dynamic effects 
 did_multiplegt(wagepan, Y, G, T, D, controls, placebo = placebo, dynamic = dynamic)

# perform bootstrap
 did_multiplegt(wagepan, Y, G, T, D, controls, placebo = placebo, dynamic = dynamic, brep = 2)

# perform cluster bootstrap on cluster nr and save it as dataframe A
# A <- did_multiplegt(wagepan, Y, G, T, D, controls, placebo = placebo, dynamic = dynamic,
#                     brep = 2, cluster = "nr")
#df data frame or data matrix.
#Y the outcome variable.
#G the group variable.
#T the time period variable.
#D the treatment variable.

did_multiplegt(subset(base_10p,base_10p$buffer_km ==1500 ), Y, G, T, D)



library('fixest') # Implements Sun and Abraham (2020)

library('staggered') #Implements Roth and Sant'Anna (2021), Callaway and Sant'Anna (2020), and Sun and Abraham (2020)
?staggered

# Diagnostics for TWFE with Staggered Timing


library('TwoWayFEWeights') #Diagnostics from de Chaisemartin and D'Haultfoeuille


# Diagnostic / Sensitivity for Violations of Parallel Trends




library('honestDiD')  #  Implements Rambachan and Roth (2021)

library('pretrends') # Diagnostics from Roth (2021)
################################################



A = table_result_TWFE_SA_C(Buffer = '1000 Metros Matematicas', 
                           TWFE_result = TWFE_Math_base_10p[1],
                           SA_result =SA_Math_base_10p[1], 
                           ATT_result = ATT_Math_base_10p)


subject = A[1,2]
title = A[1,5]
period = A[c(3:(nrow(A)-8)), c(1)]
library(dplyr)
TWFE   = A[c(3:(nrow(A)-8)), c(1,2)]     %>% 
  cbind(data.frame (stringr::str_split_fixed(TWFE[[2]], " ", 2) ))  %>%
  mutate(X1  =as.numeric( X1 ) ) %>%
  mutate(X2  =as.numeric( X2 ) ) %>%
  mutate(Periodo  =as.numeric( Periodo ) )%>%
  rename(coefs  =  X1)    %>%
  rename(ses  =  X2)     %>%
  select(c('Periodo','coefs','ses' ))   


TWFE$Method = 'TWFE'  

SUN = A[c(3:(nrow(A)-8)), c(1,3)]     %>% 
  cbind(data.frame (stringr::str_split_fixed(SUN[[2]], " ", 2) ))  %>%
  mutate(X1  =as.numeric( X1 ) ) %>%
  mutate(X2  =as.numeric( X2 ) ) %>%
  mutate(Periodo  =as.numeric( Periodo ) )%>%
  rename(coefs  =  X1)    %>%
  rename(ses  =  X2)     %>%
  select(c('Periodo','coefs','ses' ))   

SUN$Method = 'Sun et'  
Callaway =  A[c(3:(nrow(A)-8)), c(1,4)]     %>% 
  cbind(data.frame (stringr::str_split_fixed(Callaway[[2]], " ", 2) ))  %>%
  mutate(X1  =as.numeric( X1 ) ) %>%
  mutate(X2  =as.numeric( X2 ) ) %>%
  mutate(Periodo  =as.numeric( Periodo ) )%>%
  rename(coefs  =  X1)    %>%
  rename(ses  =  X2)     %>%
  select(c('Periodo','coefs','ses' ))   

Callaway$Method = 'Callaway et' 

All_in = rbind(SUN, TWFE)
All_in = rbind(All_in, Callaway)

minimo = min(All_in$coefs-1.96*All_in$ses)
maximo = max(All_in$coefs+1.96*All_in$ses)

ggplot(data=All_in, mapping=aes(y=coefs, x=period, group=Method, col=Method, fill=Method)) +
  geom_line(linetype="dashed") +
  geom_point() + 
  geom_errorbar(aes(ymin=(coefs-1.96*ses), ymax=(coefs+1.96*ses)), width=0.2) +
  ylim(c(minimo,maximo)) +
  theme_bw()

library(ggplot2)

plt_TWFE =  ggplot(data=TWFE, mapping=aes(y=coefs, x=period  )) +
  geom_line(linetype="dashed") +
  geom_point(aes(color = "#E7B800")) + 
  geom_errorbar(aes(ymin=(coefs-1.96*ses), ymax=(coefs+1.96*ses)), width=0.2) +
  ylim(c(minimo,maximo)) +
  theme_bw()
plt_TWFE
plt_SUN = 
  plt_SUN

ggparagraph(plt_TWFE)
#


library(stringr)
SUN_et = A[c(3:(nrow(A)-8)), c(1,3)]
Callaway =  A[c(3:(nrow(A)-8)), c(1,4)]

