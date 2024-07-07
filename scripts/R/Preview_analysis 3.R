# install "paneltools"
install.packages("devtools")

if ("paneltools" %in% rownames(installed.packages()) == FALSE) {
  devtools:: install_github("xuyiqing/paneltools")
}
library(paneltools)

# Stacked Dif-Diff 
#  Cengiz et al. (2019),
# Estimate the impact of road construction or modification on educational outcomes. 
# Este método DID apilado elimina el sesgo resultante de las comparaciones "prohibidas" en la descomposición de Goodman-Bacon
 
df.use <- get.cohort(balanced_data,D="treat_",index=c("cole_cod_dane","year"))
df.st <- NULL
target.cohorts <- setdiff(unique(df.use$Cohort),"Control")
k <- 1
for(cohort in target.cohorts){
  df.sub <- df.use[which(df.use$Cohort%in%c(cohort,"Control")),]
  df.sub$stack <- k
  df.st <- rbind(df.st,df.sub)
  k <- k + 1
}
df.st$st_unit <- as.numeric(factor(paste0(df.st$stack,'-',df.st$bfs)))
df.st$st_year <- as.numeric(factor(paste0(df.st$stack,'-',df.st$year)))

# mod_twfe2 = feols(math_c ~ i(time_to_treat, treat_, ref = -1) + ## Our key interaction: time × treatment status
#                     cole_naturaleza + estu_trabaja + buffer_km |                    ## Other controls
#                     cole_cod_dane + year,                             ## FEs
#                   cluster = ~cole_cod_dane,                          ## Clustered SEs
#                   data = data)

model.st <- feols(math_c ~ treat_|st_unit+st_year,
                  data=df.st, cluster = "st_unit")
print(model.st)
df.st$treat <- as.numeric(df.st$treat_relative>0)
df.st[which(is.na(df.st$Time_to_Treatment)),'Time_to_Treatment'] <- 0 # can be arbitrary value
st.est <- feols(math_c ~ i(Time_to_Treatment, treat, ref = -1)| st_unit + st_year,data = df.st,cluster = "st_unit")
print(st.est$coeftable)
st.output <- as.data.frame(st.est$coeftable)
st.output$Time <- c(0:6) #
p.st <- esplot(st.output,Period = 'Time',Estimate = 'Estimate',
               SE = 'Std. Error', xlim = c(-12,10))
p.st


##################
# Interaction Weighted
#  Sun and Abraham (2021)
#################
#Se propone un estimador ponderado por interacción (iw) como medio para estimar efectos de tratamiento heterogéneos.
#Este estimador es un promedio ponderado de las estimaciones promedio del efecto del tratamiento (ATT) para cada cohorte,
#obtenido a partir de una regresión TWFE que incluye variables ficticias de cohorte que interactúan completamente con indicadores 
#de tiempo relativo hasta el inicio del tratamiento. El estimador iw es robusto a los efectos de tratamiento heterogéneos (HTE)
colnames(balanced_data)
balanced_data = data
balanced_data$year_treated_sa = ifelse(is.na(balanced_data$year)==T, 10000, balanced_data$year_treated)
MODEL_SA = feols( math_c ~ sunab(year_treated_sa, year, ref.p = c(.F + -1:4, -1)   )    ## The only thing that's changed
                  | cole_cod_dane + year ,                             ## FEs
                  cluster = ~ cole_cod_dane ,                         ## Clustered SEs
                  data =  balanced_data )
iplot(MODEL_SA)
df.st$treat <- as.numeric(df.st$treatment_mean>0)
df.st[which(is.na(df.st$Time_to_Treatment)),'Time_to_Treatment'] <- 0 # can be arbitrary value
st.est <- feols(nat_rate_ord ~ i(Time_to_Treatment, treat, ref = -1)| st_unit + st_year,data = df.st,cluster = "st_unit")


st.output <- as.data.frame(MODEL_SA$coeftable)
st.output$Time <- c(-3:-2, 0:12) #
p.st <- esplot(st.output,Period = 'Time',Estimate = 'Estimate',
               SE = 'Std. Error', xlim = c(-12,10))
p.st

##################
# doubly-robust estimator 
#Callaway and Sant’Anna (2021) 
##################

#Incorpora covariables previas al tratamiento utilizando unidades nunca tratadas o aún no tratadas como grupo de comparación.
#En este tutorial, solo utilizaremos el modelo de resultados en lugar del modelo doble robusto para calcular el ATT y los efectos del tratamiento dinámico.
#Primero, utilizamos las unidades nunca tratadas como grupo de comparación. 
# El ATT estimado es numéricamente el mismo que el estimador iw. Además, configuramos la opción bstrap en FALSO para informar el error estándar analítico, que es 0,302.


df.cs <- df.use

balanced_data$year_treated_cs = ifelse(is.na(balanced_data$year_treated)==T, 0, balanced_data$year_treated)
balanced_data$cole_cod_dane = as.numeric(balanced_data$cole_cod_dane)
cs.est.1 <- att_gt(yname = "math_c",
                   gname = "year_treated_cs",
                   idname = "cole_cod_dane",
                   tname = "year",
                   xformla = ~1,
                   control_group = "notyettreated",
                   allow_unbalanced_panel = F,
                   data = balanced_data)
cs.est.att.1 <- aggte(cs.est.1, type = "simple", na.rm=T, bstrap = F)
print(cs.est.att.1)

cs.att.1 <- aggte(cs.est.1, type = "dynamic",
                  bstrap=FALSE, cband=FALSE, na.rm=T) 
print(cs.att.1)
cs.output <- cbind.data.frame(Estimate = cs.att.1$att.egt,
                             SE = cs.att.1$se.egt,
                             time = cs.att.1$egt + 1)
p.cs.1 <- esplot(cs.output,Period = 'time',Estimate = 'Estimate',
                 SE = 'SE', xlim = c(-12,10))
p.cs.1

##################
# Panel Match
#Imai, Kim, and Wang (2021)
##################
# El estimador PanelMatch es equivalente al estimador de diferencias en diferencias con emparejamiento (DIDM) propuesto por Chaisemartin y D’Haultfœuille (2020) 
#Este método proporciona un marco más flexible y robusto para estimar el impacto de un tratamiento al permitir emparejar observaciones 
#tratadas con no tratadas de manera más precisa, teniendo en cuenta tanto el historial de tratamiento como las covariables. 
#El uso del comando PanelMatch facilita este proceso al automatizar la creación de conjuntos coincidentes y 
#la estimación del efecto del tratamiento.

df.pm <- balanced_data
# we need to convert the unit and time indicator to integer
df.pm$cole_cod_dane<- as.integer(as.factor(df.pm[,"cole_cod_dane"]))
df.pm[,"year"] <- as.integer(as.factor(df.pm[,"year"]))
#df.pm <- df.pm[,c("cole_cod_dane","year","math_c","treat_","treat") ]
colnames(df.pm)


#

PM.results <- PanelMatch(lag=1, 
                         time.id="year", 
                         unit.id = "cole_cod_dane", 
                         treatment = 'treat_', 
                         refinement.method = "none", 
                         data = df.pm, 
                         qoi = "att", 
                         lead = c(0:3), 
                         outcome.var = 'math_c', 
                         match.missing = TRUE)

## For pre-treatment dynamic effects
PM.results.placebo <- PanelMatch(lag=3, 
                                 time.id="year", 
                                 unit.id = "cole_cod_dane", 
                                 treatment = 'treat_', 
                                 refinement.method = "none", 
                                 data = df.pm, 
                                 qoi = "att", 
                                 lead = c(0:3), 
                                 outcome.var = 'math_c', 
                                 match.missing = TRUE,
                                 placebo.test = TRUE)
PE.results.pool <- PanelEstimate(PM.results, data = df.pm, pooled = TRUE)
summary(PE.results.pool)

# Dynamic Treatment Effects
PE.results <- PanelEstimate(PM.results, data = df.pm)
PE.results.placebo <- placebo_test(PM.results.placebo, data = df.pm, plot = F)

est_lead <- as.vector(PE.results$estimates)
est_lag <- as.vector(PE.results.placebo$estimates)
sd_lead <- apply(PE.results$bootstrapped.estimates,2,sd)
sd_lag <- apply(PE.results.placebo$bootstrapped.estimates,2,sd)
coef <- c(est_lag, 0, est_lead)
sd <- c(sd_lag, 0, sd_lead)
pm.output <- cbind.data.frame(ATT=coef, se=sd, t=c(-2:4))
p.pm <- esplot(data = pm.output,Period = 't',
               Estimate = 'ATT',SE = 'se')
p.pm

##################
#Imputation Method
#Liu, Wang, and Xu (2022) , Borusyak, Jaravel, and Spiess (2021) and Gardner (2021)
##################
#
#Liu, Wang y Xu (2022) proponen un método para estimar un modelo contrafactual con efectos fijos utilizando solo las observaciones no tratadas. 
#El modelo se representa por la ecuación \( Y_{it} = \alpha_i + \xi_t + \delta \text{TWFE} D_{it} + \epsilon \), 
#donde los parámetros \(\alpha_i\) y \(\xi_t\) se estiman usando solo observaciones no tratadas y se imputa el contrafactual usando los valores estimados
# \(\hat{\alpha_i}\) y \(\hat{\xi_t}\). 
#Este método, también propuesto independientemente por Borusyak, Jaravel y Spiess (2021) y Gardner (2021), 
#se conoce como el "método de imputación" o "DID en dos etapas". 
#Para implementar este estimador, se puede utilizar el método de efectos fijos (fe) en el paquete `fect`.


out.fect <- fect(math_c~treat_, data = balanced_data, index = c("cole_cod_dane","year"),
                 method = 'fe', se = TRUE)
print(out.fect$est.avg)

fect.output <- as.matrix(out.fect$est.att)
print(fect.output)


fect.output <- as.data.frame(fect.output)

fect.output$Time <- row.names(fect.output)
p.fect <- esplot(fect.output,Period = 'Time',Estimate = 'ATT',
                 SE = 'S.E.',CI.lower = "CI.lower", 
                 CI.upper = 'CI.upper',xlim = c(-4,6))
fect.output$Time = as.numeric(fect.output$Time )
# Crear la gráfica con ggplot2
ggplot(fect.output, aes(x = Time, y = ATT)) +
  geom_point(color = "blue") +
  # geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = -1, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = CI.lower, ymax = CI.upper), width = 0.2, color = "blue") +
  labs(x = "Time", y = "ATT", title = "ATT with Confidence Intervals Over Time") +
  theme_minimal()
