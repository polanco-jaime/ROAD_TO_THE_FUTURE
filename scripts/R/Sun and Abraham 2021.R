# Aggregate time observation perior in two years bin.rel = "bin::2" 
# ref_p= c(  -1:2, -2)
sqldf::sqldf("  SELECT DISTINCT delta_0p_100p,   NOMBRE  TOT FROM data_ai WHERE delta_0p_100p IS NOT NULL ") 
sqldf::sqldf("  SELECT DISTINCT   delta_0p_10p,delta_10p_100p,  NOMBRE   FROM data_10p ") 
sqldf::sqldf("  SELECT DISTINCT   delta_0p_50p,delta_50p_100p,  NOMBRE   FROM data_50p  ") 
sqldf::sqldf("  SELECT DISTINCT delta_0p_100p,   NOMBRE  TOT FROM data_100p ") 

sqldf::sqldf("  SELECT DISTINCT delta_0p_100p, delta_0p_10p,  delta_10p_100p, 
             delta_0p_50p,delta_50p_100p,delta_0p_100p, 
             NOMBRE  TOT FROM data_ai ") 
 
###################################################################
## Math and Reading Results by period
#0% of the Concession Progress
table(data_ai$year_treated_ai)
# data_ai = data_ai[data_ai$year_treated_ai!=2010, ]
result = sqldf::sqldf("  SELECT DISTINCT delta_0p_100p,   NOMBRE  TOT FROM data_ai WHERE delta_0p_100p IS NOT NULL ") 
as.integer(-1-round(median(result[[1]], na.rm = T), 0))
as.integer(-1-round(mean(result[[1]], na.rm = T), 0))
writeLines( create_latex_table(result,  caption = "Your caption here", label = "tab:yourlabel") , 
            'Tables/delta_time_ai_100p.txt')

 table(data_ai$time_to_treat_ai)
ref_point =  c(  -1:10 ) 
length(ref_point)
SA_math_0p = feols(sd_math_i ~ #cole_jornada+estu_ocup_madre_category+
                     sunab(year_treated_ai, year  ,  ref.p = ref_point ) |
        cole_cod_dane_institucion+  year,
      cluster = ~cole_cod_dane_institucion+NOMBRE,
      data = data_ai )
 
 
SA_reading_0p = feols(sd_reading_i ~ #cole_jornada+estu_ocup_madre_category+
                        sunab(year_treated_ai, year  ,  ref.p = ref_point ) |
                     cole_cod_dane_institucion+  year,
                   cluster = ~cole_cod_dane_institucion+NOMBRE,
                   data = data_ai )
etable(SA_math_0p)
summary(SA_math_0p, agg = "cohort") 
summary(SA_math_0p, agg = "ATT")  


summary(SA_reading_0p, agg = "ATT")  
iplot(SA_reading_0p ) 
iplot(SA_math_0p ) 

sunab_att_table('SA_reading_0p')
sunab_att_table('SA_math_0p')
#10% of the Concession Progress
#  ref.p= c(  -1:2, -2)
###################################################################
## Math and Reading Results by period
#10% of the Concession Progress
 
ref_point_10p =  c(  -5:(-5+length(ref_point)) ) 
length(ref_point_10p)
SA_math_10p = feols(sd_math_i ~ #cole_jornada+estu_ocup_madre_category+
                      sunab(year_treated_10p, year  ,   ref.p= ref_point_10p ) |
                     cole_cod_dane_institucion+  year,
                   cluster = ~cole_cod_dane_institucion+NOMBRE,
                   data = data_10p )
summary(SA_math_10p, agg = "ATT")  
iplot(SA_math_10p )

SA_reading_10p = feols(sd_reading_i ~ #cole_jornada+estu_ocup_madre_category+estu_genero+
                         sunab(year_treated_10p, year  ,   ref.p= ref_point_10p ) |
                      cole_cod_dane_institucion+  year,
                    cluster = ~NOMBRE+cole_cod_dane_institucion,
                    data = data_10p )
summary(SA_reading_10p, agg = "ATT")  
iplot(SA_reading_10p )

iplot(list(SA_math_10p, SA_reading_10p) , ref = "all")
etable(SA_math_10p, SA_reading_10p)

sunab_att_table('SA_math_10p')
sunab_att_table('SA_reading_10p')
#50% of the Concession Progress  
ref_point_50p =  c(  -6:(-6+length(ref_point)) ) 
length(ref_point_50p)

SA_math_50p = feols(sd_math_i ~ #DISTANCE+ estu_genero + GEN+
                      sunab(year_treated_50p, year  , ref.p= ref_point_50p ) |
                      cole_cod_dane_institucion+  year,
                    cluster = ~cole_cod_dane_institucion+NOMBRE,
                    data = data_50p )
summary(SA_math_50p, agg = "ATT") 
iplot(SA_math_50p)

SA_reading_50p = feols(sd_reading_i ~ #DISTANCE+ estu_genero + GEN+
                      sunab(year_treated_50p, year  , ref.p= ref_point_50p ) |
                      cole_cod_dane_institucion+  year,
                    cluster = ~cole_cod_dane_institucion+NOMBRE,
                    data = data_50p )
summary(SA_reading_50p, agg = "ATT") 
iplot(SA_reading_50p)
summary(SA_reading_50p)

#100% of the Concession Progress 

SA_math_100p = feols(sd_math_i ~#cole_jornada+estu_ocup_madre_category+
                       sunab(year_treated_100p, year,  ref.p= c(  -11:0)   )
                     |
                      cole_cod_dane_institucion+  year,
                    cluster = ~cole_cod_dane_institucion+NOMBRE,
                    data = data_100p )
iplot(SA_math_100p)
etable(SA_math_100p)
colnames(data_100p)
table(data_10p$estu_ocup_madre_category)
SA_reading_100p = feols(sd_reading_i ~ #cole_jornada+estu_ocup_madre_category+
                          sunab(year_treated_100p, year,  ref.p= c(  -12:0)   ) |
                       cole_cod_dane_institucion+  year,
                     cluster = ~cole_cod_dane_institucion+NOMBRE,
                     data = data_100p )
 
iplot(SA_reading_100p)
###################################################
library(dplyr)



# Example usage
sunab_latex_staggered(
  list(get('SA_math_0p'), get('SA_math_10p'), get('SA_math_50p'), get('SA_math_100p')),
  "Dynamic DiD Estimation for Mathematics Scores",
  "table:math_scores"
)

sunab_latex_staggered(
  list(get('SA_reading_0p'), get('SA_reading_10p'), get('SA_reading_50p'), get('SA_reading_100p')),
  "Dynamic DiD Estimation for Reading Scores",
  "table:reading_staggered_scores"
)


 


# Example usage:
models <- c('SA_math_0p', 'SA_math_10p', 'SA_math_50p', 'SA_math_100p')
completions <- c("0%", "10%", "50%", "100%")
etable(get('SA_math_0p'), get('SA_math_10p') , get('SA_math_50p'), get('SA_math_100p'))
?etable
final_table <- unify_sunab_att_tables(models, completions, att_outcome ='att_sunab_math')
print(final_table)

models <- c('SA_reading_0p', 'SA_reading_10p', 'SA_reading_50p', 'SA_reading_100p')
completions <- c("0%", "10%", "50%", "100%")

final_table <- unify_sunab_att_tables(models, completions, att_outcome ='att_sunab_reading')
print(final_table)
############################### Math final results
est = "SUNAB_2021"
# First plot
title1 <- "Impact of 0% Road Concession Progress\n on Math Scores"
png(paste0("Graph/", generate_file_name(title1), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_math_0p),   horizon = NULL, TITULO = title1)
dev.off()

# Second plot
title2 <- "Impact of 10% Road Concession Progress\n on Math Scores"
png(paste0("Graph/", generate_file_name(title2), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_math_10p), horizon = NULL, TITULO = title2)
dev.off()

# Third plot
title3 <- "Impact of 50% Road Concession Progress\n on Math Scores"
png(paste0("Graph/", generate_file_name(title3), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_math_50p), horizon = NULL, TITULO = title3)
dev.off()

# Fourth plot
title4 <- "Impact of 100% Road Concession Progress\n on Math Scores"
png(paste0("Graph/", generate_file_name(title4), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_math_100p), horizon = NULL, TITULO = title4)
dev.off()
############################### Reading final results
# First plot

title1 <- "Impact of 0% Road Concession Progress\n on Reading Literacy Scores"
png(paste0("Graph/", generate_file_name(title1), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_reading_0p),  horizon = NULL, TITULO = title1)
dev.off()

# Second plot
title2 <- "Impact of 10% Road Concession Progress\n on Reading Literacy Scores"
png(paste0("Graph/", generate_file_name(title2), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_reading_10p) , horizon = NULL, TITULO = title2)
dev.off()

# Third plot
title3 <- "Impact of 50% Road Concession Progress\n on Reading Literacy Scores"
png(paste0("Graph/", generate_file_name(title3), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_reading_50p)[SA_Result_table(SA_reading_50p)$period >= -14, ],   horizon = NULL, TITULO = title3)
dev.off()

# Fourth plot
title4 <- "Impact of 100% Road Concession Progress\n on Reading Literacy Scores"
png(paste0("Graph/", generate_file_name(title4), "_", est, ".png"), width = 1030, height = 598)
event_study_plot_sunab_2021(SA_Result_table(SA_reading_100p),   horizon = NULL, TITULO = title4)
dev.off()

