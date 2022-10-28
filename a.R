df = base_10p

MODELO_SA = feols( math_c_sd ~ sunab(year_treated_sa, year )    ## The only thing that's changed
                  | id_name + year ,                             ## FEs
                  cluster = ~ id_name ,                         ## Clustered SEs
                  data = subset(df, df$buffer_km == 1000 ) )
MODELO_SA1 = feols( math_c_sd ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) )    ## The only thing that's changed
                   | id_name + year ,                             ## FEs
                   cluster = ~ id_name ,                         ## Clustered SEs
                   data = subset(df, df$buffer_km == 1000 ) )
MODELO_SA3 = feols( math_c_sd_nat ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) )    ## The only thing that's changed
                    | id_name + year ,                             ## FEs
                    cluster = ~ id_name ,                         ## Clustered SEs
                    data = subset(df, df$buffer_km == 1000 ) )


MODELO_SA2 = feols( math_c_sd ~ sunab(year_treated_sa, year, ref.p = c(.F + 0:3, -1) )    ## The only thing that's changed
                    | id_name + year ,                             ## FEs
                    cluster = ~ id_name ,                         ## Clustered SEs
                    data = subset(df, df$buffer_km == 1000 ) )

etable(MODELO_SA1 ,MODELO_SA3  )
iplot( list(MODELO_SA1 ,MODELO_SA3 )  )
iplot( MODELO_SA1  )
iplot( MODELO_SA  )
iplot( list(MODELO_SA,MODELO_SA1 ,MODELO_SA2 )  )
summary(MODELO_SA)
df$estu_trabaja
MODELO_SA3 = feols( estu_trabaja ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) )    ## The only thing that's changed
                    | id_name + year ,                             ## FEs
                    cluster = ~ id_name ,                         ## Clustered SEs
                    data = subset(df, df$buffer_km == 1000 &  df$cole_naturaleza == 'PRIV') )

MODELO_SA1 = feols( estu_trabaja ~ sunab(year_treated_sa, year, ref.p = c(.F + -2:2, -1) )    ## The only thing that's changed
                    | id_name + year ,                             ## FEs
                    cluster = ~ id_name ,                         ## Clustered SEs
                    data = subset(df, df$buffer_km == 1000 & 
                                    df$cole_naturaleza == 'PUBL') )

iplot( list(MODELO_SA1 )  , pt.join = TRUE , main = "PUBL"  )
iplot( list(MODELO_SA3 ), pt.join = TRUE  , main = "PRIV")

iplot( list(MODELO_SA1 , MODELO_SA3 )  )
