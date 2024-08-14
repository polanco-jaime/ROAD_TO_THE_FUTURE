load("Data/PISAT.Rda")
colnames(PISAT)
PISAT_ = PISAT[, c('PV1MATH', 'PV1READ', 'Year', 'CNT' ,  "SENWT1"      , "W_HOUSEWHT1"      )]
PISAT_= sqldf("SELECT CNT,  AVG(PV1MATH) as PV1MATH, AVG(PV1READ) as PV1READ, AVG(SENWT1) as SENWT1, AVG(W_HOUSEWHT1) as W_HOUSEWHT1
      FROM PISAT_ WHERE Year = 2018
      GROUP BY 1
      ")


rai <- read_excel("Data/rai.xlsx", skip = 2)

# Load required package
library(dplyr)


# Create a named vector for country-to-code mapping
country_codes <- c(
  "Angola" = "AGO", "Benin" = "BEN", "Botswana" = "BWA", "Burkina Faso" = "BFA",
  "Burundi" = "BDI", "Cameroon" = "CMR", "Cape Verde" = "CPV", "Chad" = "TCD",
  "Comoros" = "COM", "Congo, Dem. Rep." = "COD", "Congo, Rep." = "COG",
  "Cote d'Ivoire" = "CIV", "Equatorial Guinea" = "GNQ", "Eritrea" = "ERI",
  "Ethiopia" = "ETH", "Gabon" = "GAB", "Gambia, The" = "GMB", "Ghana" = "GHA",
  "Guinea" = "GIN", "Guinea-Bissau" = "GNB", "Kenya" = "KEN", "Lesotho" = "LSO",
  "Liberia" = "LBR", "Madagascar" = "MDG", "Malawi" = "MWI", "Mali" = "MLI",
  "Mauritania" = "MRT", "Mauritius" = "MUS", "Mozambique" = "MOZ", "Namibia" = "NAM",
  "Niger" = "NER", "Nigeria - 8 states" = "NGA", "Rwanda" = "RWA",
  "Sao Tome and Principe" = "STP", "Senegal" = "SEN", "Sierra Leone" = "SLE",
  "Somalia" = "SOM", "South Africa" = "ZAF", "Sudan" = "SDN", "Tanzania" = "TZA",
  "Togo" = "TGO", "Uganda" = "UGA", "Zambia" = "ZMB", "Zimbabwe" = "ZWE",
  "Brunei Darussalam" = "BRN", "Cambodia" = "KHM", "China" = "CHN", "Fiji" = "FJI",
  "Indonesia" = "IDN", "Japan" = "JPN", "Korea, Dem. Rep." = "PRK", "Korea, Rep." = "KOR",
  "Lao PDR" = "LAO", "Malaysia" = "MYS", "Micronesia, Fed. Sts." = "FSM",
  "Mongolia" = "MNG", "Myanmar" = "MMR", "New Zealand" = "NZL",
  "Papua New Guinea" = "PNG", "Philippines" = "PHL", "Samoa" = "WSM",
  "Solomon Islands" = "SLB", "Thailand" = "THA", "Timor-Leste" = "TLS",
  "Tonga" = "TON", "Vanuatu" = "VUT", "Vietnam" = "VNM", "Albania" = "ALB",
  "Armenia" = "ARM", "Austria" = "AUT", "Azerbaijan" = "AZE", "Belarus" = "BLR",
  "Belgium" = "BEL", "Bosnia and Herzegovina" = "BIH", "Bulgaria" = "BGR",
  "Croatia" = "HRV", "Cyprus" = "CYP", "Czech Republic" = "CZE", "Denmark" = "DNK",
  "Estonia" = "EST", "Finland" = "FIN", "France" = "FRA", "Georgia" = "GEO",
  "Germany" = "DEU", "Greece" = "GRC", "Hungary" = "HUN", "Iceland" = "ISL",
  "Ireland" = "IRL", "Italy" = "ITA", "Kazakhstan" = "KAZ", "Kyrgyz Republic" = "KGZ",
  "Latvia" = "LVA", "Lithuania" = "LTU", "Macedonia, FYR" = "MKD", "Moldova" = "MDA",
  "Netherlands" = "NLD", "Norway" = "NOR", "Poland" = "POL", "Portugal" = "PRT",
  "Romania" = "ROU", "Russian Federation" = "RUS", "Serbia and Montenegro" = "SCG",
  "Slovenia" = "SVN", "Spain" = "ESP", "Sweden" = "SWE", "Tajikistan" = "TJK",
  "Turkey" = "TUR", "Turkmenistan" = "TKM", "Ukraine" = "UKR",
  "United Kingdom" = "GBR", "Uzbekistan" = "UZB", "Argentina" = "ARG",
  "Bahamas, The" = "BHS", "Barbados" = "BRB", "Belize" = "BLZ", "Bolivia" = "BOL",
  "Brazil" = "BRA", "Chile" = "CHL", "Colombia" = "COL", "Costa Rica" = "CRI",
  "Cuba" = "CUB", "Dominica" = "DMA", "Dominican Republic" = "DOM",
  "Ecuador" = "ECU", "El Salvador" = "SLV", "Grenada" = "GRD", "Guatemala" = "GTM",
  "Guyana" = "GUY", "Haiti" = "HTI", "Honduras" = "HND", "Jamaica" = "JAM",
  "Mexico" = "MEX", "Nicaragua" = "NIC", "Panama" = "PAN", "Paraguay" = "PRY",
  "Peru" = "PER", "Puerto Rico" = "PRI", "St. Kitts and Nevis" = "KNA",
  "St. Lucia" = "LCA", "St. Vincent and the Grenadines" = "VCT", "Suriname" = "SUR",
  "Trinidad and Tobago" = "TTO", "Uruguay" = "URY", "Venezuela, RB" = "VEN",
  "Algeria" = "DZA", "Bahrain" = "BHR", "Djibouti" = "DJI", "Egypt, Arab Rep." = "EGY",
  "Iran, Islamic Rep." = "IRN", "Iraq" = "IRQ", "Israel" = "ISR", "Jordan" = "JOR",
  "Kuwait" = "KWT", "Lebanon" = "LBN", "Libya" = "LBY", "Malta" = "MLT",
  "Morocco" = "MAR", "Oman" = "OMN", "Qatar" = "QAT", "Saudi Arabia" = "SAU",
  "Syrian Arab Republic" = "SYR", "Tunisia" = "TUN", "United Arab Emirates" = "ARE",
  "Yemen, Rep." = "YEM", "Afghanistan" = "AFG", "Bangladesh" = "BGD", "Bhutan" = "BTN",
  "India" = "IND", "Maldives" = "MDV", "Nepal" = "NPL", "Pakistan" = "PAK",
  "Sri Lanka" = "LKA", 'Canada' = 'CAN', 'United States' = 'USA')


rai <- rai %>% mutate(CountryCode = country_codes[Country])
colnames(rai)[7] =  "RuralAccessIndex"
rai <- rai[ is.na(rai[[11]])==F , ]
rai <- rai[ rai[[7]]>=0 , ]

union_ = sqldf::sqldf("SELECT * FROM PISAT_
             INNER JOIN rai
             ON CountryCode = CNT
             ")

ggplot(union_, aes(x = RuralAccessIndex, y = PV1MATH)) +
  geom_point(color = "steelblue", alpha = 0.7) +  # Scatter plot with blue points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line in red
  labs(title = "PISA 2018: Math Scores vs. Rural Access Index",
       x = "Rural Access Index",
       y = "Math Score (PV1MATH)") +
  theme_bw() +  # Clean background theme
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) 


# Fit the linear model
model <- lm(PV1MATH ~ RuralAccessIndex, data = union_)
# install.packages("ggpmisc")

library(ggpmisc) # For the "stat_poly_eq" function

model <- lm(PV1MATH ~ RuralAccessIndex, data = union_)


png(paste0("Graph/RAI_PISA_math.png"), width = 1030, height = 598)

ggplot(union_, aes(x = RuralAccessIndex, y = PV1MATH)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "PISA 2018: Math Scores vs. Rural Access Index",
       x = "Rural Access Index",
       y = "Math Score (PV1MATH)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  stat_poly_eq(formula = y ~ x,  # Use 'x' and 'y' for the formula
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")*")), 
               parse = TRUE, size = 3.5, 
               label.x = 0.1, label.y = 0.9) 


 
dev.off()

model <- lm(PV1READ ~ RuralAccessIndex, data = union_)

png(paste0("Graph/RAI_PISA_reading.png"), width = 1030, height = 598)
ggplot(union_, aes(x = RuralAccessIndex, y = PV1READ)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "PISA 2018: Reading Scores vs. Rural Access Index",
       x = "Rural Access Index",
       y = "Reading Score (PV1READ)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  stat_poly_eq(formula = y ~ x,  # Use 'x' and 'y' for the formula
               aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")*")), 
               parse = TRUE, size = 3.5, 
               label.x = 0.1, label.y = 0.9) 
dev.off()
summary(union_[union_[[10]]<=0.5,])
union_$RuralAccessIndex2 = union_$RuralAccessIndex*100
summary(lm(PV1READ ~ log(RuralAccessIndex), data =  union_))

summary(lm(PV1READ ~ RuralAccessIndex2+SENWT1 + W_HOUSEWHT1 , data =  union_))
summary(lm(PV1MATH~ RuralAccessIndex2+SENWT1 + W_HOUSEWHT1 , data =  union_))
1.6643*15
summary(lm(PV1MATH ~ log(RuralAccessIndex), data = union_))

# Al comparar el indice de acceso rural con las pruebas pisa, 
# se puede observar que aquellos paises que poseen mayor acceso a una via 
# transitable tienen mejores resultados en matematicas y lenguaje de la prueba PISA. De hecho, un aumento de 15% en el indice de accecibilidad rural implica 22 puntos adicionales en lenguaje y 25 puntos adicionales en matematicas. 
# Esto en pruebas pisa es equivalente a mas de un año de aprendizaje escolar. 
# 20 puntos 
######################################
ggplot(union_, aes(x = RuralAccessIndex, y = PV1MATH, color = Region)) + # Color by Region
  geom_point(alpha = 0.7) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "PISA 2018: Math Scores vs. Rural Access Index by Region",
       x = "Rural Access Index",
       y = "Math Score (PV1MATH)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) 
