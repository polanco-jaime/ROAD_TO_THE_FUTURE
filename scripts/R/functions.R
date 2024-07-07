#load("C:/Users/USER/Desktop/DID roads/did_roads/did_roads.RData")
lista = c('readr','readxl','sqldf','plyr', 
          'did' , 'arrow',  'plyr', 'ggplot2',
          'dplyr','fixest' , 'gargle' , 'stringr'
          #, 'bigrquery' 
)
for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
}

###################################
#picture buffers
generate_file_name <- function(title) {
  # Remove special characters, spaces, and make lowercase
  cleaned_title <- gsub("[^[:alnum:]]", "_", title)
  cleaned_title <- gsub("\\s+", "_", cleaned_title)
  cleaned_title <- gsub("__", "_", cleaned_title)
  cleaned_title <- tolower(cleaned_title)
  return(cleaned_title)
}

event_study_plot_sunab_2021 = function (out, seperate = TRUE, horizon = NULL, TITULO= '') {

  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "sans", face = "bold", size = 15, hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "sans"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "sans"),
    axis.title = element_text(family = "sans", face = "bold", size = 12, colour = "steelblue4"),
    axis.text = element_text(family = "mono", face = "bold", colour = "cornflowerblue", size = 12),
    legend.position = "bottom"
  )
  estimators = unique(out$estimator)
  levels = c("TWFE", "Borusyak, Jaravel, Spiess (2021)", "Callaway and Sant'Anna (2021)", 
             "Gardner (2021)", "Roth and Sant'Anna (2021)", "Sun and Abraham (2021)",
             "Score at 1000 Meters" ,"Score at 1500 Meters" ,"Score at 2000 Meters",
             "Score at 2500 Meters" ,"Score at 3000 Meters" ,"Score at 3500 Meters",
             "Score at 4000 Meters" ,"Score at 4500 Meters", 'Private schools' , 
             'Public schools' ,  'All sample schools' 
  )
  
  levels = levels[levels %in% estimators]
  out$estimator = factor(out$estimator, levels = levels)
  
  color_scale = c(TWFE = "#374E55", `Gardner (2021)` = "#DF8F44", 
                  `Callaway and Sant'Anna (2021)` = "#00A1D5", `Sun and Abraham (2021)` = "#B24745",
                  `Roth and Sant'Anna (2021)` = "#79AF97", `Borusyak, Jaravel, Spiess (2021)` = "#6A6599",
                  `Score at 1000 Meters`  = "#374E55" , `Score at 1500 Meters` = "#DF8F44" ,`Score at 2000 Meters`  = "#00A1D5",
                  `Score at 2500 Meters`= "#B24745" , `Score at 3000 Meters` = "#79AF97",`Score at 3500 Meters` = "#6A6599",
                  `Score at 4000 Meters` = '#ED8975' , `Score at 4500 Meters` = '#EAAC8B' , 
                  `Private schools` = "#374E55", `Public schools` = "#DF8F44", 
                  `All sample schools` = "#00A1D5"   )
  
  color_scale = color_scale[names(color_scale) %in% estimators]
  out$ci_lower = out$Estimate - 1.96 * out$Std..Error
  out$ci_upper = out$Estimate + 1.96 * out$Std..Error
  position <- ifelse(seperate, "identity", ggplot2::position_dodge(width = 0.2))
  
  if (!is.null(horizon)) {
    out = out[out$period >= horizon[1] & out$period <= horizon[2],  ] 
  }
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(min(out$period) - 0.5, max(out$period) + 0.5)
  
  x_min <- min(out[out$ref_p == T, "period"], na.rm = TRUE)
  x_max <- max(out[out$ref_p == T, "period"], na.rm = TRUE)
  
  # Define shadow_rect with scalar xmin and xmax
  shadow_rect <- geom_rect(
    aes(xmin = as.numeric(x_min), xmax = as.numeric(x_max), ymin = -Inf, ymax = Inf, fill = "red4", alpha = 0.1),
    alpha = 0.1,
    inherit.aes = FALSE,  # don't inherit aesthetics from other layers
    show.legend = FALSE   # hide rectangle from legend
  )
 
 
  
  Plot= ggplot2::ggplot(data = out, ggplot2::aes(x = .data$period, 
                                                 y = .data$Estimate, 
                                                 color = .data$estimator,
                                                 ymin = .data$ci_lower , 
                                                 ymax = .data$ci_upper)) + 
                                              # {
                                              #      if (seperate) 
                                              #        ggplot2::facet_wrap(~estimator, scales = "free")
                                              #    }  +  
    theme_light()  +
    shadow_rect +
    ggplot2::geom_point(position = position, size = 1.2) + 
    # shadow_rect + 
    ggplot2::geom_errorbar(position = position, width  = 0.15) + 
    ggplot2::geom_vline(xintercept = x_min, linetype = "dashed") + 
    ggplot2::geom_vline(xintercept =x_max, linetype = "dashed") + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")  +
   
    ggplot2::ggtitle(TITULO)+ 
    ggplot2::scale_x_continuous(limits = x_lims, breaks = c(  (min(out$period)  ) :  (max(out$period)   ) ) ) +
    # ggplot2::geom_tile(colour="white" ) +
    
    ggplot2::theme(  plot.title = element_text(hjust = 0.5, vjust = 0.5),
                     axis.line.x = element_line(color="steelblue4", size = 0.5),
                     axis.line.y = element_line(color="steelblue4", size = 0.5) ) +
    
    ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", 
                  x = "Event Time", color = "Estimator") + {
                    if (seperate) 
                      ggplot2::scale_y_continuous(limits = y_lims)
                  } + {
                    if (seperate) 
                      ggplot2::scale_x_continuous(limits = x_lims, breaks =  c(  (min(out$period)  ) :  (max(out$period)   ) ) )
                  }  + ggplot2::scale_color_manual(values = color_scale)  
  P =  Plot +
    theme_w_did( )     + mynamestheme   
  print(P  )
}
SA_Result_table = function(model){
  sa_result = data.frame(model[["coeftable"]])
  sa_result$period = as.numeric( gsub("year::", "", row.names(sa_result) ))
  row.names(sa_result)= NULL
  sa_result$ref_p = 1
  sa_result = rbind(sa_result,
                    data.frame(
                      "Estimate" =0,  "Std..Error" =0, "t.value" =0,    "Pr...t.." =0,
                      "period" = min(sa_result$period,na.rm = T):max(sa_result$period,na.rm = T) , 'ref_p' = 1
                      
                    )
  )
  sa_result = arrange(sa_result, period)
  
  sa_result <- sa_result %>%
    group_by(period) %>%
    summarise(Estimate = sum(Estimate),
              Std..Error = sum(Std..Error),
              t.value = sum(t.value),
              Pr...t.. =sum(Pr...t..) , 
              ref_p = sum(ref_p)
    )
  
  sa_result$ref_p = ifelse(sa_result$ref_p==1, T, F)
  sa_result$estimator = "Sun and Abraham (2021)"
  return(sa_result)
}
create_latex_table <- function(result, caption = "Table caption", label = "tab:label") {
  # Calculate average of the first column
  average <- mean(result[[1]], na.rm = TRUE) # Access column by name
  
  # Format each row as a LaTeX table row
  latex_rows <- apply(result, 1, function(row) {
    paste(row[c(1, 2)], collapse = " & ")  # Select specific columns
  })
  
  # Combine rows into a complete LaTeX table format
  latex_table <- paste("\\begin{table}[ht]", 
                       "\\begin{tabular}{cc}
 ", # Adjust column alignment as needed
                       "\\hline
 ",
                       "Delta Time & Road Name \\\\ \\hline
 ",
                       paste(latex_rows, collapse = "\\\\
       "),  # Combine rows with line breaks
                       "\\hline
 ",
                       sprintf("Average & %.2f \\\\ \\hline
         ", average),
                       "\\end{tabular}
 ",
                       sprintf("\\caption{%s}
         ", caption), # Add your caption
                       sprintf("\\label{%s}
         ", label),
                       "\\end{table}")
  
  return(latex_table)
}
plot_histogram_output <- function(data, output, heterogenity, plot_title) {
  data$output = data[[output]]
  data$heterogenity = data[[heterogenity]]
  data = data[!is.na(data[[heterogenity]]), ]
  
  # Calculate mean of output variable for each level of heterogenity
  mean_data <- aggregate(output ~ heterogenity, data = data, FUN = mean, na.rm = TRUE)
  
  # Plotting
  plot <- ggplot(data, aes(x = output)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
    geom_density(fill = "red", alpha = 0.5) +
    facet_wrap(~ heterogenity, scales = "free") +
    labs(title = plot_title,
         x = "Average SD",
         y = "Density") +
    theme_minimal() +
    # Add vertical dashed lines for each mean value
    geom_vline(data = mean_data, aes(xintercept = output, color = heterogenity),
               linetype = "dashed", size = 0.5) +
    # Add text annotations for each mean value
    geom_text(data = mean_data, aes(x = output, label = paste("Mean:", round(output, 2)),
                                    y = -Inf, color = heterogenity),
              vjust = -1, hjust = 0.5, size = 5, color = "green")
  
  print(plot)
}
acumulative_treatment = function(data = data.frame(), year_treated='', ids = '', reference_title_date = '10% of the Concession Progress'){
  data$year_treated__ = data[[year_treated]]
  data$ids__ = data[[ids]]
  colegios_tratados_por_ano <- data %>%
    filter(!is.na(year_treated__)) %>%  # Filtrar para eliminar las filas donde no hay tratamiento
    group_by(year_treated__) %>%  # Agrupar por año de tratamiento
    summarise(num_colegios = n_distinct(ids__))  # Contar colegios únicos
  
  colegios_tratados_por_ano = rbind(colegios_tratados_por_ano,
                                    data.frame("year_treated__" = min(colegios_tratados_por_ano$year_treated__,na.rm = T):max(colegios_tratados_por_ano$year_treated__,na.rm = T) ,
                                               "num_colegios" =0
                                    )
  )
  
  colegios_tratados_por_ano <- colegios_tratados_por_ano %>% 
    group_by(year_treated__) %>%  # Agrupar por año de tratamiento
    summarise(num_colegios = sum(num_colegios))  # Contar colegios únicos
  
  
  colegios_tratados_por_ano <- colegios_tratados_por_ano %>%
    arrange(year_treated__) %>%  # Ordenar por año de tratamiento
    mutate(cumulative_colegios = cumsum(num_colegios))  # Calcular la suma acumulada
  
  
  colegios_tratados_por_ano$Treated_rate = colegios_tratados_por_ano$cumulative_colegios/max(colegios_tratados_por_ano$cumulative_colegios, na.rm = T)
  minimo = min(colegios_tratados_por_ano$year_treated__)
  maximo = max(colegios_tratados_por_ano$year_treated__)
  breaks_ = (minimo: maximo)
  
  plot = ggplot(colegios_tratados_por_ano, aes(x = year_treated__)) +
    geom_bar(aes(y = 1), stat = "identity", fill = "white", color = "gray", width = 0.8, alpha = 0.8) +
    geom_bar(aes(y = Treated_rate, fill = "Treated"), stat = "identity", color = "gray", width = 0.8) +
    labs(title = paste0( "Cumulative Treatment Progress of Schools by Year\n (Reference Date: ", reference_title_date,")" ),
         x = "Year of Treatment",
         y = "Cumulative Treated Rate",
         fill = "Legend") +
    scale_fill_manual(name = "Legend", values = c("white" = "white", "Treated" = "lightblue"),
                      labels = c("Treated", "Treated")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +  
    scale_x_continuous(breaks = breaks_) + 
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey", size = 0.2),
      panel.grid.minor = element_blank()
    )
  print(plot)
  return(c(plot, colegios_tratados_por_ano) )
}

balancing_data <- function(data=data.frame(), id = 'string: colname id',
                           time_references = 'string: colname period observation'
){
 
  
  data$Id_ = data[[id]]
  data$Id_ = as.character(data$Id_) 
  data = subset(data, is.na( data$Id_ )==F )
  data$time_references__ = data[[time_references]]
  data = subset(data, is.na( data$time_references__ )==F )
 
  print(paste0("The number of unique IDs: " , length(unique(data$Id_))))
  print(paste0("The number of unique periods of references are: " , length(unique(data$time_references__))))
  print(paste0("The number periods of references are: " , (paste0(sort(unique(data$time_references__)), collapse = ' ' ))))
  table((data$time_references__ ))
  complete_ids <- data %>%
    group_by(Id_) %>%
    summarise(n_years = n_distinct(time_references__)) %>%
    filter(n_years == length(unique(data$time_references__))) %>%
    pull(Id_)
    print(length(complete_ids))
  filtered_data <- data %>%
    filter(Id_ %in% complete_ids)
  all_combinations <- expand.grid(
    Id_ = unique(filtered_data$Id_),
    time_references__ = unique(filtered_data$time_references__) )
  
  data <- all_combinations %>%
    left_join(filtered_data, by = c("Id_", "time_references__"))
  
 data =  data[, 1: (length(colnames(data)) - 2) ]
  return(data)
  
} 
plot_cohort_means <- function(data, year_treated) {
 
  # Define the custom colors
  custom_colors <- c(
    "1994" = "cyan",
    "2002" = "magenta",
    "2006" = "yellow",
    "2007" = "brown",
    "2010" = "pink",
    "2011" = "red", 
    "2012" = "blue", 
    "2014" = "green", 
    "2015" = "navy",
    "2016" = "purple", 
    "2017" = "orange", 
    "2018" = "teal",
    "2019" = "darkgreen", 
    "2020" = "black", 
    "2021" = "gray" 
  )
  custom_linetypes <- c(
    "1994" = "solid",
    "2002" = "solid",
    "2006" = "solid",
    "2007" = "solid",
    "2010" = "solid",
    "2011" = "solid", 
    "2012" = "solid", 
    "2014" = "solid", 
    "2015" = "solid",
    "2016" = "solid", 
    "2017" = "solid", 
    "2018" = "solid",
    "2019" = "solid", 
    "2020" = "dashed", 
    "2021" = "dashed" 
  ) 
  
  # Filter and summarize the data
  cohort_means <- data %>%
    filter(!is.na(!!sym(year_treated))) %>% 
    group_by(!!sym(year_treated), year) %>%
    summarize(sd_math_i = mean(sd_math_i, na.rm = TRUE), .groups = 'drop')
  
  # Plot the means
  ggplot(data = cohort_means, aes(x = year, y = sd_math_i, colour = factor(!!sym(year_treated)), linetype = factor(!!sym(year_treated)))) + 
    geom_line(size = 0.5) + 
    # scale_color_manual(values = custom_colors) + 
    scale_linetype_manual(values = custom_linetypes) +
    labs(x = "Year", y = "Outcome", color = "Cohort (Following Sun et al)", linetype = "Cohort (Following Sun et al)") + 
    theme_bw(base_size = 16)
}

# Example usage
# 

pvaluebuffer <- function(model_list, titulo,subject, reference_time){
  library(ggplot2)
  library("scales")
  mynamestheme <- theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = (12))
  )
  summaries_ =  data.frame()
  
  for (i  in 1:length(model_list)) {
    suma_ = summary(model_list[[i]])
    pvalue = suma_[["coeftable"]][,4] 
    
    buf  = str_split( str_split(names(model_list)[i],
                                pattern = 'at ')[[1]][2],
                      pattern = ' Meters')[[1]][1]
    
    periodo = row.names(summary(model_list[[i]])[["coeftable"]])
    temp = data.frame('pvalue' =pvalue ,
                      'DISTANCE' = as.numeric(paste0( buf ))       )
    #temp$pvalue = pvalue 
    temp$Time.to.treatment = as.numeric(gsub(
      gsub( gsub(periodo, replacement = '', 
                 pattern = 'time_to_treat::'),
            replacement = '', pattern = ':treat' ),
      replacement = '', pattern = 'year::' ) )
    
    row.names(temp) <- NULL
    summaries_ = rbind(temp, summaries_)
    
  }
  summaries_ = summaries_ %>% subset(summaries_$Time.to.treatment >=reference_time)
  summaries_$pvalue = ifelse(summaries_$pvalue >= 0.1, 0.11, summaries_$pvalue)
  
  
  colours = c("darkred", "red", "pink",
              "lightpink", "lightgray" )
  values=rescale(c(0.0001, 0.01, 0.025 
                   ,0.05, 0.11))
  
  Plot = ggplot(summaries_, aes(x = reorder( DISTANCE , DISTANCE),
                                y = reorder(Time.to.treatment ,Time.to.treatment) , fill = pvalue)) +
    # scale_y_continuous(limits = c(0, 8)) + 
    ggtitle(paste0("Road impact on ", subject, " score by distance\n (Treatment time = Year of ", titulo, ")" ))+  
    theme(  plot.title = element_text(hjust = 0.5, vjust = 0.5),
            axis.line.x = element_line(color="steelblue4", size = 0.5),
            axis.line.y = element_line(color="steelblue4", size = 0.5))+
    scale_fill_gradientn(values=values, colours= colours,
                         guide="colorbar"
    ) +
    geom_tile(colour="white", size=0.25)  +
    xlab('DISTANCE FROM ROAD TO SCHOOL (Meters)') + ylab('Time to treatment (Post intervention)') +
    #remove extra space
    scale_y_discrete(expand=c(0, 0)) +
    #remove extra space
    
    scale_x_discrete(expand=c(0 , 0)) #  + 
  #expand_limits(y=c(min(summaries_$Time.to.treatment), max(summaries_$Time.to.treatment)+1) )
  print(Plot  + mynamestheme  )
  
}

###################################

coeficients_se <- function (model_list){
  summaries_ =  data.frame()
  for (i  in 1:length(model_list)) {
    suma_ = summary(model_list[[i]])
    pvalue = suma_[["coeftable"]][,4] 
    
    buf  = str_split( str_split(names(model_list)[i],
                                pattern = 'at ')[[1]][2],
                      pattern = ' km')[[1]][1]
    
    periodo = row.names(summary(model_list[[i]])[["coeftable"]])
    temp = data.frame('coefficients_se' =
                        paste( suma_$coefficients , ' ['  , suma_$se , ']'  
                               #, significancia_pvalue (pvalue) , ''
                        )   ,
                      'buffer_km' = paste0('buffer_', buf)       )
    #temp$pvalue = pvalue 
    temp$period = as.numeric(gsub( gsub(periodo, replacement = '', 
                                        pattern = 'time_to_treat::'),
                                   replacement = '', pattern = ':treat' ) )
    
    #row.names(temp) <- NULL
    summaries_ = rbind(temp, summaries_)
    
  }
}

###################################

safe_results_did <- function (Result, academic_subject ,distancia,interaction ="TWFE"){
  pch_ = c(16,17 ,15, 1,2, 0,20)
  
  for (i in 1:length(Result) ) {
    print(Result[i] )
    for_graph =  Result[i] 
    print(for_graph)
    lista_modelo = get(Result[[i]])
    print("logro hasta aca")
    if (str_split((for_graph),"_")[[1]][length(str_split((for_graph),"_")[[1]])] == '10p') {
      titulo = paste0(academic_subject, " score  at year of\n 10 % advance of construction")
    }else if (str_split((for_graph),"_")[[1]][length(str_split((for_graph),"_")[[1]])] == 'ai') {
      titulo = paste0(academic_subject, " score at year of\n Work Initiation Act")
    }else if (str_split((for_graph),"_")[[1]][length(str_split((for_graph),"_")[[1]])] == 'ent') {
      titulo =paste0(academic_subject, " score at year which\n road construction has finished")
    }else if (str_split((for_graph),"_")[[1]][length(str_split((for_graph),"_")[[1]])] == 'ic') {
      titulo = paste0(academic_subject, " score at year which\n the award of the contract")
    }else {warning("")}
    pch_ = c(16,17 ,15, 1,2, 0,20)
    png(paste0("graph/", (for_graph),distancia,".png"),  width = 1030, height = 598)
    if(distancia =='_short'){
      iplot(lista_modelo , sep = 0.05, ref.line = reference_time,
            xlab = 'Time to treatment',
            main = paste0(interaction ,': ',  titulo), 
            ylim.add = c(0, 0.005)  )
      
      legend("topleft",bg="transparent", 
             col = 1:length(names(lista_modelo)),bty = "n",
             pch = pch_ ,
             legend = names(lista_modelo)  )
    }else {
      iplot(lista_modelo[1:length(names(lista_modelo))-1], sep = 0.05, ref.line = reference_time,
            xlab = 'Time to treatment',
            main = paste0(interaction,': ', titulo), 
            ylim.add = c(0, 0.005)  )
      
      legend("topleft",bg="transparent", 
             col = 1:length(names(lista_modelo)),bty = "n",
             pch = pch_[1:length(names(lista_modelo))-1],
             legend = names(lista_modelo)[1:length(names(lista_modelo))-1]
      )
    }
    dev.off() 
    
    
   
  } 
  
}

###################################
 
  
drop_character_graph_tab = function(tabla){
  tabla[] <- lapply(tabla, gsub, pattern='time_to_treat::', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern=':treat', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='year::', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='treat x time_to_treat =', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='year = ', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='[*]', replacement='')
  row.names(tabla) <- NULL
  
  return(tabla)
}

###################################

drop_character = function(tabla){
  as = row.names(tabla)
  lista = c()
  for (i in 1:length(as)) {
    
    lista <- append(lista, gsub( gsub( gsub(gsub(
      gsub( gsub(as[i], replacement = '', 
                 pattern = 'time_to_treat::'),
            replacement = '', pattern = ':treat' ),
      replacement = '', pattern = 'year::' ),
      replacement = '', pattern = 'treat x time_to_treat =' ),
      replacement = '', pattern = 'year = ' ),
      replacement = '', pattern = ' ' ))
    
  }
  return(lista)
}


table_result_DiD = function(Buffer, TWFE_result, SA_result){
  a = data.frame(etable(TWFE_result) )
  a1 = data.frame(etable(SA_result) )
  colnames(a)[1] = paste0('TWFE_Math_Score_', Buffer)
  colnames(a1)[1] = paste0('Sun_And_Abraham')
  columna1 = colnames(a)[1]
  columna2 = colnames(a1)[1]
  Buffer_= Buffer #'1000M'
  if(length( row.names(a)) >= length( row.names(a1)) ) {
    lista = drop_character(a)
    lista2 = drop_character(a1)
    a1$Periodo  = lista2

    tabla = data.frame('Periodo' = lista, 'TWFE' = a[[1]] )
    tabla = tabla %>% left_join(a1, by = "Periodo")
  }else{
    lista = drop_character(a1)
    lista2 = drop_character(a)
    a$Periodo  = lista2
    tabla = data.frame('Periodo' = lista, 'Sun_And_Abraham' = a1[[1]]  )
    tabla = tabla %>% left_join(a, by = "Periodo")
    
  }
  tabla$buffer = Buffer_
  tabla[nrow(tabla)-4,3]= ifelse(is.na(tabla[nrow(tabla)-4,3]) ,  '__________________',tabla[nrow(tabla)-4,3] )
  return(tabla)
}

 
 

###############################
##### ATT Model and Result ###

Callaway_model = function(buffer,  tabla, anticipation,  yname, idname ="id_name"  ){
    ### Modeling
    ## Callaway, Brantly and Pedro H.C. Sant'Anna.
    
    att_gt_ = att_gt(yname = yname,
                     gname = "year_treated_att",
                     idname = idname,
                     tname = "year",anticipation = anticipation,
                     data = subset(tabla, tabla$buffer_km == buffer),
                     allow_unbalanced_panel= T, panel = F, 
                     clustervars = idname
    )
    modelo_att <- aggte( ### getting the att stimation for a DiD
      att_gt(yname = yname,
             gname = "year_treated_att",
             idname = "id_name",
             tname = "year",anticipation = anticipation,
             data = subset(tabla, tabla$buffer_km == buffer),
             allow_unbalanced_panel= T, panel = F, 
             clustervars = "id_name"
      ),
      ### getting the att stimation for a DiD Dynamic
      type = "dynamic",
      na.rm = TRUE)
    
    
    print(ggdid(modelo_att))
    return(modelo_att)
}



table_result_ATT = function(buffer,  tabla, anticipation,  yname ){
  ### Modeling
  ## Callaway, Brantly and Pedro H.C. Sant'Anna.
  tabla$idname =  as.numeric(tabla$cole_cod_dane)
  
  modelo_att <- aggte( ### getting the att stimation for a DiD
                      att_gt(yname = yname,
                             gname = "year_treated_att",
                             idname = "idname",
                             tname = "year",anticipation = anticipation,
                             data = subset(tabla, tabla$buffer_km == buffer),
                             allow_unbalanced_panel= T, panel = F
                             ),
                      ### getting the att stimation for a DiD Dynamic
                      type = "dynamic",
                      na.rm = TRUE)
  #did::gplot(modelo_att)  
  ### Tabuling
  #modelo_att =es
  periodos=  modelo_att[[4]]
  subject = modelo_att[["DIDparams"]][["yname"]]
  
  coeficiente = ifelse(as.numeric(modelo_att[[5]])<0, as.numeric(modelo_att[[5]])*-1,as.numeric(modelo_att[[5]]))
  st_error = as.numeric(modelo_att[[6]])
  
  simbolo = c() 
  for (i in 1:length(coeficiente)) {
  simbolo = append(simbolo, significancia_un_valor(coeficiente[i], st_error[i] )  )    
  }
  att = paste0( as.character(round( as.numeric(modelo_att[[5]]), 4)  ) , simbolo, " (",
                as.character(round( as.numeric(modelo_att[[6]]), 4)  ) , ") ")
  

  
  t_statistic = round(coeficiente, 4) / round( as.numeric(modelo_att[[6]]), 4)
  
  head_ = data.frame("Periodo" = c("DependentVar.:", "" ) ,
                     'Callaway'  = c(subject, "" ),
                     'pvalue' = '')
  
  body_ = data.frame("Periodo" = as.character( periodos ) ,
                     'Callaway'  =  att,
                     'pvalue' = as.character( pt(q= t_statistic, df = 2, lower.tail=FALSE) )
                     )
  tail_ = data.frame("Periodo" = c("Fixed-Effects:", "cole_cod_dane" , "year"
                                   ,"__________________", "S.E.:Clustered",
                                   "Observations","R2", "WithinR2"  ) , 
                     'Callaway'  = c("------------------", "No", "No",
                                 "__________________" ,
                                 "by: cole_cod_dane", 
                                 as.character(modelo_att[[13]][[21]]),
                                 "", "") ,
                     'pvalue' = '')
  all_body  = rbind(head_, body_)
  all_body  = rbind(all_body, tail_)
  
  
  #all_body = significancia(Tabla = all_body)
  #all_body$ATT_  = paste0(all_body$ATT_ , all_body$significancia)
  all_body = all_body[,c(1,2)]
  return(all_body)
}



 
 
#############################################################
############# Asteriscos en la significancia estadistica ###
significancia = function(Tabla){
  Tabla$significancia = ''
  for (j in 1:nrow(Tabla)) {
    i =  Tabla[j,3]
    elemento = ifelse(is.na(as.numeric(i)), 1, as.numeric(i))
    if(elemento <= 0.05){
      Tabla[j,'significancia'] =  '***'
    }else if( elemento > 0.05 &  elemento <=0.06){
      Tabla[j,'significancia']  =  '**'
    }else if( elemento > 0.06 &  elemento <=0.085){
      Tabla[j,'significancia']  =  '*'
    }else if( elemento > 0.085 &  elemento <=0.1){
      Tabla[j,'significancia']  =  '.'
    }else {Tabla[j,'significancia']  =  '' }
    
  }
  return(Tabla)
  
}

 
significancia_un_valor = function(coeficiente, standar_error){
  coeficiente = ifelse(coeficiente<0, coeficiente*-1,coeficiente)
  t_statistic =   round( coeficiente  / standar_error , 4)
  
  pvalue = ( pt(q= t_statistic, df = 2, lower.tail=FALSE) )
   
    elemento = pvalue
    if(elemento <= 0.05){
      sig =  '***'
    }else if( elemento > 0.05 &  elemento <=0.06){
      sig  =  '**'
    }else if( elemento > 0.06 &  elemento <=0.085){
      sig  =  '*'
    }else if( elemento > 0.085 &  elemento <=0.1){
      sig =  '.'
    }else {sig  =  '' }
  return(sig)
    
  }
 
 
################################
table_result_TWFE_SA_C = function(Buffer, TWFE_result, SA_result, ATT_result){
  tabla  = left_join(table_result_DiD(Buffer =  Buffer, 
                             TWFE_result = TWFE_result,
                             SA_result = SA_result ), 
            ATT_result,
            by = "Periodo"
           )
  tabla = tabla[, c(1,2,3,5,4)]
  tabla[] <- lapply(tabla, gsub, pattern='[*]', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='[(]', replacement=' ')
  tabla[] <- lapply(tabla, gsub, pattern='[)]', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='. ', replacement='0')
  tabla[nrow(tabla)-4,4]= ifelse(is.na(tabla[nrow(tabla)-4,4]) ,  '__________________',tabla[nrow(tabla)-4,4] )
  tabla[nrow(tabla)-4,5]= ifelse(is.na(tabla[nrow(tabla)-4,5]) ,  '__________________',tabla[nrow(tabla)-4,5] )
  tabla[nrow(tabla)-7,5]= ifelse(is.na(tabla[nrow(tabla)-7,5]) ,  '------------------',tabla[nrow(tabla)-7,5] )
  
  return(tabla)
}
 
#############################################################
#########################################

theme_w_did <- function (base_size = 16, base_family = "", base_line_size = base_size/22, 
          base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_grey(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", 
                                          colour = NA), panel.border = element_rect(fill = NA, 
                                                                                    colour = "white", size = rel(1)), panel.grid = element_line(colour = "grey87"), 
          panel.grid.major = element_line(size = rel(0.5)), 
          panel.grid.minor = element_line(size = rel(0.25)), 
          axis.ticks = element_line(colour = "grey70", size = rel(0.5)), 
          legend.key = element_rect(fill = "white", colour = NA), 
          strip.background = element_rect(fill = "white", 
                                          colour = NA), strip.text = element_text(colour = "steelblue4", 
                                                                                  size = rel(0.8), margin = margin(0.8 * half_line, 
                                                                                                                   0.8 * half_line, 0.8 * half_line, 0.8 * half_line)), 
          complete = TRUE)
}


#########################################

event_study_plot = function (out, seperate = TRUE, horizon = NULL, TITULO= '') {
  library(ggplot2)
  library("scales")
  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = (12)),
    legend.position = "bottom"
    # ,panel.background = element_rect(fill = "white", colour = NA),
    # panel.border     = element_rect(fill = NA, colour = "grey70" ), #, linewidth = 0.5 
    # panel.grid = element_line(color = "#808080", size = 0.05 ),
    # panel.grid.major = element_line(linewidth = rel(0.05) ),
    # panel.grid.minor = element_line(linewidth = rel(0.05) )
                                  )
  estimators = unique(out$estimator)
  levels = c("TWFE", "Borusyak, Jaravel, Spiess (2021)", "Callaway and Sant'Anna (2020)", 
             "Gardner (2021)", "Roth and Sant'Anna (2021)", "Sun and Abraham (2020)",
             "Score at 1000 Meters" ,"Score at 1500 Meters" ,"Score at 2000 Meters",
             "Score at 2500 Meters" ,"Score at 3000 Meters" ,"Score at 3500 Meters",
             "Score at 4000 Meters" ,"Score at 4500 Meters", 'Private schools' , 
             'Public schools' ,  'All sample schools' 
             )
  
  levels = levels[levels %in% estimators]
  out$estimator = factor(out$estimator, levels = levels)
  
  color_scale = c(TWFE = "#374E55", `Gardner (2021)` = "#DF8F44", 
                  `Callaway and Sant'Anna (2020)` = "#00A1D5", `Sun and Abraham (2020)` = "#B24745",
                  `Roth and Sant'Anna (2021)` = "#79AF97", `Borusyak, Jaravel, Spiess (2021)` = "#6A6599",
                  `Score at 1000 Meters`  = "#374E55" , `Score at 1500 Meters` = "#DF8F44" ,`Score at 2000 Meters`  = "#00A1D5",
                  `Score at 2500 Meters`= "#B24745" , `Score at 3000 Meters` = "#79AF97",`Score at 3500 Meters` = "#6A6599",
                  `Score at 4000 Meters` = '#ED8975' , `Score at 4500 Meters` = '#EAAC8B' , 
                  `Private schools` = "#374E55", `Public schools` = "#DF8F44", 
                  `All sample schools` = "#00A1D5"   )
  
  color_scale = color_scale[names(color_scale) %in% estimators]
  out$ci_lower = out$estimate - 1.96 * out$std.error
  out$ci_upper = out$estimate + 1.96 * out$std.error
  if (seperate) 
    position = "identity"
  else position = ggplot2::position_dodge(width = 0.5)
  if (!is.null(horizon)) {
    out = out[out$term >= horizon[1] & out$term <= horizon[2], 
    ] 
    }
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(min(out$term) - 0.5, max(out$term) + 0.5)
  Plot = ggplot2::ggplot(data = out, ggplot2::aes(x = .data$term, 
                                                  y = .data$estimate, color = .data$estimator,
                                                  ymin = .data$ci_lower , 
                                                  ymax = .data$ci_upper)) + {
                                                    if (seperate) 
                                                      ggplot2::facet_wrap(~estimator, scales = "free")
                                                  } +  theme_light()  +
    ggplot2::geom_point(position = position, size = 1.8) + 
    ggplot2::geom_errorbar(position = position) + 
    ggplot2::geom_vline(xintercept = -1, linetype = "dashed") + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::ggtitle(TITULO)+ ggplot2::scale_x_continuous(limits = x_lims, breaks = c(  (min(out$term)  ) :  (max(out$term)   ) ) ) +
    # ggplot2::geom_tile(colour="white" ) +
    ggplot2::theme(  plot.title = element_text(hjust = 0.5, vjust = 0.5),
                     axis.line.x = element_line(color="steelblue4", size = 0.5),
                     axis.line.y = element_line(color="steelblue4", size = 0.5)) +
    
    ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", 
                  x = "Event Time", color = "Estimator") + {
                    if (seperate) 
                      ggplot2::scale_y_continuous(limits = y_lims)
                  } + {
                    if (seperate) 
                      ggplot2::scale_x_continuous(limits = x_lims, breaks =  c(  (min(out$term)  ) :  (max(out$term)   ) ) )
                  }  + ggplot2::scale_color_manual(values = color_scale) 
  P =  Plot +
    theme_w_did( )     + mynamestheme  
  print(P  )
}



########################################################
TWFE_table = function(estimator = 'TWFE', MODELO ) {
  
  a = summary(MODELO)
  a = data.frame(
    'estimate' = a[["coefficients"]],
    'std.error' = a[["se"]]
  ) 
  a$term = rownames(a)
  
  a = drop_character_graph_tab(a)
  a$term = gsub(pattern =  '_', replacement = '' , a$term)
  
  a$estimator = estimator
  a$term = as.numeric(a$term)
  a$estimate = as.numeric(a$estimate)
  a$std.error = as.numeric(a$std.error)
  a = a[,c('estimator','term','estimate','std.error')]
  row.names(a) <- NULL
  return(a)
}
 
Callaway_table = function(buffer,  tabla, anticipation,  yname ){
  tabla1 = tabla
  
  att_gt_ = att_gt(yname = yname,
         gname = "year_treated_att",
         idname = "id_name",
         tname = "year",anticipation = anticipation,
         data = subset(tabla1, tabla1$buffer_km == buffer),
         allow_unbalanced_panel= T, panel = F, 
         clustervars =  "id_name"  ,   cband = F )
  
  modelo_att <- aggte( ### getting the att stimation for a DiD
    att_gt_  , ### getting the att stimation for a DiD Dynamic
    type = "dynamic",
    na.rm = TRUE, alp = 0.05 , bstrap=T )
  
  periodos=  modelo_att[[4]]
  coeficiente = modelo_att[[5]]
  st_error = as.numeric(modelo_att[[6]])
  body_ = data.frame("estimator" = "Callaway and Sant'Anna (2020)" , 
                     "term" = as.numeric( periodos ) ,
                     'estimate'=coeficiente , 
                     'std.error'  =  st_error
  ) 
  body_ = subset(body_, body_$term <= 8 )
  body_ = subset(body_, body_$term >= -10 )
  
  return(body_)
}

SA_table = function( MODELO ) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  
  estimator = 'Sun and Abraham (2020)'
  a = etable(MODELO)
  a$term = rownames(a)
  
  a = a[  (3:(nrow(a)-8)) , ]
  row.names(a) <- NULL
  a = drop_character_graph_tab(a)
  
  a = cbind(a , data.frame(str_split_fixed(a[[1]], " ", 2))) 
  
  a[[3]]   = gsub("\\.*$","", a[[3]]   )
  
  #a[[3]] =   ifelse(substr(a[[3]], 1,1) == '-', substr(a[[3]], 1,7), substr(a[[3]], 1,6) )
  a = a[,c(2,3,4)]
  colnames(a) = c('term', 'estimate', 'std.error')
  a$std.error = as.numeric( gsub(a$std.error, pattern = '[()]', replacement = '') )  
  a$estimator = estimator
  a$term = as.numeric(a$term)
  a$estimate = as.numeric(a$estimate)
  a$std.error = as.numeric(a$std.error)
  a = a[,c('estimator','term','estimate','std.error')]
  a = subset(a, a$term <= 10 )
  a = subset(a, a$term >= -8 )
  return(a)
}

results_by_buffer <- function(lista_modelo){
  lista_modelo_table = data.frame()
  for (i in 1:length(lista_modelo) ) {
    temp  = SA_table(  lista_modelo[[i]]  )
    temp$estimator = names(lista_modelo)[i]
    lista_modelo_table= rbind(temp,lista_modelo_table)
  }
  return(lista_modelo_table[order(lista_modelo_table[[1]]),] )
}
 



Gardner_table   = function( yname, df ) {
  df = df %>% subset(df$time_to_treat >= -8)
  MODELO =data.frame()
  
  for (i in 1:length(unique( df$buffer_km))) {
    print(i)
    temo = did2s::event_study( estimator = c(  "did2s"),
                                 data = subset(df, df$buffer_km == kilometros[i] ),
                                 yname  = yname,
                                 idname = "id_name",
                                 gname = 'year_treated',
                                 tname = 'year' )
    temo$buffer_km = kilometros[i]
    MODELO = rbind(temo,MODELO )
    
  }
  MODELO = subset(MODELO, MODELO$term <= 8 )
  MODELO = subset(MODELO, MODELO$term >= -8 )
  return(MODELO)
}

 
ATT_biased  <- function(base, Y, treat, period_var ){
  ### dependent
  var = Y
  base$Y = base[[Y]]
  base = dplyr::select(base, -var)
  ### Treatment
  var = treat
  base$treatment = base[[treat]]
  base = dplyr::select(base, -var)
  #############################
  mini_time = min(base[[period_var]], na.rm = T)
  maxi_time = max(base[[period_var]], na.rm = T)
  
  before = subset(base , base[[period_var]] == mini_time)
  after = subset(base , base[[period_var]] == maxi_time)
  
  before_est =  as.data.frame(summary(lm(data= before, Y~treatment))[["coefficients"]] )
  after_est =  as.data.frame( summary(lm(data= after, Y~treatment))[["coefficients"]] )
  before_est$esti = row.names(before_est)
  before_est$period = 'before'
  row.names(before_est) <- NULL   
  
  after_est$esti = row.names(after_est)
  after_est$period = 'after'
  row.names(after_est) <- NULL   
  after_est = subset(after_est, after_est$esti =='treatment')
  before_est = subset(before_est, before_est$esti =='treatment')
  delta = data.frame(
  'estimate' = as.numeric(after_est[[1]])  -  as.numeric(before_est[[1]]) ,
  'Std. Error' = mean(as.numeric(after_est[[2]]) , as.numeric(before_est[[2]])),
  't value' = mean(as.numeric(after_est[[3]]) , as.numeric(before_est[[3]])),
  'Pr(>|t|)' = mean(as.numeric(after_est[[4]]) , as.numeric(before_est[[4]])),
  'esti' = 'DiD', 'period' = 'ATT'
  )
  colnames(delta)   = colnames(after_est)
  appended_ = rbind(after_est, before_est)
  appended_ = rbind(appended_, delta)
    return( appended_ )
}



Descriptive_statistics <- function(base, heterogenidad, unidades_de_info,tratamiento, lista ) {
  library(tidyr)
  library(maditr )
  
  esta_desc = data.frame()
  esta_desc_heterogenities = data.frame()
  for (j in unique(base[[tratamiento]])  ) {
    
    TEMPORAL =  base %>% subset(base[[tratamiento]] == j)
    treated = j
    
    for (i in lista ) {
      
      temp = data.frame("variable" = i, 
                        "mean"  =  mean( TEMPORAL [[i]], na.rm = T )  ,
                        "median" = median(TEMPORAL[[i]], na.rm = T ) ,
                        "sd" = sd(TEMPORAL[[i]], na.rm = T ),
                        'N_indiviuos' = length(unique(TEMPORAL[[ unidades_de_info ]])),
                        "Observaciones" = length(TEMPORAL[[ unidades_de_info ]]) ,
                        "treat" = treated,
                        "heterogenidad" = 'All sample'
      )
      esta_desc = rbind(temp, esta_desc)
    }
    
  }
  
  esta_desc_heterogenities = data.frame()
  for (j in unique(base[[tratamiento]])  ) {
    TEMPORAL =  base %>% subset(base[[tratamiento]] == j)
    treated = j
    
    for (k in unique(TEMPORAL[[heterogenidad]]) ) {
      TEMPORAL_2=  subset(TEMPORAL, TEMPORAL[[heterogenidad]] == k)   
      
      for (l in lista ) {
        temp2 = data.frame("variable" = l, 
                           "mean"  =  mean( TEMPORAL_2 [[l]], na.rm = T )  ,
                           "median" = median(TEMPORAL_2[[l]], na.rm = T ) ,
                           "sd" = sd(TEMPORAL_2[[l]], na.rm = T ),
                           'N_indiviuos' = length(unique(TEMPORAL_2[[ unidades_de_info ]])),
                           "Observaciones" = length(TEMPORAL_2[[ unidades_de_info ]] ) ,
                           "treat" = treated, 
                           "heterogenidad" = k
        )
        esta_desc_heterogenities = rbind(temp2, esta_desc_heterogenities)
      }
      
    }
  }
  
  estadisticas_descriptivas = rbind(esta_desc, esta_desc_heterogenities)
  return(estadisticas_descriptivas)
}


ATT_SA<- function(tabla, buffer, Y_var, heterogenities =F){
  df = get(tabla)
  if (heterogenities ==F) {
    df = subset(df,df$buffer_km==buffer  )
    df$Y = df[[Y_var]] 
    resumen = data.frame( summary(
      feols( Y ~  sunab(year_treated_sa, year , att = T, bin.rel = "bin::2") #+ ## The only thing that's changed
             | id_name + year,                               ## FEs
             cluster = ~ id_name   ,                        ## Clustered SEs
             data = df )
      , agg = "ATT")[["coeftable"]])
    
    resumen$significancia =significancia_un_valor(coeficiente = resumen[[1]], standar_error= resumen[[2]])
    resumen$agg = row.names(resumen)
    row.names(resumen) <- NULL   
    resumen$Y_var = Y_var
    resumen$table = tabla
    resumen$heterogenities = heterogenities
    colnames(resumen) = c("Estimate"  , "Std_Error", "t_value",    "P_Value" ,'significancia'  , "agg" , "Y_var" ,"table" , 'heterogenities'    )
  }else{
    df = subset(df,df$buffer_km==buffer  )
    hetero = unique(df[[heterogenities ]])
    resumen_hetero = data.frame()
    for (i in hetero) {
      df$Y = df[[Y_var]] 
      resumen = data.frame( summary(
        feols( Y ~  sunab(year_treated_sa, year , att = T, bin.rel = "bin::2") #+ ## The only thing that's changed
               | id_name + year,                               ## FEs
               cluster = ~ id_name   ,                        ## Clustered SEs
               data = subset(df, df[[heterogenities ]] == i )
               )
        , agg = "ATT")[["coeftable"]])
      
      resumen$significancia =significancia_un_valor(coeficiente = resumen[[1]], standar_error= resumen[[2]])
      resumen$agg = row.names(resumen)
      row.names(resumen) <- NULL   
      resumen$Y_var = Y_var
      resumen$table = tabla
      resumen$heterogenities = i
      colnames(resumen) = c("Estimate"  , "Std_Error", "t_value",    "P_Value" ,'significancia'  , "agg" , "Y_var" ,"table" , 'heterogenities'    )
      
      resumen_hetero = rbind(resumen, resumen_hetero)
    }
    
    resumen = resumen_hetero

  }

  
  
  return(resumen)
}

