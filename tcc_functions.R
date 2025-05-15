library(tidyverse)
library(tsibble)
library(seasonal)
library(rbcb)
library(sidrar)
library(ipeadatar)
library(fredr)
library(vars)
library(stats)
library(bsvarSIGNs)
library(extrafont)
library(ragg)
library(lmtest)
library(fUnitRoots)

fredr::fredr_set_key("f100d8e9c7003e99b6e92f252f390f6b")

seasonal::checkX13()

palette_list <- function(name){
  df <- list("indigo" = c("#4250AF","#5F6BBB","#7C85C6","#A0A7D6", "#C6CAE6"),
             "red" = c("#D3493F","#DE5E56", "#D77976", "#E39E9C", "#F7CFD3"),
             "blue" = c("#4286DE", "#5EA3EE", "#77B3F1", "#9DC8F5", "#C2DDF8"),
             "usual" = c("#6E45A8","#D3493F", "#EA7100", "#34D33C","#4286DE","#000"))
  return(df[[name]])
}

default_line_plot <- function(data,
                              title=NULL,
                              subtitle=NULL,
                              source="Autoria própria",
                              dbreaks="1 year"){
  plot <- ggplot2::ggplot(data=data, aes(x=date, y=value, color=id)) +
    geom_line(linewidth=1) +
    labs(x=NULL,
         y=NULL,
         title=title,
         subtitle=subtitle,
         color=NULL,
         caption=paste0("Fonte: ",source),
    ) +
    scale_y_continuous(labels=scales::number,
                       breaks=scales::pretty_breaks()) +
    scale_x_date(labels=scales::date_format("%b/%Y"),
                 breaks=scales::date_breaks(dbreaks)) +
    scale_color_manual(values=palette_list("usual")) +
    theme_bw(base_size=11, base_family="JetBrains Mono") +
    theme(legend.position="bottom",
          plot.title = element_text(hjust=0.5),
          axis.text.x = element_text(angle=45, hjust=1))
  return(plot)
}

create_seas_col <- function(data,
                            wch.col="value",
                            freq="m"){

  df <- data[c("date",wch.col)] %>% 
  na.omit()
  
  dict1 <- c("w" = 52,
             "m" = 12,
             "q" = 4,
             "y" = 1)
  
  dict2 <- c("w" = week,
             "m" = month,
             "q" = quarter,
             "y" = year)
  
  ts <- df %>%
    dplyr::arrange(date) %>%
    dplyr::select(-date) %>% 
    ts(start = c(year(first(df$date)), dict2[[freq]](first(df$date))),
       end = c(year(last(df$date)), dict2[[freq]](last(df$date))),
       frequency = dict1[[freq]])
  
  adjusted <- seasonal::seas(x=ts)
  
  output <- zoo::fortify.zoo(adjusted$data[,3]) %>% 
    dplyr::mutate(date = as.Date(round((Index - 1970)*365.25,0), origin="1970-01-01"),
                  date = ym(paste0(year(date), sprintf("%02d", month(date))))) %>% 
    dplyr::relocate(date) %>% 
    dplyr::select(-Index) %>% 
    purrr::set_names("date",paste0(wch.col,"_seasonal"))
  
  data <- merge(data, output, by="date", all=T)
  
  return(data)
}

hp_filter <- function(data,
                      freq=c("m","q","y")){
  
  #Filtro Hodrick-Prescott parametrizado conforme Ravn & Uhlig (2002)
  
  start = min(data$date)
  end = max(data$date)
  
  if(freq=="m"){
    lambda = 129600
    data.ts <- data %>% 
      dplyr::select(-date) %>% 
      ts(.,
         start = c(year(start), month(start)),
         end = c(year(end), month(end)),
         frequency = 12)
    
    filter <- mFilter::hpfilter(data.ts, freq=lambda, type="lambda")
    
    result <- tibble(date = seq.Date(from=start, to=end, by="month"),
                     value = filter$cycle)
    
  }
  
  if(freq=="q"){
    lambda = 1600
    data.ts <- data %>% 
      dplyr::select(-date) %>% 
      ts(.,
         start = c(year(start), quarter(start)),
         end = c(year(end), quarter(end)),
         frequency = 4)
    
    filter <- mFilter::hpfilter(data.ts, freq=lambda, type="lambda")
    
    result <- tibble(date = seq.Date(from=start, to=end, by="quarter"),
                     value = filter$cycle)
  }
  
  if(freq=="y"){
    lambda = 6.25
    data.ts <- data %>% 
      dplyr::select(-date) %>% 
      ts(.,
         start = c(year(start)),
         end = c(year(end)),
         frequency = 1)
    
    filter <- mFilter::hpfilter(data.ts, freq=lambda, type="lambda")
    
    result <- tibble(date = seq.Date(from=start, to=end, by="year"),
                     value = filter$cycle)
  }

  return(result)
  
}



font_import()
y
loadfonts(device="win")
