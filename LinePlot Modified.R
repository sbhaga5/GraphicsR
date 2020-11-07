### linePlot ###
## Data: Database + Manual(AVA returns)
## Base: linePlot() function in `pensionviewr`
# By: Anil, Swaroop, and Jen

rm(list=ls())
###Load/install packages
#R.Version()
#https://github.com/ReasonFoundation/pensionviewr
#Create token -> usethis::edit_r_environ() -> restart -> Sys.getenv("GITHUB_PAT")
#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
#library(janitor)
library(grid)#https://bookdown.org/rdpeng/RProgDA/the-grid-package.html
library(tidyverse)
#library(openxlsx)
library(tseries)
library(plyr)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
#library(shinyFiles)
library(DT)
library(plotly)
library(rlang)
library(purrr)

###################
#Using 2 New Functions to download and filter datat from datatabase
PERSI.data <- pullStateData(2001)
PERSI.data <- filterData(PERSI.data, 2001)

pl <- planList()
#filter for PERSI
PERSI.data<- PERSI.data %>% filter (plan_name == "Idaho Public Employee Retirement System")
#View(PERSI.data)

PERSI.data$year <- as.numeric(PERSI.data$year)
#Set to data.frame for visualization
PERSI.data <- data.frame(PERSI.data)
#View(PERSI.data)

#####GEOMEAN FUNCTION
geomean <- function(x) {
  x <- as.vector(na.omit(x))
  x <- x +1
  exp(mean(log(x)))-1 
}
returns <- as.numeric(PERSI.data$return_1yr)
nyear <- 10
rolling <- geomean(returns[1:nyear])
n <- length(na.omit(returns))-nyear
#Geomean function
for(i in 1:n){
  rolling <- rbind(rolling, geomean(returns[(i+1):(i+nyear)]))
}
rolling <- data.table(rolling)

PERSI.data <- data.table(rbind.fill(rolling, PERSI.data))
PERSI.data[(PERSI.data[!is.na(return_1yr),.N]+1):(PERSI.data[!is.na(return_1yr),.N]+rolling[,.N])]$V1<- PERSI.data[(1:rolling[,.N])]$V1
PERSI.data <- PERSI.data[!(1:rolling[,.N])]
# UAL4 <- data.table(UAL4[, Tr30 := tr30[(n-UAL4[!is.na(Actual_Return),.N]):last]])

###############
#Adding AVA returns (Arkansas ERS example*)
ava_returns <- matrix(0, 19,1)
ava_returns[,1] <- c(-0.064, -0.0736, 0.0332, 0.1763, 4.70, 9.00, 12.40, 8.00, -5.90, 2.00, 3.10, 4.50, 11.40, 13.80, 8.80, 8.20, 7.70, 5.80, 6.50)
#View(ava_returns)
ava_returns <- data.table(ava_returns/100)

PERSI.data <- data.table(PERSI.data)
PERSI.data <- PERSI.data[, ava_return := ava_returns]

PERSI.data$year <- as.numeric(PERSI.data$year)

PERSI.data <- data.frame(PERSI.data)
#View(colnames(PERSI.data))
PERSI.data <- PERSI.data %>% select(year, return_1yr, ava_return, arr, V1)

##Modified linePlot
linePlot <- function(data, yaxisMin = NULL, yaxisMax = NULL, yaxisSeq = 5,
                     yaxisScale = 100, percentageTRUE = TRUE,
                     labelY = NULL, lab1 = NULL, 
                     lab2 = NULL, lab3 = NULL, 
                     lab4 = NULL, lab5 = NULL) {
  
  reasontheme::set_reason_theme(style = "slide")
  
  data <- data.table(data) %>% dplyr::mutate_all(dplyr::funs(as.numeric))
  colnames(data) <- c("year", if(!is_null(lab1)){paste(lab1)},
                      if(!is_null(lab2)){paste(lab2)},
                      if(!is_null(lab3)){paste(lab3)},
                      if(!is_null(lab4)){paste(lab4)},
                      if(!is_null(lab5)){paste(lab5)})
  graph <- data.table(melt(data, id.vars="year"))
  
  
  lineColors <- c(palette_reason$Orange,palette_reason$Yellow, palette_reason$SatBlue, palette_reason$LightGrey) #Updated palette to reason one
  options(repr.plot.width = 1, repr.plot.height = 0.75)
  
  ggplot2::ggplot(graph, ggplot2::aes(x = year, y = yaxisScale * value, group = variable)) +
    ggplot2::geom_line(ggplot2::aes(colour = str_wrap(factor(variable), 20)), size = 1.5) + #Added str_wrap(to cut legend text)
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::scale_colour_manual(values = lineColors) +
    ggplot2::scale_y_continuous(
      breaks = seq(yaxisMin, yaxisMax, by=yaxisSeq), limits = c(yaxisMin, yaxisMax), #added limits and expanded break scale
      labels = function(b) {
        if(percentageTRUE){
          paste0(round(b, 0), "%")
        }else{
          paste0(round(b, 0))
        }  
      },
      expand = c(0, 0)
    ) +
    
    ggplot2::scale_x_continuous(breaks = seq(min(graph$year), max(graph$year), by = 2), #added blank years
                                expand = c(0, 0)
    ) +
    
    labs(x = element_blank(), y = labelY)+
    theme(legend.text=element_text(size=12))+ #Added element to control legend font size
    theme(legend.position= c(0.51, 0.1)) #Moved legend to the bottom
}

#Line Plot -- Inv.Returns
graph <- linePlot(PERSI.data, yaxisMin = -20, yaxisMax = 20, yaxisSeq = 4,
                  yaxisScale = 100, percentageTRUE = TRUE,
                  labelY = "", lab1 = "Market Valued Returns (Actual)", 
                  lab2 = "Actuarially Valued Investment Return (Smoothed by Plan)", 
                  lab3 = "Assumed Rate of Return", 
                  lab4 = "10-Year Geometric Rolling Average", lab5 = NULL)
graph
