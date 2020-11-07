##Updated debtPlot*(version ANedits)
setwd(getwd())
##############
library(pensionviewr)
# needed before installing the pensionviewr
library(reasontheme)
# these have the data functions that can be uploaded directly from the websource
library(data.table)
# works with other functions like ggplot..
library(tidyverse)
library(ggplot2)

reason_color_pal()
library(extrafont)
font_import(pattern="Roboto")
loadfonts(device = "win", quiet = TRUE)

palette_reason <- data.table(
  Orange = "#FF6633", 
  LightOrange = "#FF9933",
  DarkGrey = "#333333", 
  SpaceGrey = "#A69FA1",
  DarkBlue = "#0066CC",
  GreyBlue = "#6699CC", 
  Yellow = "#FFCC33", 
  LightBlue = "#66B2FF", 
  SatBlue = "#3366CC", 
  Green = "#669900",
  LightGreen = "#00CC66",
  Red = "#CC0000",
  LightRed = "#FF0000")

##Load list of plans
pl <- planList()
set_reason_theme(style = "slide")

#####
urlfile <- "https://raw.githubusercontent.com/ReasonFoundation/GraphicsR/master/Idaho%20Amo%20Data.csv"
NegAmoData <- read_csv(url(urlfile), col_names = FALSE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)
NegAmoData <- NegAmoData[2:nrow(NegAmoData),]
year <- NegAmoData[,1]
NegAmo <- NegAmoData[,2]
NegAmoDef <- NegAmoData[,3]
NegAmoExcess <- NegAmoData[,4]
PlotDataAmo <- data.frame(year,'Contributions',NegAmo)
PlotDataDef <- data.frame(year,'Deficiency',NegAmoDef)
PlotDataExcess <- data.frame(year,'Excess',NegAmoExcess)

colnames(PlotDataAmo) <- c('year','type','value')
colnames(PlotDataDef) <- c('year','type','value')
colnames(PlotDataExcess) <- c('year','type','value')

PlotData <- rbind(PlotDataAmo,PlotDataDef,PlotDataExcess)
names <- data.frame(PlotData[,2])
value <- PlotData[,3]

NegAmoPlot <- function(data, title = NULL, caption = FALSE, grid = FALSE, ticks = TRUE, font) {
  graph <-
    data.frame(NegAmo, NegAmoDef, NegAmoExcess) %>%
    tidyr::drop_na()
  graph <- graph %>%
    dplyr::mutate(sign = dplyr::case_when((NegAmoExcess) ~ "positive",
                                          NegAmoDef ~ "negative"))
  ggplot2::ggplot(graph,
                  ggplot2::aes(x = graph$year)) +
    ggplot2::geom_bar(data,stat = 'count', position = 'stack') +
    ggplot2::labs(y = "Contributions", x = NULL) +
    
    # colors assigned to pos, neg
    ggplot2::scale_fill_manual(
      values = c(NegAmoDef = paste(palette_reason$Red),#Referenced Color Palette
                 NegAmoExcess = paste(palette_reason$Green)),#Referenced Color Palette
      aesthetics = c("colour", "fill")
    ) +
    
    theme_bw()+
    # sets the y-axis scale
    ggplot2::scale_y_continuous(
      # creates 10 break points for left y labels
      breaks = scales::pretty_breaks(n = 10),
      sec.axis = ggplot2::sec_axis(
        ~ . / (y_maximum / 100),
        breaks = scales::pretty_breaks(n = 10),
        name = "Funded Ratio",
        #set limits
        # set the Y left labels
        labels = function(b) {
          paste0(round(b, 0), "%")
        }
      ),
      # removes the extra space so the fill is at the origin
      expand = c(0, 0)
    )
}
