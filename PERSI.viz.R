####### Reason Foundation
##### Sample Graphs/Charts in R
#### By Anil Niraula
### Data: Reason

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
library(ggplot2)
library(tidyverse)
library(tseries)
library(data.table)
library(readr)
library(rsconnect)
library(dplyr)
library(plyr)

##Load list of plans
pl <- data.table(planList())

columns <- c("total_pension_liability_dollar", "wage_inflation",
             "payroll_growth_assumption", "other_contribution_dollar",
             "other_additions_dollar", "x1_year_investment_return_percentage",
             "fiscal_year_of_contribution", "statutory_payment_dollar",
             "statutory_payment_percentage")

#Custom function to load filtered data from the database
filteredData <- function(plan, y, fy){
  Plan <- data.table(pullData(plan, y))
  ##Create missing columns for plans with no data for variables in "columns" vector
  for (i in (1:length(columns))){
    if(sum((colnames(Plan) == columns[i]))==0) {
      Plan[,columns[i] := NA] }
  }
  ####
  Plan <- Plan %>%
    filter(year > fy-1)
  Plan <- Plan %>%
    select(
      year,
      plan_name = display_name,
      state,
      return_1yr = x1_year_investment_return_percentage,
      actuarial_cost_method_in_gasb_reporting,
      funded_ratio = actuarial_funded_ratio_percentage,
      actuarial_valuation_report_date,
      ava = actuarial_value_of_assets_gasb_dollar,
      mva = market_value_of_assets_dollar,
      mva_smooth = market_assets_reported_for_asset_smoothing,#added
      aal = actuarially_accrued_liabilities_dollar,
      tpl = total_pension_liability_dollar,
      adec = actuarially_required_contribution_dollar,
      adec_paid_pct = actuarially_required_contribution_paid_percentage,
      statutory = statutory_payment_dollar,#NEW
      statutory_pct = statutory_payment_percentage,#NEW
      amortizaton_method,
      asset_valuation_method_for_gasb_reporting,
      total_benefit_payments = total_benefits_paid_dollar,#added
      benefit_payments = benefit_payments_dollar,
      refunds = refunds_dollar,#added
      admin_exp = administrative_expense_dollar,
      cost_structure,
      payroll = covered_payroll_dollar,
      ee_contribution = employee_contribution_dollar,
      ee_nc_pct = employee_normal_cost_percentage,
      er_contribution = employer_contribution_regular_dollar,
      er_nc_pct = employer_normal_cost_percentage,
      er_state_contribution = employer_state_contribution_dollar,
      er_proj_adec_pct = employers_projected_actuarial_required_contribution_percentage_of_payroll,
      other_contribution = other_contribution_dollar,#added
      other_additions = other_additions_dollar,#added
      fy_contribution = fiscal_year_of_contribution,
      inflation_assum = inflation_rate_assumption_for_gasb_reporting,
      arr = investment_return_assumption_for_gasb_reporting,
      number_of_years_remaining_on_amortization_schedule,
      payroll_growth_assumption,
      total_amortization_payment_pct = total_amortization_payment_percentage,
      total_contribution = total_contribution_dollar,
      total_nc_pct = total_normal_cost_percentage,
      total_number_of_members,
      total_proj_adec_pct = total_projected_actuarial_required_contribution_percentage_of_payroll,
      type_of_employees_covered,
      uaal = unfunded_actuarially_accrued_liabilities_dollar,
      wage_inflation
    )
}

###########
####Load Idaho PERS data
#View(pl$display_name)
IPERS <- filteredData(pl, "Idaho Public Employee Retirement System", 2001)
IPERS$year <- as.numeric(IPERS$year)
#Set to data.frame for visualization
IPERS <- data.frame(IPERS)

#Graph without a theme
ggplot(IPERS, aes(x = year, y = uaal))+
  geom_line(color = "red")+
  theme_bw()

###Sample ggplot theme
plotTheme <- ggplot2::theme(     panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                 plot.margin = margin(0, 0,0,0, "cm"),
                                 #plot.margin = margin(0.1, 3,0,3, "cm"),
                                 axis.text.y = element_text(size=8, color = "black"),
                                 axis.text.x = element_text(size=8, color = "black", angle = 90, hjust = 1, vjust = 0.5),
                                 legend.title = element_text(size = 8, colour = "white", face = "bold"),
)  

###Plot IPERS UAL with custom theme
ggplot(IPERS, aes(x = year, y = uaal))+
  geom_line(color = "red")+
  theme_bw()+
  plotTheme

#### To set reasonTheme you need ot have "Calibri" font downloaded
#INSTUCTIONS:
#https://rdrr.io/github/ReasonFoundation/reasontheme/man/calibri_install.html
calibri_test()
#[1] "Calibri is imported and registered." -- Means you are ok
calibri_install()#to install

#########
##To set the theme for ppt type:
set_reason_theme(style = "slide")

###Using debtPlot() 
###function from pensionviewr package (need year, uaal, and funded_ratio)
IPERS.debt <- data.table(IPERS %>% select(year, uaal, funded_ratio))
debtPlot(IPERS.debt)

##########Copied function details below
### Source: https://github.com/ReasonFoundation/pensionviewr/blob/master/R/debt_plot.R
##Things to experiment with:
#- Number of Ticks on Y-axis
#- Changing colors
#- Removing red line above the overfunding green area
#- Create color palette

debtPlot <- function(data) {
  data <- data %>%
    dplyr::filter(data$uaal != 0)
  # extrapolate between years linearly
  extrapo <- stats::approx(data$year, data$uaal, n = 10000)
  extrapo2 <- stats::approx(data$year, data$funded_ratio, n = 10000)
  graph <-
    data.frame(year = extrapo$x,
               uaal = extrapo$y,
               funded_ratio = extrapo2$y) %>%
    tidyr::drop_na()
  graph <- graph %>%
    dplyr::mutate(sign = dplyr::case_when(.data$uaal >= 0 ~ "positive",
                                          .data$uaal < 0 ~ "negative"))
  
  
  y_maximum <- max(graph$uaal)
  
  ggplot2::ggplot(graph,
                  ggplot2::aes(x = graph$year)) +
    ggplot2::geom_area(ggplot2::aes(y = graph$uaal, fill = graph$sign, colour = graph$sign)) +
    ggplot2::geom_line(ggplot2::aes(y = graph$funded_ratio * (y_maximum)),
                       color = "#3300FF",
                       size = 1) +
    # axis labels
    ggplot2::labs(y = "Unfunded Accrued Actuarial Liabilities (Millions)", x = NULL) +
    
    # colors assigned to pos, neg
    ggplot2::scale_fill_manual(
      values = c("negative" = "#669900",
                 "positive" = "#CC0000"),
      aesthetics = c("colour", "fill")
    ) +
    
    # sets the y-axis scale
    ggplot2::scale_y_continuous(
      # creates 10 break points for labels
      breaks = scales::pretty_breaks(n = 10),
      # changes the format to be dollars, without cents, scaled to be in billions
      labels = scales::dollar_format(
        prefix = "$",
        scale = (1e-6),
        largest_with_cents = 1
      ),
      # defines the right side y-axis as a transformation of the left side axis, maximum UAAL = 100%, sets the breaks, labels
      sec.axis = ggplot2::sec_axis(
        ~ . / (y_maximum / 100),
        breaks = scales::pretty_breaks(n = 10),
        name = "Funded Ratio",
        labels = function(b) {
          paste0(round(b, 0), "%")
        }
      ),
      # removes the extra space so the fill is at the origin
      expand = c(0, 0)
    ) +
    
    # sets the x-axis scale
    ggplot2::scale_x_continuous(breaks = round(seq(min(graph$year), max(graph$year), by = 1), 1),
                                expand = c(0, 0)) +
    
    ggplot2::theme(legend.position = "none")
}

####
