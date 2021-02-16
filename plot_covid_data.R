library(readr)
library(tidyverse)
library(plotly)

vaccinedestfile <- "~/RProjects/COVID/vaccine_data.csv"
vaccinedata <- read.csv(vaccinedestfile)

confirmed_cases_file <- "~/RProjects/COVID/confirmed_cases.csv"

confirmed_cases <- read.csv(confirmed_cases_file)

covid_status_file <- "~/RProjects/COVID/covid_status.csv"

covid_status <- read.csv(covid_status_file)

case_overview <- confirmed_cases %>% group_by(Reporting_PHU, Age_Group) %>% summarise(total = n() ) %>% pivot_wider(names_from = Age_Group, values_from = total)

covid_status$Reported.Date <- as.Date(covid_status$Reported.Date)
staticplot <- covid_status %>%
  ggplot(aes(x=Reported.Date, group=1)) + 
#  geom_line(aes(y=Confirmed.Positive), color="purple") +
#  geom_line(aes(y=Resolved), color="steelblue") + 
#  geom_line(aes(y=Deaths), color="red") +
  geom_line(aes(y = Number.of.patients.in.ICU.on.a.ventilator.with.COVID.19), color = "pink") +
  geom_line(aes(y = Number.of.patients.in.ICU.with.COVID.19), color = "coral") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")

dynamicplot <- covid_status %>%
  plot_ly(x=~Reported.Date) %>%
  add_lines(y = ~Confirmed.Positive, name = "Confirmed Positive", legendgroup = "current") %>%
  add_lines(y = ~Resolved, name = "Resolved", legendgroup = "cumulative") %>%
  add_lines(y = ~Deaths, name = "Deaths", legendgroup = "cumulative") %>%
  add_lines(y = ~Number.of.patients.in.ICU.on.a.ventilator.with.COVID.19, name = "Patients on vent", legendgroup = "current") %>%
  add_lines(y = ~Number.of.patients.in.ICU.with.COVID.19, name = "Patients in ICU", legendgroup = "current") %>%
  add_lines(y = ~Number.of.patients.hospitalized.with.COVID.19, name = "Patients in Hospital", legendgroup = "current") %>%
  layout(title = "Ontario COVID-19 Numbers",
         xaxis = list(title = "Reported Date"),
         yaxis = list(title = "Quantity of Cases")
         )
dynamicplot
