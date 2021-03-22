library(readr)
library(tidyverse)
library(plotly)
library(sf)
library(rmapshaper)
library(leaflet)
library(ggformula)
library(directlabels)
library(ggrepel)

#  Load all of my data files.  These are downloaded daily by a separate script
vaccinedestfile <- "~/RProjects/COVID/vaccine_data.csv"
vaccinedata <- read.csv(vaccinedestfile)
confirmed_cases_file <- "~/RProjects/COVID/confirmed_cases.csv"
confirmed_cases <- read.csv(confirmed_cases_file)
covid_status_file <- "~/RProjects/COVID/covid_status.csv"
covid_status <- read.csv(covid_status_file)
covid_status_by_PHU_file <- "~/RProjects/COVID/covid_status_by_PHU.csv"
covid_status_by_PHU <- read.csv(covid_status_by_PHU_file)

# Create a table showing total case count by region
case_overview <- confirmed_cases %>% group_by(Reporting_PHU_ID, Age_Group) %>% summarise(total = n() ) %>% pivot_wider(names_from = Age_Group, values_from = total)

# Change this field to be a date field so rendering graphics works better
covid_status$Reported.Date <- as.Date(covid_status$Reported.Date)

# Static plot to explore data.  Unlikely to be used for anything.
staticplot <- covid_status %>%
  ggplot(aes(x=Reported.Date, group=1)) + 
#  geom_line(aes(y=Confirmed.Positive), color="purple") +
#  geom_line(aes(y=Resolved), color="steelblue") + 
#  geom_line(aes(y=Deaths), color="red") +
  geom_line(aes(y = Number.of.patients.in.ICU.on.a.ventilator.with.COVID.19), color = "pink") +
  geom_line(aes(y = Number.of.patients.in.ICU.with.COVID.19), color = "coral") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%y")

# staticplot

# Dynamic plot of similar data, likely to be used in finished product
dynamicplot <- covid_status %>%
  plot_ly(x=~Reported.Date) %>%
  add_lines(y = ~Confirmed.Positive, name = "Confirmed Positive", legendgroup = "current") %>%
  add_lines(y = ~Resolved, name = "Resolved", legendgroup = "cumulative") %>%
  add_lines(y = ~Deaths, name = "Deaths", legendgroup = "cumulative") %>%
  add_lines(y = ~Number.of.patients.in.ICU.on.a.ventilator.with.COVID.19, name = "Patients on vent", legendgroup = "current") %>%
  add_lines(y = ~Number.of.patients.in.ICU.with.COVID.19, name = "Patients in ICU", legendgroup = "current") %>%
  add_lines(y = ~Number.of.patients.hospitalized.with.COVID.19, name = "Patients in Hospital", legendgroup = "current") %>%
  add_lines(y = ~Total.LTC.Resident.Deaths, name = "Long Term Care Resident Deaths", legendgroup = "cumulative") %>%
  layout(title = "Ontario COVID-19 Numbers",
         xaxis = list(title = "Reported Date"),
         yaxis = list(title = "Quantity of Cases")
         )
# dynamicplot

# Lets look at daily updates by region.  Create separate pivots for active, resolved, and death counts by region
covid_status_by_PHU$FILE_DATE <- as.Date(covid_status_by_PHU$FILE_DATE)
covid_status_by_PHU$PHU_NUM <- as.integer(covid_status_by_PHU$PHU_NUM)
activecases <- covid_status_by_PHU[covid_status_by_PHU$PHU_NAME != "",] %>%
  dplyr::select(!c(PHU_NAME, RESOLVED_CASES, DEATHS)) %>% 
  pivot_wider(names_from = c(PHU_NUM), values_from = c(ACTIVE_CASES), values_fn = sum, names_sep = "`")
resolvedcases <- covid_status_by_PHU[covid_status_by_PHU$PHU_NAME != "",] %>% 
  dplyr::select(!c(PHU_NAME, ACTIVE_CASES, DEATHS)) %>% 
  pivot_wider(names_from = c(PHU_NUM), values_from = c(RESOLVED_CASES), values_fn = sum, names_sep = "`")
deathcases <- covid_status_by_PHU[covid_status_by_PHU$PHU_NAME != "",] %>% 
  dplyr::select(!c(PHU_NAME, ACTIVE_CASES, RESOLVED_CASES)) %>% 
  pivot_wider(names_from = c(PHU_NUM), values_from = c(DEATHS), values_fn = sum, names_sep = "`")


# I want to generate a plot that shows the 30-day trend for each PHU
# For each PHU we will generate a small plot, save as svg in /img folder
# then reference to it will be included in popup html so image of plot shows
# within the popup

plotsize <- c(100,60)

make_phu_plot <- function (cases, col_id) {
  df <- data.frame("x" = cases[1], "y" = cases[col_id])
  colnames(df) <- c("FILE_DATE", "y")
  df <- df %>% slice_max(FILE_DATE, n = 30)
  ggplot(data = df, aes(x = FILE_DATE, y = y)) +
    geom_line(
      colour = "pink",
      size = 0.1
    ) +
    geom_spline (
      color = "red",
      size = 0.75,
      alpha = 0.5,
      lineend = "square"
    ) +
    #geom_dl(
    #  data = subset(df, FILE_DATE == max(FILE_DATE)),
    #  aes(label = y),
    #  method = list(
    #    dl.trans(x = x + 0.2),
    #    "last.points", cex = 0.8)
    #) +
    geom_text(
      data = subset(df, FILE_DATE == max(FILE_DATE)),
      aes(label = y),
      size = 3,
      nudge_x = 2,
      hjust = "inward",
      vjust = "inward"
    ) +
    geom_text_repel(
      data = subset(df, y == max(y)),
      aes(label = y),
      size = 3,
      nudge_y = 5
      #hjust = "inward",
      #vjust = "inward"
    ) +
    theme_void() +
    ggtitle("Last 30 days") + 
    theme(
      plot.title = element_text(color="black", size=10, face="italic", hjust = 0.5, vjust = 2),
      panel.grid.major.x = element_line(colour = "black"),
      axis.text.x = element_text(angle = 0, debug = FALSE, size = 8)
    ) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%B %e")
}

save_phu_plot <- function (cases, col_id) {
  ggsave(
    paste("~/ShinyApps/OntarioCOVID/img/",colnames(cases[col_id]),".svg",sep=""),
    plot = make_phu_plot(cases,col_id),
    width = 80,
    height = 20,
    units = "mm",
    dpi = "screen"
  )
}
result <- sapply(2:length(activecases), function(i) save_phu_plot(activecases,i))
#save_phu_plot(activecases,phuquantity)
#ggplot(activecases, aes(x=1, y=activecases[2])) + geom_line()

determine_rise_fall <- function(cases, col_id) {
  df <- data.frame("x" = cases[1], "y" = cases[col_id])
  colnames(df) <- c("FILE_DATE", "y")
  print(max((df$y[df$FILE_DATE == max(df$FILE_DATE)-3]),1,na.rm=TRUE))
  #print(df$y[df$FILE_DATE == max(df$FILE_DATE)],",", (df$y[df$FILE_DATE == max(df$FILE_DATE)-3]))
  if (df$y[df$FILE_DATE == max(df$FILE_DATE)] / 
      max((df$y[df$FILE_DATE == max(df$FILE_DATE)-3]),1,na.rm=TRUE) >= 1.03) {
    return("Rising")
  } else if (df$y[df$FILE_DATE == max(df$FILE_DATE)] / 
             max((df$y[df$FILE_DATE == max(df$FILE_DATE)-3]),1,na.rm=TRUE) <= 0.97) {
    return("Falling")
  } else return("Stagnant")
}

determine_growth <- function(cases,col_id) {
  df <- data.frame("FILE_DATE" = cases[1], "y" = cases[col_id])
  colnames(df) <- c("FILE_DATE", "y")
  print(df)
  return((df$y[df$FILE_DATE == max(df$FILE_DATE)] / 
             max((df$y[df$FILE_DATE == max(df$FILE_DATE)-3]),1,na.rm=TRUE)))
}

riseorfall <- as_tibble(sapply(2:length(activecases), function(i) determine_rise_fall(activecases,i)))
riseorfall$value <- factor(riseorfall$value, levels = c("Rising", "Stagnant", "Falling"))
riseorfall$PHU_ID <- as.numeric(colnames(activecases[-1]))
riseorfall$growth <- (sapply(2:length(activecases), function(i) determine_growth(activecases,i)))


# Load in all of our shape file data.

if(file.exists('shapes/simplified.shp')) {
  shapes <- st_read('shapes/simplified.shp')

} else {
  shapes <- st_read('shapes/Ministry_of_Health_Public_Health_Unit_Boundary.shp')
  shapes <- ms_simplify(shapes)
  result <- st_write(shapes,'shapes/simplified.shp', append=FALSE)
}
mapshapes <- st_read('shapes/simplified.shp')

latestactive <- activecases[activecases$FILE_DATE == max(activecases$FILE_DATE),] %>% 
  pivot_longer(!FILE_DATE, names_to = "PHU_ID", values_to = "latestactive")
latestactive$PHU_ID <- as.integer(latestactive$PHU_ID)
# latestactive <- latestactive %>% pivot_longer(!FILE_DATE, names_to = "PHU_ID", values_to = "latestactive")
# latestactive$PHU_ID <- as.integer(latestactive$PHU_ID)
mapshapes <- latestactive %>% right_join(mapshapes)

latestresolved <- resolvedcases[resolvedcases$FILE_DATE == max(resolvedcases$FILE_DATE),] %>% 
  pivot_longer(!FILE_DATE, names_to = "PHU_ID", values_to = "latestresolved")
latestresolved$PHU_ID <- as.integer(latestresolved$PHU_ID)
# latestresolved <- latestresolved %>% pivot_longer(!FILE_DATE, names_to = "PHU_ID", values_to = "latestresolved")
# latestresolved$PHU_ID <- as.integer(latestresolved$PHU_ID)
mapshapes <- latestresolved %>% 
  dplyr::select(PHU_ID, latestresolved) %>% right_join(mapshapes, by = "PHU_ID")

latestdeath <- deathcases[deathcases$FILE_DATE == max(deathcases$FILE_DATE),] %>% 
  pivot_longer(!FILE_DATE, names_to = "PHU_ID", values_to = "latestdeath")
latestdeath$PHU_ID <- as.integer(latestresolved$PHU_ID)
mapshapes <- latestdeath %>% 
  dplyr::select(PHU_ID, latestdeath) %>% right_join(mapshapes, by = "PHU_ID")

#mapshapes$popup <- paste(sep="",
#                      "<b>", NAME_ENG,"</b><br>",
#                      "<b>Active</b><div id=currentactive>",latestactive,"<br></div>",
#                      "<b>Resolved:</b><div id=currentresolved>",latestresolved,"<br></div>")
shapes <- shapes %>% 
  left_join(mapshapes %>% 
  dplyr::select(c("PHU_ID", "latestactive", "latestresolved", "latestdeath")))

shapes <- shapes %>% left_join(case_overview, by = c("PHU_ID" = "Reporting_PHU_ID"))

shapes <- shapes %>% left_join(riseorfall, by="PHU_ID")

shapes$popup <- paste(sep="",
                      "<div style=\"font-size:14px;text-align:center;font-weight:bold;\">", shapes$NAME_ENG,"</div><br>",
                      "<div id=current><b>Active: </b>",shapes$latestactive,"</div>",
                      "<b>Resolved: </b>",shapes$latestresolved,"<br></div>",
                      "<br><div style=\"font-size:12px;text-align:center;font-weight:bold;\">","Total COVID-19 cases by age group","</div>",
                      "<table rules=\"all\" style= \"border:1px solid black;font-size:10px;width:300px;text-align:center\"><tr style=\"font-weight:bold;\"><td>&lt;20</td><td>20s</td><td>30s</td><td>40s</td><td>50s</td><td>60s</td><td>70s</td><td>80s</td><td>90+</td></tr>",
                      "<tr style=\"font-weight:normal;\"><td>", shapes$`<20`,
                      "</td><td>", shapes$`20s`,
                      "</td><td>", shapes$`30s`,
                      "</td><td>", shapes$`40s`,
                      "</td><td>", shapes$`50s`,
                      "</td><td>", shapes$`60s`,
                      "</td><td>", shapes$`70s`,
                      "</td><td>", shapes$`80s`,
                      "</td><td>", shapes$`90+`,
                      "</td></tr></table><br>",
                      "<img src=\"./img/", shapes$PHU_ID, ".svg\" ><br>",
                      "<div style=\"font-size:6pt;text-align:right\">","Last updated: ",max(activecases$FILE_DATE),"</div>"
                      )
pal <- colorFactor(palette = c("#D2222D","#FCBE00","#087200"),as.factor(shapes$value))

m <- leaflet(shapes) %>%
  addTiles() %>%
  addPolygons(
    color = "#444444", 
    weight = 1, 
    smoothFactor = 0.5,
    opacity = 1.0,
    #fillOpacity = ~as.numeric(Frequency)/100,
    fillColor = ~pal(value),
    popup = ~popup,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    "topright", 
    pal = pal,
    values = ~value,
    title = "3-day Trend"
  )
library(htmlwidgets)
saveWidget(m, file="~/ShinyApps/OntarioCOVID/index.html")
