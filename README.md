# OntarioCOVID

Process and visualize data on COVID-19 for Ontario Canada

All data comes from <https://data.ontario.ca/dataset?keywords_en=COVID-19>

My initial goal for this project was to visualize how each region is faring over time with their respective case counts. It is difficult to compare say Renfrew to Waterloo because they are so different with population and geography, but by showing a scaled graphic of the case trend you can more clearly see whether they are trending together or separately.

### How it works

Each day I run a script to download updated data from the Ontario government that is outside the scope of this project. It's a pretty simple script. Then this script runs and loads all of the data. From this it calculates the following

-   Current case count by Public Health Unit and by age group

-   Total death count by Public Health Unit

-   Historical case count by Public Health Unit for last 30 days

-   The 3 day trend in each Public Health Unit

It then creates a .svg file showing the trend for the last 30 days for each public health unit. It includes on this file a label at the maximum and the current point.

Then, it loads a shapefile also provided by the Ontario government that provides the geographic region covered by each Public Health Unit. Next a data frame is created to include both the shapes and the case information. From this, the html to generate the popup is generated and attached. Finally, all of this is passed to the `leaflet` package for `R` which generates the final map output.
