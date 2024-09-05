

## Load libraries, install if needed
if (!require(sf)) install.packages("sf") else library("sf")
if (!require(tmap)) install.packages("tmap") else library("tmap") 
if (!require(tidyverse)) install.packages("tidyverse") else library("tidyverse")

## Set theme for all graphs
theme_set(theme_bw())

##
## make spatial objects ######
##

plot <- read_csv("data/plot.csv")
treeplot <- read_csv("data/treeplot.csv")

sf_lao <- st_read("data/gadm41_LAO_0.json")

sf_plot <- plot |> 
  st_as_sf(coords = c("plot_gps_lon", "plot_gps_lat"), crs = 4326)

plot
sf_plot

## !! EX
## + What are the column for lat/lon coordinates in "treeplot"? Use names()
## + Create 'sf_treeplot' based on 'treeplot'.
## !!
names(treeplot)

sf_treeplot <- treeplot |> 
  st_as_sf(coords = c("treeplot_gps_lon", "treeplot_gps_lat"), crs = 4326)



##
## Basic sf plot ######
##

ggplot() +
  geom_sf(data = sf_plot) +
  geom_sf(data = sf_lao)

## TIP: ggplot stack geometries, place the one on top at the end
ggplot() +
  geom_sf(data = sf_lao) +
  geom_sf(data = sf_plot)
  
## TIP: add transparency
ggplot() +
  geom_sf(data = sf_lao, fill = NA) +
  geom_sf(data = sf_plot)

## Change color and fill
ggplot() +
  geom_sf(data = sf_lao, fill = "green", color = "red", linewidth = 1)

ggplot() +
  geom_sf(
    data      = sf_lao, 
    fill      = "green", 
    color     = "red", 
    linewidth = 1
    )

ggplot() +
  geom_sf(
    data = sf_lao, fill = "green", color = "red", 
    linewidth = 1
    )

ggplot() +
  geom_sf(
    data = sf_lao, fill = "#546789", color = "#984562", 
    linewidth = 1
    )


## TIP: Zoom in
ggplot() +
  geom_sf(data = sf_lao, fill = NA) +
  geom_sf(data = sf_plot) +
  coord_sf(xlim = c(103, 104), ylim = c(18, 20))
 
## New plot with treeplots
sf_treeplot |>
  filter(plot_no == 3) |>
  ggplot() +
  geom_sf()


## !! EX 
## Make treeplot maps for plot 6 and 26
## !!
sf_treeplot |>
  filter(plot_no == 6) |>
  ggplot() +
  geom_sf()

sf_treeplot |>
  filter(plot_no == 26) |>
  ggplot() +
  geom_sf()

## TIP: where to place data?
ggplot(sf_plot) + geom_sf(color = "blue")

## I don't like green
# ggplot() + geom_sf(data = sf_plot, color = 'green')

sf_plot |> ggplot() + geom_sf(color = 'red')

sf_plot |> 
  ggplot() + 
  geom_sf(color = 'red') +
  geom_sf(data = sf_lao, fill = NA)


##
## Interactive maps ######
## See https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html
##

# sf_treeplot <- treeplot |>
#   st_as_sf(coords = c("treeplot_gps_lon", "treeplot_gps_lat"), crs = 4326)


tmap_mode("view")

tm_basemap("Esri.WorldImagery") +
tm_shape(sf_lao) +
  tm_borders(col = "red") +
tm_shape(sf_treeplot) +
  tm_symbols("treeplot_no", size = 0.5)
  
## !! EX
## + What spatial data would you like to visualize?
## !!

