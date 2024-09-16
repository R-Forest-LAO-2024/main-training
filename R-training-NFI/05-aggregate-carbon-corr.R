
## 1. libraries
library(tidyverse)

theme_set(theme_bw())

## 2. Load data
tree_agb <- read_csv("data/tree_agb.csv") ## load tree_agb.csv from 'data' folder
treeplot <- read_csv("data/treeplot.csv") ## load treeplot.csv from 'data' folder
plot     <- read_csv("data/plot.csv") ## load plot.csv from 'data' folder

CF <- 0.47

##
## 3. Aggregate from tree to treeplot ######
##


## 3.1 Aggregate 'tree' to 'treeplot'


## Check all trees have `treeplot_id`
table(tree_agb$treeplot_id, useNA = "ifany")
length(unique(tree_agb$treeplot_id))

## Demo summarise() with number of trees per ha
tp1 <- tree_agb |>
  group_by(treeplot_id) |>
  summarise(tree_count_ha = sum(treeplot_scale_factor)) 
tp1

tp11 <- tree_agb |>
  mutate(tree_n = 1) |>
  group_by(treeplot_id) |>
  summarise(tree_count = sum(tree_n))


## !! EX: Aggregate AGB 
## + Create an object 'tp2' and assign to it 'tree_agb' then pipe group_by() and 
##   summarise() to group by `treeplot_id` and calculate the sum of tree AGB per ha 
##   in a new variable `treeplot_agb_ha`
## !!
tp2 <- tree_agb |>
  group_by(treeplot_id) |>
  summarise(treeplot_agb_ha = sum(tree_agb_ha))
 

## summarise() can combine operations
tp_agb <- tree_agb |>
  group_by(treeplot_id) |>
  summarise(
    tree_count      = n(),
    tree_count_ha   = sum(treeplot_scale_factor),
    treeplot_agb_ha = sum(tree_agb_ha)
  ) 
tp_agb


## 3.2 Join treeplot information (most important: Land Cover)


## !! EX
## Create tp_comb and assign it a left_join() between 'tp_agb' and 'treeplot'.
## !! 
tp_comb <- left_join(tp_agb, treeplot, by = "treeplot_id")


## 3.3 Calculate belowground biomass


tp_rs <- tp_comb |>
  mutate(
    rs = case_when(
      lc_class == "CF" & treeplot_agb_ha <  50  ~ 0.46,
      lc_class == "CF" & treeplot_agb_ha <= 150 ~ 0.32,
      lc_class == "CF" & treeplot_agb_ha >  150 ~ 0.23,
      lc_class != "CF" & treeplot_agb_ha <  125 ~ 0.2,
      lc_class != "CF" & treeplot_agb_ha >= 125 ~ 0.24,
      TRUE ~ NA_real_
    )
  )
tp_rs

## 3.4 Calculate carbon


## !! EX
## + Create 'tp_carbon' from tp_rs and use mutate() to calculate 
##   - `treeplot_bgb_ha` with RS * AGB
##   - `treeplot_carbon_ag` with AGB * CF
##   - `treeplot_carbon_live` with (AGB + BGB) * CF
## !! 
tp_carbon <- tp_rs |>
  mutate(
    treeplot_bgb_ha = treeplot_agb_ha * rs,
    treeplot_carbon_ag = treeplot_agb_ha * CF,
    treeplot_carbon_live = (treeplot_agb_ha + treeplot_bgb_ha) * CF
  )


## 3.5 Make a plot of treeplot_carbon


ggplot(tp_carbon) +
  geom_boxplot(aes(x = plot_id, y = treeplot_carbon_ag, color = lc_class)) +
  theme(legend.position = "bottom")

ggplot(tp_carbon) +
  geom_col(aes(x = treeplot_id, y = treeplot_carbon_live, color = lc_class)) +
  geom_col(aes(x = treeplot_id, y = treeplot_carbon_ag, fill = lc_class)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  #theme(legend.position = "bottom") +
  coord_flip() +
  labs(
    x = "treeplot_id",
    y = "Carbon stock (tC/ha)",
    fill = "",
    color = ""
  )


## CORRECTION ALL IN ONE SEQUENCE
treeplot_carbon <- tree_agb |>
  group_by(treeplot_id) |>
  summarise(
    tree_count = n(),
    tree_count_ha = sum(treeplot_scale_factor),
    treeplot_agb_ha = sum(tree_agb_ha)
  ) |>
  left_join(treeplot, by = "treeplot_id") |>
  mutate(
    rs = case_when(
      lc_class == "CF" & treeplot_agb_ha <  50 ~ 0.46,
      lc_class == "CF" & treeplot_agb_ha <= 150 ~ 0.32,
      lc_class == "CF" & treeplot_agb_ha >  150 ~ 0.23,
      lc_class != "CF" & treeplot_agb_ha <  125 ~ 0.2,
      lc_class != "CF" & treeplot_agb_ha >= 125 ~ 0.24,
      TRUE ~ NA_real_
    ),
    treeplot_bgb_ha = treeplot_agb_ha * rs,
    treeplot_carbon_ag = treeplot_agb_ha * 0.47,
    treeplot_carbon_live = (treeplot_agb_ha + treeplot_bgb_ha) * 0.47
  )

ggplot(treeplot_carbon) +
  geom_boxplot(aes(x = plot_id, y = treeplot_carbon_ag, color = lc_class)) +
  theme(legend.position = "bottom")


##
## 4. Aggregate to plot ######
##

## 4.1 We need plot level land cover first
plot_lc <- plot |> select(plot_no, lc_class_plot)

tp_carbon2 <- treeplot_carbon |>
  left_join(plot_lc, by = "plot_no") |>
  filter(lc_class == lc_class_plot)

table(tp_carbon2$plot_no)
table(treeplot_carbon$plot_no)

## 4.2 Aggregate to plot
plot_c <-  tp_carbon2 |>
  group_by(plot_no, lc_class_plot) |>
  summarise(
    count_treeplot = n(),
    plot_carbon_ag = mean(treeplot_carbon_ag),
    plot_carbon_live = mean(treeplot_carbon_live),
    .groups = "drop"
  )

gg <- plot_c |>
  ggplot(aes(x = plot_no)) +
  geom_col(aes(y = plot_carbon_ag, fill = lc_class_plot)) +
  theme_bw() +
  labs(
    x = "Plot ID",
    y = "Carbon of aboveground live trees (tC/ha)",
    fill = ""
  )
print(gg)



##
## 5. Aggregate to forest type ######
##

forest_carbon <- plot_c |>
  group_by(lc_class_plot) |>
  summarise(
    count_plot = n(),
    carbon_ag = mean(plot_carbon_ag),
    carbon_ag_sd = sd(plot_carbon_ag),
    carbon_live = mean(plot_carbon_live),
    carbon_live_sd = sd(plot_carbon_live), 
  ) |>
  mutate(
    carbon_ag_se = carbon_ag_sd / sqrt(count_plot),
    carbon_ag_me = carbon_ag_se * qt(0.975, count_plot-1),
    carbon_ag_U  = carbon_ag_me / carbon_ag * 100,
    carbon_ag_cilower = carbon_ag - carbon_ag_me,
    carbon_ag_ciupper = carbon_ag + carbon_ag_me
  )

print(forest_carbon)

gg <- forest_carbon |>
  ggplot(aes(x = lc_class_plot)) +
  geom_col(aes(y = carbon_live), fill = "darkred") +
  geom_col(aes(y = carbon_ag), fill = "orange") +
  theme_bw() +
  labs(
    x = "Forest type",
    y = "Carbon (tC/ha)"
  )
print(gg)

gg_carbon <- forest_carbon |>
  ggplot(aes(x = lc_class_plot)) +
  geom_col(aes(y = carbon_ag, fill = lc_class_plot)) +
  geom_errorbar(aes(ymin = carbon_ag_cilower, ymax = carbon_ag_ciupper), width = 0.4) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = "Forest type code",
    y = "Aboveground carbon (tC/ha)"
  )
print(gg_carbon)

