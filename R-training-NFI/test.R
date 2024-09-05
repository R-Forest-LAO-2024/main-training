


tt_dbh <- tree_agb |>
  mutate(
    tree_dbh_class  = floor(tree_dbh / 5) * 5,
    tree_dbh_class2 = if_else(tree_dbh < 100, tree_dbh_class, 100)
  )

tp_dbh <- tt_dbh |>
  group_by(treeplot_id, plot_no, tree_dbh_class2) |>
  summarise(
    tree_count = sum(treeplot_scale_factor), 
    .groups = "drop"
  ) |>
  left_join(plot_lc, by = "plot_no") |>
  group_by(plot_no, lc_class_plot, tree_dbh_class2) |>
  summarise(
    tree_count_plot = mean(tree_count), 
    .groups =  "drop"
  )

ggplot(tp_dbh) +
  geom_col(aes(x = tree_dbh_class2, y = tree_count_plot, fill = lc_class_plot)) +
  facet_wrap(~lc_class_plot) +
  theme(legend.position = "none")



