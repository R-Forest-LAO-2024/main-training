

set.seed(1)

tree <- tibble(
  plot_no = c(rep(1, 10), rep(2, 7), rep(3, 12), rep(4, 9)),
  treeplot_no = c(
    rep("A", 4), rep("B", 5), rep("C", 1),
    rep("A", 2), rep("B", 1), rep("C", 4),
    rep("A", 7), rep("B", 4), rep("C", 1),
    rep("A", 3), rep("B", 3), rep("C", 3)
    ),
  treeplot_id = paste0(plot_no, treeplot_no),
  tree_dbh = sample(10:105, 38, replace = TRUE)
)

treeplot <- tibble(
  plot_no = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)),
  treeplot_no = rep(c("A", "B", "C"), 4),
  treeplot_id = paste0(plot_no, treeplot_no),
  lc_class = sample(c("MDF", "DD", "EG", "CF", "MCB"), 12, replace = TRUE)
)

plot <- tibble(
  plot_no = 1:4,
  lc_class_plot = c("MDF", "DD", "DD", "EG")
)

write_csv(tree, "data/tree_new.csv")
write_csv(treeplot, "data/treeplot_new.csv")
write_csv(plot, "data/plot_new.csv")
