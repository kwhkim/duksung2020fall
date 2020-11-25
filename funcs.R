getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

range2 = function(x) {
  max(x) -min(x)
}

x_label_vertical = function() {
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

legend_col_alpha1 = function() {
  guides(colour = guide_legend(override.aes=list(alpha=1)))
}
