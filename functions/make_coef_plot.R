make_coef_plot <- function(data) {
  ggplot(data, aes(x = order, y = coef, color = direction)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "grey40") +
    geom_point(size = 3) +
    geom_linerange(aes(ymin = low_ci_95, ymax = high_ci_95), linewidth = 1) +
    geom_linerange(aes(ymin = low_ci_50, ymax = high_ci_50), linewidth = 1.5) +
    scale_color_manual(values = c("positive" = "#80b1d3", "negative" = "#fb8072"),
                       breaks = c("positive", "negative")) +
    coord_flip(clip = "off") +
    theme(axis.line = element_line(color = "black", linewidth = 0.28),
          axis.text = element_text(size = 16, color = "black"),
          text = element_text(size = 16, color = "black", family = "sans"),
          axis.title = element_text(size = 14, color = "black"),
          plot.title = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          #axis.title.x = element_blank(),
          axis.title.y = element_blank()
    )
}