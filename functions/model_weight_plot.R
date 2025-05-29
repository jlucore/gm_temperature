model_weight_plot <- function(data) {
  ggplot(data, aes(y = order)) +
    geom_bar(stat = "identity", aes(x = -m_full, fill = "m_full"), position = "identity", width = 0.4) +
    geom_bar(stat = "identity", aes(x = m_main, fill = "m_main"), position = "identity", width = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    scale_fill_manual(values = c("m_main" = "#66bd63", "m_full" = "#fee08b")) +
    labs(x = "", y = "") +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.28),
      axis.text.y = element_blank(),
      axis.text = element_text(size = 12, color = "black"),
      text = element_text(size = 10, color = "black", family = "sans"),
      axis.title = element_text(size = 12, color = "black"),
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none",
      panel.background = element_blank()
    )
}
