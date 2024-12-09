mytheme <- theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    #panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    text = element_text(size = 12, family = "Calibri Light"),
    strip.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text(vjust = 0.9, size = 18),
    axis.title.y = element_text(
      vjust = 0.1,
      size = 12,
      margin = margin(
        t = 0,
        r = 10,
        b = 0,
        l = 0
      )
    ),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.title = element_blank()
  )

create_violin_boxplot <- function(data, x_var, y_var, color_var, theme, x_label, y_label, fill_colors, color_colors) {
  ggplot(data, aes_string(x = x_var, y = y_var, color = color_var, fill = color_var)) +
    geom_flat_violin(
      aes_string(fill = color_var),
      position = position_nudge(x = .25, y = 0),
      adjust = 1.5,
      trim = FALSE,
      alpha = .5,
      colour = NA
    ) +
    geom_boxplot(
      aes_string(x = x_var, y = y_var, color = color_var),
      position = position_dodge(width = 0.25),
      outlier.shape = NA,
      alpha = .5,
      width = .2,
      fill = "white"
    ) +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    theme +
    xlab(x_label) +
    ylab(y_label) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = color_colors)
}

create_grad_plot <- function(data, x_var, y_var, x_label, y_label) {
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}}, fill = yeo)) +
    geom_point(aes(color = yeo)) +
    facet_wrap(~ group) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    theme(text = element_text(size = 12, family = "Calibri Light")) +
    scale_fill_manual(values = cmap_yeo) +
    scale_color_manual(values = cmap_yeo) +
    labs(x = x_label, y = y_label)
}

create_diff_plot <- function(data1, x_var, y_var, data2, x_label, y_label) {
  ggplot(data1, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_point(alpha = 0.5, color = "grey") +
    facet_wrap(~ group) +
    geom_point(data = data2, aes(x = {{x_var}}, y = {{y_var}}, color = yeo)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    theme(text = element_text(size = 12, family = "Calibri Light")) +
    scale_fill_manual(values = cmap_yeo) +
    scale_color_manual(values = cmap_yeo) +
    labs(x = x_label, y = y_label) +
    scale_color_manual(values = cmap_yeo)
}

create_diff_plot2 <- function(data, data2, y_label) {
  ggplot(data, aes(x = group, y = mean_grad)) +
    geom_point(alpha = 0.5, color = "grey", shape = "-", size = 5) +
    geom_point(data = data2,
               aes(x = group, y = mean_grad, color = yeo),
               shape = "-", size = 5) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    theme(text = element_text(size = 12, family = "Calibri Light")) +
    ylab(y_label) +
    scale_color_manual(values = cmap_yeo)
}

create_diff_plot3 <- function(data) {
  ggplot(data) +
    geom_brain(atlas = glasser,
               position = position_brain(side ~ hemi),
               aes(fill = statistic)) +
    scale_fill_gradient2(low = "#440154", mid = "grey", high = "#43BF71",
                         limits = c(-7, 8)) +
    theme_void() +
    theme(legend.key.height = unit(0.5, "cm")) +
    theme(legend.key.width = unit(0.25, "cm")) +
    theme(text = element_text(size = 12, family = "Calibri Light")) +
    labs(fill = "t-stat")
}

# group_diff_g1_plot3 <- ggplot(data = diff_regions[gradient == "gradient_1", ]) +
#   geom_brain(atlas = glasser,
#              position = position_brain(side ~ hemi),
#              aes(fill = statistic)) +
#   scale_fill_gradient2(low = "#440154", mid = "grey", high = "#43BF71",
#                        limits = c(-7, 8)) +
#   theme_void() +
#   theme(legend.key.height = unit(0.5, "cm")) +
#   theme(legend.key.width = unit(0.25, "cm")) +
#   theme(text = element_text(size = 12, family = "Calibri Light")) +
#   labs(fill = "t-stat")
