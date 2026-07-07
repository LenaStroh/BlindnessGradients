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
    theme_minimal(base_size = 8) +
    theme(axis.title.x = element_blank()) +
    ylab(y_label) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = color_colors)
}

create_grad_plot <- function(data, x_var, y_var, x_label, y_label) {
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}}, fill = yeo)) +
    geom_point(aes(color = yeo), size = 0.75) +
    facet_wrap(
      ~ factor(group, levels = c("SC", "CB", "LB")),
      labeller = as_labeller(c(
        "SC" = "Sighted",
        "CB" = "Blind",
        "LB" = "Blind"
      ))
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    theme(text = element_text(size = 8)) +
    scale_fill_manual(values = cmap_yeo) +
    scale_color_manual(values = cmap_yeo) +
    labs(x = x_label, y = y_label)
}

create_dist_plot <- function(data, ridge_scale = 0.9, summary_stat = median, summary_gap = 0.15) {

  data <- data %>%
    mutate(
      yeo   = factor(yeo, levels = names(cmap_yeo)),
      group = factor(group, levels = c("SC", "CB", "LB"))
    )

  y0_lookup <- setNames(seq_along(levels(data$yeo)), levels(data$yeo))

  # --- one smooth density curve per (yeo, group), so individual points can be
  #     placed exactly on the curve they belong to ---
  curve_list <- data %>%
    group_by(yeo, group) %>%
    group_map(function(sub, key) {
      if (nrow(sub) < 2) return(NULL)
      dens <- stats::density(sub$mean_grad, n = 512)
      list(
        yeo   = as.character(key$yeo),
        group = as.character(key$group),
        y0    = y0_lookup[[as.character(key$yeo)]],
        dens  = dens,
        peak  = max(dens$y),
        pts   = sub
      )
    })
  curve_list <- Filter(Negate(is.null), curve_list)

  # single global scale so relative ridge heights stay comparable across rows
  height_unit <- ridge_scale / max(vapply(curve_list, function(c) c$peak, numeric(1)))

  curve_df <- bind_rows(lapply(curve_list, function(c) {
    data.frame(
      x = c$dens$x, y0 = c$y0, y = c$y0 + c$dens$y * height_unit,
      yeo = c$yeo, group = c$group
    )
  }))

  # --- individual datapoints as verticals reaching up to their own curve ---
  # (group_map() strips the grouping columns from `sub`, so add them back explicitly)
  point_df <- bind_rows(lapply(curve_list, function(c) {
    c$pts %>%
      mutate(
        yeo    = c$yeo,
        group  = c$group,
        y_base = c$y0,
        y_tip  = c$y0 + stats::approx(c$dens$x, c$dens$y, xout = mean_grad, rule = 2)$y * height_unit
      )
  }))

  # --- summary marker (just below the ridge baseline) with dotted guide + error bar ---
  summary_df <- bind_rows(lapply(curve_list, function(c) {
    x_summary <- summary_stat(c$pts$mean_grad)
    se        <- sd(c$pts$mean_grad) / sqrt(nrow(c$pts))
    data.frame(
      yeo = c$yeo, group = c$group,
      x = x_summary, xmin = x_summary - se, xmax = x_summary + se,
      y_base = c$y0, y_marker = c$y0
    )
  }))

  curve_cb   <- curve_df   %>% filter(group %in% c("CB", "LB"))
  curve_sc   <- curve_df   %>% filter(group == "SC")
  point_cb   <- point_df   %>% filter(group %in% c("CB", "LB"))
  point_sc   <- point_df   %>% filter(group == "SC")
  summary_cb <- summary_df %>% filter(group %in% c("CB", "LB"))
  summary_sc <- summary_df %>% filter(group == "SC")

  ggplot() +

    # --- CB/LB group (outline only by yeo, no fill, so individual lines stay visible) ---
    geom_line(
      data = curve_cb,
      aes(x = x, y = y, color = yeo, group = interaction(yeo, group)),
      linewidth = 0.4, show.legend = FALSE
    ) +
    geom_segment(
      data = point_cb,
      aes(x = mean_grad, xend = mean_grad, y = y_base, yend = y_tip,
          color = yeo, group = interaction(yeo, group)),
      linewidth = 0.1, show.legend = FALSE
    ) +

    # --- SC group (grey outline only) ---
    geom_line(
      data = curve_sc,
      aes(x = x, y = y, group = interaction(yeo, group)),
      color = "grey60", linewidth = 0.4, show.legend = FALSE
    ) +
    geom_segment(
      data = point_sc,
      aes(x = mean_grad, xend = mean_grad, y = y_base, yend = y_tip, group = interaction(yeo, group)),
      color = "grey60", linewidth = 0.1, show.legend = FALSE
    ) +
    geom_point(
      data = summary_sc,
      aes(x = x, y = y_marker),
      color = "grey60", size = 1.2, show.legend = FALSE
    ) +
    geom_point(
      data = summary_cb,
      aes(x = x, y = y_marker, color = yeo),
      size = 1.2, show.legend = FALSE
    ) +

    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          text = element_text(size = 8)) +

    xlab("gradient score") +
    ylab(NULL) +

    scale_y_continuous(breaks = y0_lookup, labels = names(y0_lookup)) +
    scale_color_manual(values = cmap_yeo) +
    scale_x_continuous(position = "top")
}

create_lolli_plot <- function(data, x_var, y_var, colors){
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_segment(aes(x = 0, xend = {{x_var}}, y = {{y_var}}, yend = {{y_var}}), color="grey") +
    geom_point(aes(color = {{y_var}}, 
                   alpha = ifelse(p.value < 0.05, 1, 0.5)),
               size = 1.5) +
    theme_minimal() +
    theme(
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(size = 8),
      legend.position = "none") +
    scale_color_manual(values = colors)  +
    ylab(element_blank()) +
    xlab("t-statistic") +
    scale_x_continuous(position = "top")
}

create_lolli_plot2 <- function(data, x_var, y_var, plot_title, colors, max_break) {
  data <- data %>%
    mutate(
      y_order = as.numeric({{y_var}}),
      Group   = factor(Group,  levels = c("SC", "CB")),
      source  = factor(source, levels = c("Observed", "Bootstrap"))
    ) %>%
    arrange(Group, y_order)
  
  # separate data for ribbon (bootstrap only, needs lower/upper CI)
  boot_data <- data %>% filter(source == "Bootstrap") %>%
    mutate(y_order = as.numeric({{y_var}}))
  
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    # CI ribbon for bootstrap
    geom_ribbon(
      data = boot_data,
      aes(
        xmin  = lower_ci,
        xmax  = upper_ci,
        y     = {{y_var}},
        group = Group,
        fill  = Group
      ),
      alpha       = 0.2,
      orientation = "y",
      inherit.aes = FALSE
    ) +
    # lines
    geom_line(
      aes(group    = interaction(Group, source),
          color    = Group,
          linetype = source),
      orientation = "y"
    ) +
    # points
    geom_point(aes(color = Group, shape = source), size = 1.5) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values  = colors) +
    scale_linetype_manual(values = c(Observed = "solid", Bootstrap = "dashed")) +
    scale_shape_manual(values  = c(Observed = 16, Bootstrap = 17)) +
    theme_minimal() +
    theme(
      panel.border     = element_blank(),
      axis.ticks.y     = element_blank(),
      panel.grid.minor = element_blank(),
      text             = element_text(size = 8),
      plot.title       = element_text(size = 8, hjust = 0.5, margin = margin(0,0,0,0)),
      plot.margin      = margin(5, 0, 0, 0)
    ) +
    scale_x_continuous(position = "top", limits = c(0, max(max_break))) +
    ylab(element_blank()) +
    xlab("% of bootstraps/participants") +
    labs(title = plot_title) +
    facet_grid(~source)
}

create_lolli_plot3 <- function(data, x_var, y_var, plot_title, colors, max_break, min_break) {
  
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    # CI ribbon from bootstrap data
    geom_ribbon(
      data = data,
      aes(
        xmin  = lower_ci,
        xmax  = upper_ci,
        y     = {{y_var}},
        group = group,
        fill  = group
      ),
      alpha       = 0.25,
      orientation = "y",
      inherit.aes = FALSE
    ) +
    # lines connecting observed points
    geom_line(
      aes(group = group, color = group),
      orientation = "y"
    ) +
    # observed points
    geom_point(aes(color = group), shape = 16, size = 1.5) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values  = colors) +
    theme_minimal() +
    theme(
      panel.border     = element_blank(),
      axis.ticks.y     = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      text             = element_text(size = 8),
      plot.title       = element_text(size = 8, hjust = 0.5, margin = margin(0,0,0,0)),
      plot.margin      = margin(5, 0, 0, 0)
    ) +
    scale_x_continuous(position = "top", limits = c(min_break, max(max_break))) +
    ylab(element_blank()) +
    xlab("% of participants") +
    labs(title = plot_title)
}

create_diff_plot3 <- function(data, min_val, max_val, title) {
  ggplot(data) +
    geom_brain(
      atlas = glasser(),
      position = position_brain(view ~ hemi, views = c("medial", "lateral")),
      aes(fill = statistic)
    ) +
    scale_fill_gradient2(
      low = "#43BF71",
      mid = "grey",
      high = "#440154",
      limits = c({{min_val}}, {{max_val}})
    ) +
    theme_void() +
    labs(fill = "t-stat") +
    ggtitle(title) +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(0.25, "cm"),
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),
      text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 8)
    )
}

plot_2_groups <- function(data) {
  ggplot(data) +
    geom_brain(
      atlas = glasser(),
      position = position_brain(. ~ hemi + view, views = c("lateral", "medial")),
      aes(fill = mean_grad)
    ) +
    scale_fill_gradientn(colors = c("blue", "grey", "red"), name = "gradient\nscore") +
    theme_void() +
    theme(legend.key.height = unit(0.5, "cm")) +
    theme(legend.key.width = unit(0.25, "cm")) +
    theme(text = element_text(size = 12, family = "Calibri Light")) +
    facet_grid(factor(group, levels = c("SC", "CB", "LB")) ~ ., switch = "y")
}

library(dplyr)
library(ggseg)
library(ggplot2)
library(data.table)

plot_morpho_brain <- function(morpho_df, atlas, measure_name, 
                              title = NULL,
                              fill_palette = scale_fill_viridis_c(na.value = "grey80")) {
  
  atlas_skel <- atlas$core %>%
    left_join(
      atlas$data$sf %>%
        as_tibble() %>%
        select(label, side = view) %>%
        distinct(),
      by = "label"
    ) %>%
    select(hemi, side, region, label)
  
  group_list <- unique(morpho_df$group)
  
  full_data <- lapply(group_list, function(g) {
    df_group <- morpho_df[measure == measure_name & group == g,
                          .(hemi, region, mean_meas)]
    df_full <- atlas_skel %>%
      left_join(df_group, by = c("hemi", "region")) %>%
      mutate(group = g)
    return(df_full)
  })
  
  full_data <- bind_rows(full_data)

  atlas$data$sf <- atlas$data$sf %>%
    filter(view %in% c("lateral", "medial"))
  
  p <- ggplot(full_data) +
    geom_brain(
      atlas    = atlas,
      position = position_brain(. ~ hemi + view),
      aes(fill = mean_meas)
    ) +
    fill_palette +
    theme_void() +
    labs(fill = measure_name, title = title %||% measure_name) +
    facet_grid(group ~ ., switch = "y", drop = TRUE)

  return(p)
}

make_slope_panel <- function(pc_col, grad,
                             show_y_axis    = TRUE,
                             show_x_axis    = TRUE,
                             show_strip_top = TRUE,
                             y_limits       = NULL,
                             plot_mode      = "by_group") {

  if (plot_mode == "by_group") {
    pd         <- pred_data[pred_data$pc == pc_col & pred_data$gradient == grad, ]
    color_vals <- group_colors
    fill_vals  <- group_colors
  } else if (plot_mode == "pooled") {
    pd         <- pred_data_pooled[pred_data_pooled$pc == pc_col &
                                     pred_data_pooled$gradient == grad, ]
    pd$group   <- "pooled"
    color_vals <- c(pooled = "black")
    fill_vals  <- c(pooled = "black")
  } else {
    pd         <- pred_data_pooled[pred_data_pooled$pc == pc_col &
                                     pred_data_pooled$gradient == grad, ]
    pd$group   <- "flat"
    color_vals <- c(flat = "grey")
    fill_vals  <- c(flat = "grey")
  }

  a_left  <- annot_left[annot_left$pc == pc_col & annot_left$gradient == grad, ]
  a_right <- annot_right[annot_right$pc == pc_col & annot_right$gradient == grad, ]
  a_grad  <- annot_gradient[annot_gradient$pc == pc_col &
                              annot_gradient$gradient == grad, ]

  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3, alpha = 0.5) +
    geom_ribbon(
      data = pd,
      aes(x = pc_score, ymin = lower.CL, ymax = upper.CL, fill = group),
      alpha = 0.15
    ) +
    geom_line(
      data = pd,
      aes(x = pc_score, y = fitted, color = group, group = group)
    ) +
    scale_color_manual(values = color_vals) +
    scale_fill_manual(values  = fill_vals) +
    labs(
      x     = if (show_x_axis) "PC score" else NULL,
      y     = if (show_y_axis) gradient_labels[grad] else NULL,
      title = if (show_strip_top) pc_labels[pc_col] else NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 8) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      plot.title       = if (show_strip_top)
        element_text(size = 8, hjust = 0.5)
      else
        element_blank(),
      axis.title.x = element_text(size = 7),
      axis.text    = element_text(size = 6),
      plot.margin  = unit(c(0.1, 0.02, 0.05, 0.05), "cm")
    )

  if (!show_y_axis) {
    p <- p + theme(
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )
  } else {
    p <- p + theme(axis.title.y = element_text(size = 7))
  }

  if (nrow(a_left) > 0)
    p <- p + geom_text(
      data = a_left,
      aes(x = x, y = y, label = label),
      hjust = 0, vjust = 1, size = 2,
      color = "black", inherit.aes = FALSE
    )

  if (nrow(a_right) > 0)
    p <- p + geom_text(
      data = a_right,
      aes(x = x, y = y, label = label),
      hjust = 1, vjust = 1, size = 2,
      color = "black", inherit.aes = FALSE
    )

  if (nrow(a_grad) > 0 && show_y_axis)
    p <- p + geom_text(
      data = a_grad,
      aes(x = x, y = y, label = label),
      hjust = "inward", vjust = "inward",
      angle = 90, size = 2,
      color = "black", inherit.aes = FALSE
    )

  p
}

make_dot_panel <- function(pc_col,
                           grad,
                           show_strip_top     = FALSE,
                           large_right_margin = TRUE,
                           y_limits           = NULL,
                           plot_mode          = "by_group") {

  if (plot_mode == "by_group") {
    gci        <- group_slopes_ci[group_slopes_ci$pc == pc_col &
                                    group_slopes_ci$gradient == grad, ]
    color_vals <- group_colors
  } else if (plot_mode == "pooled") {
    gci        <- group_slopes_ci_pooled[group_slopes_ci_pooled$pc == pc_col &
                                           group_slopes_ci_pooled$gradient == grad, ]
    color_vals <- c(pooled = "black")
  } else {
    gci        <- group_slopes_ci_pooled[group_slopes_ci_pooled$pc == pc_col &
                                           group_slopes_ci_pooled$gradient == grad, ]
    color_vals <- c(pooled = "grey35")
  }

  right_margin <- if (large_right_margin) 0.5 else 0.05

  p <- ggplot(gci, aes(x = group, y = slope, color = group)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3, alpha = 0.5) +
    geom_pointrange(
      aes(ymin = slope_lower, ymax = slope_upper),
      linewidth = 0.5,
      size = 0.5
    ) +
    scale_color_manual(values = color_vals) +
    labs(x = NULL, y = "slope") +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 8) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      plot.title       = if (show_strip_top)
        element_text(size = 8, hjust = 0.5)
      else
        element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y  = element_text(size = 6),
      axis.title.y = element_text(size = 7),
      plot.margin  = unit(c(0.1, right_margin, 0.05, 0.02), "cm")
    )

  p
}
