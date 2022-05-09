fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}

draw_and_run_single <- function(df,
                                dep_vars_pool,
                                indep_vars_pool,
                                max_add_preds,
                                cul_vars,
                                adj_vars,
                                weight_var,
                                do_weights,
                                region_var,
                                use_regions,
                                outlier_mad,
                                winsor_outliers,
                                is_main,
                                return_all_preds = FALSE) {
  # drawing a specification
  selected_dep_var    <- sample(dep_vars_pool, size = 1)
  
  if (indep_vars_pool[1] == "none") {
    selected_indep_vars <- NULL
  } else{
    selected_indep_vars <-
      sample(indep_vars_pool, size = sample(seq(1, max_add_preds, 1),size = 1))
    if (sum(grepl("wdi_emp", selected_indep_vars)) == 3) {
      selected_indep_vars <-
        selected_indep_vars[-which(grepl("wdi_emp", selected_indep_vars))[1]]
    }
  }
  
  if (adj_vars[1] == "none"){
    selected_adj_vars <- NULL
  }else {
    selected_adj_vars <- adj_vars
  }
  
  if (do_weights == "random") {
    do_weights <- sample(c(TRUE, FALSE), size = 1)
  }
  if (use_regions == "random") {
    use_regions <- sample(c(TRUE, FALSE), size = 1)
  }
  if (winsor_outliers == "random") {
    winsor_outliers <- sample(c(TRUE, FALSE), size = 1)
  }
  
  # preparing the specification
  data <- df %>%
    select(
      one_of(
        cul_vars,
        selected_adj_vars,
        selected_dep_var,
        selected_indep_vars,
        weight_var,
        region_var
      )
    ) %>%
    drop_na() %>% mutate(across(.cols = one_of(cul_vars, selected_adj_vars, selected_dep_var, selected_indep_vars), function(x)
      as.numeric(scale(x))))

  if (winsor_outliers) {
    data %>%
      summarise(across(.cols = c(cul_vars, selected_dep_var), function(x) {
        median(x, na.rm = TRUE) + c(-1, 1) * mad(x, na.rm = TRUE) * outlier_mad
      })) -> thresholds
    
    for (i in seq(1, length(c(cul_vars, selected_dep_var)), 1)) {
      data[c(cul_vars, selected_dep_var)][, i] <-
        Winsorize(
          pull(data[c(cul_vars, selected_dep_var)][, i]),
          minval = pull(thresholds[, i])[1],
          maxval = pull(thresholds[, i])[2]
        )
    }
    
  }
  
  if ((indep_vars_pool[1] == "none") & adj_vars[1] != "none") {
    formula <- glue("{selected_dep_var} ~
                       {paste0(c(cul_vars, selected_adj_vars), collapse = ' + ')}")
  } else if ((indep_vars_pool[1] == "none") & adj_vars[1] == "none") {
    formula <- glue("{selected_dep_var} ~
                       {paste0(c(cul_vars), collapse = ' + ')}")
  } else{
    formula <- glue(
      "{selected_dep_var} ~
                       {paste0(c(cul_vars, selected_adj_vars), collapse = ' + ')} +
                       {paste0(selected_indep_vars, collapse = ' + ')}"
    )
  }
  
  if (use_regions) {
    formula <- paste0(formula, " + ", region_var)
  }
  
  # run specification
  if (do_weights) {
    mod <-
      lm(as.formula(formula),
         data = data,
         weights = pull(data[, weight_var]))
  } else{
    mod <- lm(as.formula(formula), data = data)
  }
  
  # create results table
  results <- tidy(mod, conf.int = TRUE) %>%
    mutate(
      obs = nobs(mod),
      R2 = summary(mod)$r.squared,
      adj.R2 = summary(mod)$adj.r.squared,
      is.main = is_main
    )
  
  if (return_all_preds == FALSE) {
    results <- results %>% filter(term %in% cul_vars)
  }
  
  recode_labs(
    results,
    selected_dep_var,
    selected_indep_vars,
    do_weights,
    winsor_outliers,
    use_regions
  )
  
}

draw_and_run_set <- function(data,
                             list_dep_vars_pool,
                             list_indep_vars_pool,
                             list_max_add_preds,
                             list_cul_vars,
                             list_adj_vars,
                             list_weight_vars,
                             list_do_weights,
                             list_region_var,
                             list_use_regions,
                             list_outlier_mad,
                             list_winsorize_ourliers,
                             list_is_main, 
                             list_return_all_preds){
  
  no_models <- length(list_dep_vars_pool)
  map_dfr(as.list(1:no_models), function(i){
    
    m <- draw_and_run_single(data, 
                             dep_vars_pool    = list_dep_vars_pool[[i]],
                             indep_vars_pool  = list_indep_vars_pool[[i]],
                             max_add_preds    = list_max_add_preds[[i]],
                             cul_vars         = list_cul_vars[[i]],
                             adj_vars         = list_adj_vars[[i]],
                             weight_var       = list_weight_vars[[i]],
                             do_weights       = list_do_weights[[i]],
                             region_var       = list_region_var[[i]],
                             use_regions      = list_use_regions[[i]],
                             outlier_mad      = list_outlier_mad[[i]],
                             winsor_outliers  = list_winsorize_ourliers[[i]],
                             is_main          = list_is_main[[i]],
                             return_all_preds = list_return_all_preds[[i]]) %>% 
      mutate(cul_vars = paste(list_cul_vars[[i]], collapse = " + "),
             adj_vars = paste(list_adj_vars[[i]], collapse = " + "),
             dep_vars_pool = paste(list_dep_vars_pool[[i]], collapse = " + "))
    
  })
}

spec_curve_upper_part <- function(results,
                                  cul_vars,
                                  cul_vars_names,
                                  y_limits,
                                  nr_curves) {
  plot_curve(results %>% filter(term == cul_vars[1]),
             ci = TRUE,
             ribbon = FALSE) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "black") +
    coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
    labs(y = "Estimate (95% CI)",
         x = "",
         subtitle = cul_vars_names[1]) +
    theme_half_open() +
    theme(legend.position = "none",
          plot.margin = unit(c(1, 1, -0.75, 1), "cm")) -> p1
  
  if (nr_curves == 2) {
    plot_curve(
      results %>% filter(term == cul_vars[2]),
      ci = TRUE,
      ribbon = FALSE,
      get_ordering_from = results %>% filter(term == cul_vars[1])
    ) +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 color = "black") +
      coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
      labs(y = "Estimate (95% CI)",
           x = "",
           subtitle =  cul_vars_names[2]) +
      theme_half_open() +
      theme(legend.position = "none",
            plot.margin = unit(c(-0.75, 1, -0.5, 1), "cm")) -> p2
    p1 / p2
  } else {
    p1
  }
}

format_results <- function(df, null = 0, desc = FALSE) {
  if (isFALSE(desc)) {
    df <- df %>%
      dplyr::arrange(.data$estimate)
  } else {
    df <- df %>%
      dplyr::arrange(desc(.data$estimate))
  }
  
  df <- df %>%
    dplyr::mutate(
      specifications = 1:n(),
      color = case_when(
        conf.low > null ~ "#377eb8",
        conf.high < null ~ "#e41a1c",
        TRUE ~ "darkgrey"
      )
    )
  return(df)
}

plot_curve <-
  function (df,
            desc = FALSE,
            ci = TRUE,
            ribbon = FALSE,
            legend = FALSE,
            null = 0,
            get_ordering_from = NULL)
  {
    if (is.null(get_ordering_from)) {
      formatted_results <-
        df %>% format_results(desc = desc, null = null)
    } else{
      formatted_results_own  <-
        df %>% format_results(desc = desc, null = null) %>% select(-specifications)
      formatted_results_from <-
        format_results(get_ordering_from, desc = desc, null = null)
      
      formatted_results <- formatted_results_own %>%
        left_join(.,
                  formatted_results_from %>% select(specnr, specifications),
                  by = "specnr")
    }
    
    plot <- formatted_results %>%
      ggplot(
        aes(
          x = .data$specifications,
          y = .data$estimate,
          ymin = .data$conf.low,
          ymax = .data$conf.high,
          color = .data$color
        )
      ) +
      geom_point(aes(color = .data$color, alpha = as.integer(.data$is.main)), size = 1.25) + theme_minimal() +
      scale_color_identity() + theme(
        strip.text = element_blank(),
        axis.line = element_line("black", size = 0.5),
        legend.position = "none",
        panel.spacing = unit(0.75,
                             "lines"),
        axis.text = element_text(colour = "black")
      ) +
      labs(x = "") +
      scale_alpha_continuous(range = c(0.35, 1))
    
    
    
    if (isFALSE(legend)) {
      plot <- plot + theme(legend.position = "none")
    }
    if (isTRUE(ci)) {
      plot <-
        plot + geom_pointrange(aes(alpha = as.integer(.data$is.main)),
                               size = 1,
                               fatten = 1)
    }
    if (isTRUE(ribbon)) {
      plot <- plot + geom_ribbon(
        aes(
          ymin = .data$conf.low,
          ymax = .data$conf.high,
          color = "lightgrey",
          alpha = .data$is.main
        )
      )
    }
    return(plot)
  }

plot_choices <- function(df,
                         choices = c("x", "y", "model", "controls", "subsets"),
                         desc = FALSE,
                         null = 0) {
  value <- key <- NULL
  
  df %>%
    format_results(desc = desc, null = null) %>%
    tidyr::gather(key, value, choices) %>%
    filter(!is.na(.data$value)) %>%
    dplyr::mutate(
      key = factor(.data$key, levels = choices),
      value = fct_reorder(
        .f = .data$value,
        .x = extract_numeric(.data$key),
        .desc = TRUE
      ),
      group = case_when(
        key %in% paste0("c", sprintf("%02d", 1:3)) ~ "Dep.\nvar.",
        key %in% paste0("c", sprintf("%02d", 4:16)) ~ "Additional adjustment\nvariables",
        TRUE ~ "Model/\ndata"
      ),
      group = factor(
        group,
        ordered = TRUE,
        levels =
          c(
            "Dep.\nvar.",
            "Additional adjustment\nvariables",
            "Model/\ndata"
          )
      )
    ) %>%
    ggplot(aes(
      x = .data$specifications,
      y = .data$value,
      color = .data$color
    )) +
    geom_point(aes(x = .data$specifications,
                   y = .data$value),
               shape = 15,
               size = 1) +
    scale_color_identity() +
    theme_minimal() -> p
  
  p + facet_grid(.data$group  ~ 1,
                 scales = "free_y", space = "free_y") +
    theme_half_open() +
    theme(
      axis.line = element_line("black", size = .5),
      legend.position = "none",
      panel.spacing = unit(.75, "lines"),
      axis.text = element_text(colour = "black"),
      strip.text = element_text(size = 9),
      strip.background = element_blank(),
      axis.text.y = element_text(size = 10),
      plot.margin = unit(c(-0.5, 1, -0.5, 1), "cm")
    ) +
    labs(x = "", y = "")
  
}

recode_labs <- function(results, selected_dep_var, selected_indep_vars, do_weights, winsor_outliers, use_regions){

  results %>%
    mutate(
      # dep vars
      c01 = ifelse(selected_dep_var %in% c("obesity_m","obesity_f"),         "Obesity rate (BMI \u2265 30)", NA),
      c02 = ifelse(selected_dep_var %in% c("overw_m","overw_f"),             "Overweight rate (BMI \u2265 25)", NA),
      c03 = ifelse(selected_dep_var %in% c("BMI_m","BMI_f"),                 "Avg. BMI", NA),
      # predictor vars
      c04 = ifelse(any(grepl("hf_efiscore", selected_indep_vars)),           "Economic freedom index", NA),
      c05 = ifelse(any(grepl("wdi_empagr", selected_indep_vars)),            "Agriculture employm. share", NA),
      c06 = ifelse(any(grepl("wdi_empind", selected_indep_vars)),            "Industry employm. share", NA),
      c07 = ifelse(any(grepl("wdi_empser", selected_indep_vars)),            "Services employm. share", NA),
      c08 = ifelse(any(grepl("healthexp_perc_gdp", selected_indep_vars)),    "Health expenditure", NA),
      c09 = ifelse(any(grepl("wdi_popurb_share", selected_indep_vars)),      "Urban population share", NA),
      c10 = ifelse(any(grepl("wdi_smok", selected_indep_vars)),              "Smoking prevalence", NA),
      c11 = ifelse(any(grepl("mcdonalds", selected_indep_vars)),             "McDonald's density", NA),
      c12 = ifelse(any(grepl("temp_avg", selected_indep_vars)),              "Avg. temperature", NA),
      c13 = ifelse(any(grepl("wgi_avg", selected_indep_vars)),               "Avg. Governance Indicators", NA),
      c14 = ifelse(any(grepl("rice", selected_indep_vars)),                  "Rice supply", NA),
      c15 = ifelse(any(grepl("share_animal_prot", selected_indep_vars)),     "Share animal protein", NA),
      c16 = ifelse(any(grepl("vegies_fruits", selected_indep_vars)),         "Vegetables/fruits supply", NA),
      # others
      c17 = ifelse(use_regions == TRUE,                                      "World region fixed effects", NA),
      c18 = ifelse(do_weights == TRUE,                                       "Use population weights", NA),
      c19 = ifelse(winsor_outliers == TRUE,                                  "Winsorize outliers", NA)
      ) -> results
  return(results)
}

create_spec_curve <- function(results,
                              cul_vars,
                              cul_vars_names,
                              main,
                              y_limits,
                              nr_curves) {
  # plotting
  # ...upper part
  up <- spec_curve_upper_part(results,
                              cul_vars,
                              cul_vars_names,
                              y_limits,
                              nr_curves)
  
  # ...middle part
  middle <- plot_choices(results %>% filter(term == cul_vars[1]),
                         choices = c(paste0("c", sprintf("%02d", 1:19))))
  
  
  # ...lower part
  low <- plot_samplesizes(results %>% filter(term == cul_vars[1])) +
    theme_half_open() +
    labs(x = "Specification (ordered)",
         y = "Sample size") +
    theme(plot.margin = unit(c(-0.5, 1, 1, 1), "cm"))
  
  if (nr_curves == 2) {
    tmp <-
      (up / middle) / low + plot_layout(heights = c(0.75, 0.75, 1.5, 0.4))
  } else{
    tmp <- (up / middle) / low + plot_layout(heights = c(0.75, 1.5, 0.4))
  }
  
  titleplot <-
    ggplot() + geom_text(aes(label = main),
                         x = 0.5,
                         y = 0.5,
                         size = 6) +
    theme_void() +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  titleplot / tmp + plot_layout(heights = c(0.05, 0.95)) &
    theme(strip.placement = NULL)
}

spec_table <- function(results) {
  results %>%
    filter(!is.main) %>%
    group_by(term) %>%
    summarise(
      "Median estimate" = median(estimate),
      "Positive estimates (signif.)" = glue("{sum(estimate>0)} ({sum(estimate>0 & p.value<0.05)})"),
      "Negative estimates (signif.)" = glue("{sum(estimate<0)} ({sum(estimate<0 & p.value<0.05)})"),
      "Median sample size" = median(obs)
    )
}

part_cor_plot <- function(data,
                          icon,
                          dep,
                          adj_vars,
                          cul_vars,
                          cul_vars_names,
                          weight_var,
                          plot_title,
                          x_label,
                          y_label) {
  df_part.plots <- data %>%
    select(one_of(dep, cul_vars, adj_vars, weight_var, "country_short")) %>%
    drop_na() %>%
    mutate(across(.cols = one_of(dep, cul_vars, adj_vars), function(x)
      as.numeric(scale(x))))
  
  res_part.plots <- tibble(
    !!(weight_var) := df_part.plots %>% select(weight_var) %>% pull,!!paste0(dep, "_", cul_vars[1]) := lm(
      as.formula(
        glue("{dep} ~ {paste(adj_vars, collapse = '+')} + {cul_vars[2]}")
      ),
      data = df_part.plots,
      weights = pull(df_part.plots[, weight_var])
    )$residuals %>% as.numeric,!!paste0(dep, "_", cul_vars[2]) := lm(
      as.formula(
        glue("{dep} ~ {paste(adj_vars, collapse = '+')} + {cul_vars[1]}")
      ),
      data = df_part.plots,
      weights = pull(df_part.plots[, weight_var])
    )$residuals %>% as.numeric,!!(cul_vars[1]) := lm(
      as.formula(
        glue(
          "{cul_vars[1]} ~ {paste(adj_vars, collapse = '+')} + {cul_vars[2]}"
        )
      ),
      data = df_part.plots,
      weights = pull(df_part.plots[, weight_var])
    )$residuals %>% as.numeric,!!(cul_vars[2]) := lm(
      as.formula(
        glue(
          "{cul_vars[2]} ~ {paste(adj_vars, collapse = '+')} + {cul_vars[1]}"
        )
      ),
      data = df_part.plots,
      weights = pull(df_part.plots[, weight_var])
    )$residuals %>% as.numeric,
    country_short = df_part.plots %>% select(country_short) %>% pull
  ) %>%
    
    pivot_longer(cols = cul_vars,
                 names_to = "var",
                 values_to = "val") %>%
    rowwise %>%
    mutate(!!dep := ifelse(
      grepl(cul_vars[1], var),
      !!as.symbol(paste0(dep, "_", cul_vars[1])),
      !!as.symbol(paste0(dep, "_", cul_vars[2]))
    )) %>%
    ungroup %>%
    mutate(var = ifelse(var == cul_vars[1], cul_vars_names[1],
                        cul_vars_names[2]))
  
  p <-
    ggplot(res_part.plots, aes(
      x = val,
      y = !!as.symbol(dep),
      color = var
    ))
  if (icon == "point") {
    p <- p + geom_point(pch = 1, size = 2)
  } else{
    p <-
      p + geom_text(aes(label = country_short),
                    size = 2.5,
                    col = "grey30")
  }
  
  p +
    facet_grid(cols = vars(var), scales = "free") +
    theme_bw(13) +
    labs(x = x_label,
         y = y_label,
         title = plot_title) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      formula = y ~ x,
      mapping = aes(weight = !!as.symbol(weight_var))
    ) +

    scale_x_continuous(breaks = -2:2, limits = c(-2.1, 2.1)) +
    scale_y_continuous(breaks = -2:2, limits = c(-2.5, 2.5)) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank(),
      plot.title.position = "plot",
      legend.position = "none"
    ) +
    scale_color_manual(values = c("black", "black"))
}

plot_imp_values <- function(data, variable, axisname){

  data %>%
    left_join(flexvals,  by = "country") %>%
    left_join(indivvals, by = "country") %>%
    select(country, contains(variable)) %>%
    mutate(country = fct_reorder(country, eval(parse(text=variable)))) %>%
    pivot_longer(cols = contains(paste0(variable, "_draw")), 
                 names_to  = "var",
                 values_to = "val") %>%
    group_by(country) %>%
    summarise(!!as.symbol(paste0(variable, "_imp")) := unique(!!as.symbol(paste0(variable, "_imp"))),
              !!as.symbol(variable) := unique(!!as.symbol(variable)),
              val_p25 = quantile(val, 0.25, na.rm = TRUE),
              val_p75 = quantile(val, 0.75, na.rm = TRUE)) %>%
    ggplot() +
    geom_point(aes(y = !!as.symbol(variable), x = country, col = !!as.symbol(paste0(variable, "_imp"))), 
               size = 1.5, pch = 16) +
    geom_errorbar(aes(x = country, ymin = val_p25, ymax = val_p75, col = !!as.symbol(paste0(variable, "_imp"))), 
                  width = 0) +
    coord_flip() +
    theme_bw(14) +
    theme(axis.text.y = element_text(size = 6),
          legend.position = "bottom",
          panel.grid.major.x  = element_blank(),
          panel.grid.minor.x  = element_blank()) +
    scale_color_manual(values = c("black", "cornflowerblue")) +
    labs(x = "Country",
         y = axisname,
         col = "Imputed") +
    guides(col = guide_legend(override.aes = list(lty = 0)))
  
}
