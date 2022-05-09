############################################
#### load libraries, functions and data ####
############################################
library(tidyverse)   # v1.3.1 
library(DescTools)   # v0.99.44 
library(ggrepel)     # v0.9.1   
library(cowplot)     # v1.1.1   
library(specr)       # v0.2.1     
library(broom)       # v0.8.0     
library(patchwork)   # v1.1.1 
library(glue)        # v1.6.2         
library(mice)        # v3.14.0           
library(countrycode) # v1.3.1

source("R/helper_functions.R")
df  <- read_csv("data/data_51countries.csv", show_col_types = FALSE)
dfl <- read_csv("data/data_190countries.csv", show_col_types = FALSE)

#######################################
#### Define parameters of analysis ####
#######################################

# dependent variables
dep_vars_pool_m <- c("obesity_m", "overw_m", "BMI_m")
dep_vars_pool_f <- c("obesity_f", "overw_f", "BMI_f")

# adjustment variables full model
adj_vars_m <- c("yr_sch_m", "globaliz", "uneven_eco_dev", "gdppc_ln", "undernourish")
adj_vars_f <- c("yr_sch_f", "globaliz", "uneven_eco_dev", "gdppc_ln", "undernourish")

# additional adjustment variables (specification curves)
indep_vars_pool_m <- c("rice",
                       "mcdonalds",
                       "temp_avg",
                       "wgi_avg",
                       "share_animal_prot",
                       "vegies_fruits",
                       "hf_efiscore", 
                       "wdi_emp_agr_m",
                       "wdi_emp_ind_m",
                       "wdi_emp_ser_m",
                       "healthexp_perc_gdp",
                       "wdi_popurb_share",
                       "wdi_smok_m")

indep_vars_pool_f <- c("mcdonalds",
                       "temp_avg",
                       "wgi_avg",
                       "rice",
                       "share_animal_prot",
                       "vegies_fruits",
                       "hf_efiscore", 
                       "wdi_emp_agr_f",
                       "wdi_emp_ind_f",
                       "wdi_emp_ser_f",
                       "healthexp_perc_gdp",
                       "wdi_popurb_share",
                       "wdi_smok_f")

# cultural dimensions
cul_vars <- c("flex", "indiv")

# alternative individualism measures
indiv_vars_alt <- c("indiv_welzel",    
                    "self_expr_val", 
                    "emancip_val",              
                    "autonomy",              
                    "indiv_hofstd")

# other parameters
nspecs      <- 100 # size of random sample of specifications for specification curves
weight_var  <- "pop_log" # model weights (if applicable)
region_var  <- "region_wb" # region fixed effect (if applicable)
outlier_mad <- 3  # median absolute deviation for winsorizing (if applicable)

##############################
#### Specification curves ####
##############################

#### male ####

# sample and run model specification
set.seed(1234)
results_m <- map_dfr(as.list(1:nspecs), function(x)
  draw_and_run_single(
    df,
    dep_vars_pool_m,
    indep_vars_pool_m,
    max_add_preds = 4,
    cul_vars,
    adj_vars_m,
    weight_var,
    do_weights = "random",
    region_var,
    use_regions = "random",
    outlier_mad,
    winsor_outliers = "random",
    is_main = FALSE
  ) %>%
    mutate(specnr = x, is.main = FALSE))

# make sure there are no redundant specifications
nrow(results_m %>%
       filter(term == "flex")) -
  nrow(results_m %>%
         filter(term == "flex") %>%
         select(-specnr) %>%
         distinct) == 0

# extend set of models with full model main specification
results_m <- draw_and_run_single(
  df,
  dep_vars_pool = "obesity_m",
  indep_vars_pool = "none",
  max_add_preds = 0,
  cul_vars,
  adj_vars_m,
  weight_var,
  do_weights = TRUE,
  region_var,
  use_regions = FALSE,
  outlier_mad,
  winsor_outliers = FALSE,
  is_main = TRUE
) %>%
  mutate(specnr = nspecs + 1) %>%
  rbind(results_m)

# plot specification curve
spec_curve_m <- create_spec_curve(
  results_m,
  cul_vars,
  cul_vars_names = c("Flexibility", "Individualism"),
  main = "Male population",
  y_limits = c(-1, 1),
  nr_curves = 2
)

# build specifications summary table
spec_table_m <- spec_table(results_m %>%
                             mutate(
                               term = ifelse(
                                 term == "flex",
                                 "Flexibility (male population)",
                                 "Individualism (male population)"
                               )
                             ))

# export
png("results/figures/fig_curve_m.png", width = 3250, height = 4400, res = 350)
spec_curve_m
dev.off()
write_csv(spec_table_m, "results/tables/tab_curve_m.csv")

#### female ####

# sample and run model specification
set.seed(1234)
results_f <- map_dfr(as.list(1:nspecs), function(x)
  draw_and_run_single(
    df,
    dep_vars_pool_f,
    indep_vars_pool_f,
    max_add_preds = 4,
    cul_vars,
    adj_vars_f,
    weight_var,
    do_weights = "random",
    region_var,
    use_regions = "random",
    outlier_mad,
    winsor_outliers = "random",
    is_main = FALSE
  ) %>%
    mutate(specnr = x,
           is.main = FALSE))

# make sure there are no redundant specifications
nrow(results_f %>% 
       filter(term=="flex")) -
  nrow(results_f %>% 
         filter(term=="flex") %>% 
         select(-specnr) %>% 
         distinct) == 0

# extend set of models with full model main specification
results_f <- draw_and_run_single(
  df,
  dep_vars_pool = "obesity_f",
  indep_vars_pool = "none",
  max_add_preds = 0,
  cul_vars,
  adj_vars_f,
  weight_var,
  do_weights = TRUE,
  region_var,
  use_regions = FALSE,
  outlier_mad,
  winsor_outliers = FALSE,
  is_main = TRUE
) %>%
  mutate(specnr = nspecs + 1) %>%
  rbind(results_f)

# plot specification curve
spec_curve_f <- create_spec_curve(
  results_f,
  cul_vars,
  cul_vars_names = c("Flexibility", "Individualism"),
  main = "Female population",
  y_limits = c(-1, 1),
  nr_curves = 2
)

# build specifications summary table
spec_table_f <- spec_table(results_f %>%
                             mutate(
                               term = ifelse(
                                 term == "flex",
                                 "Flexibility (female population)",
                                 "Individualism (female population)"
                               )
                             ))

# export
png("results/figures/fig_curve_f.png", width = 3250, height = 4400, res = 350)
spec_curve_f
dev.off()
write_csv(spec_table_f, "results/tables/tab_curve_f.csv")

##########################################
#### scatter plot cultural dimensions ####
##########################################

scatt_dim_plot <- df %>%
  drop_na(obesity_m, obesity_f, flex, indiv) %>%
  rowwise %>%
  mutate(obesity_bs = mean(obesity_m, obesity_f)) %>%
  ungroup %>%
  ggplot() +
  geom_point(aes(
    x    = flex,
    y    = indiv,
    col  = gdppc_ln,
    size = obesity_bs
  )) +
  geom_text_repel(
    aes(x = flex,
        y = indiv,
        label = country),
    size = 3,
    force = 2,
    max.overlaps = 20
  ) +
  theme_bw(14) +
  theme(panel.grid = element_blank()) +
  labs(x     = "Monumentalism \u2b9c \u2b9e Flexibility",
       y     = "Collectivism \u2b9c \u2b9e Individualism",
       size  = "Obesity rate\n(BMI \u2265 30,\nin %)",
       color = "log GDP p.c.\n(USD)") +
  scale_color_gradient(low = "cadetblue1", high = "blue2")

# export
png("results/figures/fig_dimensions.png", width = 3300, height = 2500, res = 410)
scatt_dim_plot
dev.off()

###################################
#### Partial correlation plots ####
###################################

#### male ####

part_cor_plot_m <- part_cor_plot(
  df,
  icon = "country",
  dep = "obesity_m",
  adj_vars = adj_vars_m,
  cul_vars = cul_vars,
  cul_vars_names = c(
    "Flexibility (male population)",
    "Individualism (male population)"
  ),
  weight_var,
  plot_title = "b",
  x_label = "Cultural dimension (res.)",
  y_label = "Obesity rate (BMI \u2265 30) (res.)"
)

#### female ####

part_cor_plot_f <- part_cor_plot(
  df,
  icon = "country",
  dep = "obesity_f",
  adj_vars = adj_vars_f,
  cul_vars = cul_vars,
  cul_vars_names = c(
    "Flexibility (female population)",
    "Individualism (female population)"
  ),
  weight_var,
  plot_title = "a",
  x_label = "Cultural dimension (res.)",
  y_label = "Obesity rate (BMI \u2265 30) (res.)"
)

# export
png("results/figures/fit_partial.png", width = 2200, height = 2500, res = 350)
part_cor_plot_f/ part_cor_plot_m
dev.off()

############################
#### Regression results ####
############################

# define all individualism measures
indiv_vars_all <- c("indiv", indiv_vars_alt)

# define predictor pairs
cul_vars_all <- mapply(append, as.list(indiv_vars_all), "flex", SIMPLIFY = FALSE)

# run all 36 models (6 cultural predictor pairs * 2 genders * 3 adjustment sets)
regr_res <- draw_and_run_set(
  df,
  list_dep_vars_pool      = as.list(rep(c(
    "obesity_f", "obesity_m"
  ), each = 18)),
  list_indep_vars_pool    = as.list(rep("none", 36)),
  list_max_add_preds      = as.list(rep(0, 36)),
  list_cul_vars           = rep(cul_vars_all, 6),
  list_adj_vars           = rep(
    list(
      "none",
      "undernourish",
      adj_vars_f,
      "none",
      "undernourish",
      adj_vars_m
    ),
    each = 6
  ),
  list_weight_var         = as.list(rep(weight_var, 36)),
  list_do_weights         = as.list(rep(TRUE, 36)),
  list_region_var         = as.list(rep(region_var, 36)),
  list_use_regions        = as.list(rep(FALSE, 36)),
  list_outlier_mad        = as.list(rep(outlier_mad, 36)),
  list_winsorize_ourliers = as.list(rep(FALSE, 36)),
  list_is_main            = as.list(rep(TRUE, 36)),
  list_return_all_preds   = as.list(rep(TRUE, 36))
) %>%
  select(!(
    contains("c0") |
      contains("c1") | contains ("c2") | contains("is.main")
  )) %>%
  mutate(
    model = case_when(
      adj_vars == "none" ~ "Unadjusted model",
      adj_vars == "undernourish" ~ "Undernour. adjusted",
      TRUE ~ "Full model"
    ),
    gender = ifelse(grepl("f", dep_vars_pool), "Female", "Male"),
    "measure_individualism" = case_when(
      grepl("indiv \\+ flex", cul_vars) ~ "Individualism (Minkov-Hofstede)",
      grepl("indiv_welzel \\+ flex", cul_vars) ~ "Individualism (Beugelsdijk & Welzel)",
      grepl("self_expr_val \\+ flex", cul_vars) ~ "Self-expression values (Inglehart)",
      grepl("emancip_val \\+ flex", cul_vars) ~ "Emancipative values (Welzel)",
      grepl("autonomy \\+ flex", cul_vars) ~ "Autonomy (Schwartz)",
      grepl("indiv_hofstd \\+ flex", cul_vars) ~ "Individualism (Hofstede)"
    )
  ) %>%
  relocate(model, gender)

# export
write_csv(regr_res, "results/tables/tab_regr.csv")

########################################
#### Summary table of all variables ####
########################################

# define all variables needed, but remove duplicates
vars_all <- c(dep_vars_pool_m, dep_vars_pool_f, cul_vars, indiv_vars_all, 
              adj_vars_m, adj_vars_f, indep_vars_pool_m, indep_vars_pool_f)
vars_all <- vars_all[!duplicated(vars_all)]

# create raw table
descr_tab <- df %>%
  filter(!is.na(flex)) %>%
  summarise(across(
    .cols = vars_all,
    .fns = list(
      "Mean" = ~ round(mean(.x, na.rm = TRUE), 1),
      "SD"   = ~ round(sd(.x, na.rm = TRUE), 1),
      "Min"  = ~ round(min(.x, na.rm = TRUE), 1),
      "Max"  = ~ round(max(.x, na.rm = TRUE), 1),
      "N"    = ~ sum(!is.na(.x))
    ),
    .names = "{.col}.{.fn}"
  )) %>%
  pivot_longer(cols = 1:ncol(.),
               names_to = "var",
               values_to = "val") %>%
  separate(var, into = c("var", "metric"), sep = "\\.") %>%
  pivot_wider(names_from = metric, values_from = val) %>%
  rename("Variable" = "var")

# improve order and names of variables
descr_tab <- descr_tab %>% 
  mutate(Variable = factor(Variable)) %>% 
  mutate(Variable = fct_reorg(Variable,
         # dependent variables
         "Obesity rate (BMI \u2265 30) (male)" = "obesity_m", 
         "Overweight rate (BMI \u2265 25) (male)" = "overw_m",
         "Avg. BMI (male)" = "BMI_m",
         "Obesity rate (BMI \u2265 30) (female)" = "obesity_f", 
         "Overweight rate (BMI \u2265 25) (female)" = "overw_f",
         "Avg. BMI (female)" = "BMI_f",
         # cultural dimensions
         "Flexibility (Minkov-Hofstede)" = "flex",
         "Individualism (Minkov-Hofstede)" = "indiv",
         # alternative individualism measures
         "Individualism (Beugelsdijk & Welzel)" = "indiv_welzel",
         "Self-expression values (Inglehart)" = "self_expr_val",
         "Emancipative values (Welzel)" = "emancip_val",
         "Autonomy (Schwartz)" = "autonomy",
         "Individualism (Hofstede)" = "indiv_hofstd",
         # adjustment variables full model
         "log GDP per capita (in USD, PPP)" = "gdppc_ln",
         "Years of schooling (male)" = "yr_sch_m",
         "Years of schooling (female)" = "yr_sch_f",
         "Economic Globalization Index" = "globaliz",
         "Uneven Economic Development Indicator" = "uneven_eco_dev",
         "Undernourishment (in % of pop.)" = "undernourish",
         # additional adjustment variables (specification curves)
         "Avg. World Governance Indicators" = "wgi_avg",
         "Economic Freedom Index" = "hf_efiscore",
         "Urban population share" = "wdi_popurb_share",
         "Health expenditure (in % GDP)" = "healthexp_perc_gdp",
         "Avg. temperature (in degree Celcius, 1961-1990)" = "temp_avg",
         "McDonald's density (in per 100k)" = "mcdonalds",
         "Rice supply (in kg per person-year)" = "rice",
         "Vegetables/fruit supply (in kg per person-year)" = "vegies_fruits",
         "Share animal protein (in % of calories)" = "share_animal_prot",
         "Agriculture employm. share (male)" = "wdi_emp_agr_m",
         "Industry employm. share (male)" = "wdi_emp_ind_m",
         "Services employm. share (male)" = "wdi_emp_ser_m",
         "Agriculture employm. share (female)" = "wdi_emp_agr_f",
         "Industry employm. share (female)" = "wdi_emp_ind_f",
         "Services employm. share (female)" = "wdi_emp_ser_f",
         "Smoking prevalence (male)" = "wdi_smok_m",
         "Smoking prevalence (female)" = "wdi_smok_f")) %>% 
  arrange(Variable)

# export
write_csv(descr_tab, "results/tables/tab_descr.csv")

########################################################
#### Correlations between measures of individualism ####
########################################################

cors_indiv_tab <- df %>%
  select(indiv, indiv_welzel, self_expr_val,
         emancip_val, autonomy, indiv_hofstd) %>% 
  {cor(., use = "pair")} %>% as_tibble

names(cors_indiv_tab) <- c("Individualism (Minkov-Hofstede)",
                           "Individualism (Beugelsdijk & Welzel)",
                           "Self-expression values (Inglehart)",
                           "Emancipative values (Welzel)",
                           "Autonomy (Schwartz)",
                           "Individualism (Hofstede)")

cors_indiv_tab <- cors_indiv_tab %>% 
  mutate(Variable = names(cors_indiv_tab), .before = "Individualism (Minkov-Hofstede)")

# export
write_csv(cors_indiv_tab, "results/tables/tab_cors.csv")

#######################################################
#### Missingness analysis and multiple imputations ####
#######################################################

                     # create data set for multiple imputations
dfl_imp <- dfl %>%
  select(
    # variables not used for imputation
    country, 
    pop_log,
    # cultural dimensions
    flex, 
    indiv,
    # dependent variables
    obesity_f, 
    obesity_m,
    # adjustment variables full model
    adj_vars_m, 
    yr_sch_f,
    # auxiliary variables for imputation (from specification curves)
    rice,
    wgi_avg,
    # auxiliary variables for imputation  (from alternative individualism measures)
    indiv_welzel,
    self_expr_val,
    # auxiliary variables for imputation (newly added)
    fi_ftradeint,
    fh_pair,
    cool_water_ind,
    irrig_dep,
    migr_dist_ea,
    secular_rational,
    culture_zone
  ) %>%
  # count missings for each country
  rowwise %>%
  mutate(sum_NA = sum(across(.cols = c(
    adj_vars_m,
    yr_sch_f,
    flex,
    indiv,
    obesity_f,
    obesity_m,
    rice,
    wgi_avg,
    indiv_welzel,
    self_expr_val,
    fi_ftradeint,
    fh_pair,
    cool_water_ind,
    irrig_dep,
    migr_dist_ea,
    secular_rational,
    culture_zone
  ),
  ~is.na(.x)))) %>%
  ungroup %>%
  # set limit to maximum 14 of 21 missing, i.e. not more than 1/3 missing ( 155 countries remain)
  filter(sum_NA <= 7) %>%  
  select(-sum_NA) %>%      
  mutate(culture_zone = factor(culture_zone))

# table with proportion missing
tab_missings <- dfl_imp %>%
  select(-country, -pop_log) %>%
  summarise(across(.cols = everything(), ~
                     paste0(round(mean(is.na(.x))*100,0), "% (", sum(is.na(.x)), "/", n(), ")")
  )) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "% missing values") %>% 
  
  mutate(Variable = factor(Variable)) %>% 
  mutate(Variable = fct_reorg(Variable,
                              "Obesity rate (BMI \u2265 30) (male)" = "obesity_m", 
                              "Obesity rate (BMI \u2265 30) (female)" = "obesity_f", 
                              "Flexibility (Minkov-Hofstede)" = "flex",
                              "Individualism (Minkov-Hofstede)" = "indiv",
                              "Individualism (Beugelsdijk & Welzel)" = "indiv_welzel",
                              "Self-expression values (Inglehart)" = "self_expr_val",
                              "log GDP per capita (in USD, PPP)" = "gdppc_ln",
                              "Years of schooling (male)" = "yr_sch_m",
                              "Years of schooling (female)" = "yr_sch_f",
                              "Economic Globalization Index" = "globaliz",
                              "Uneven Economic Development Indicator" = "uneven_eco_dev",
                              "Undernourishment (in % of pop.)" = "undernourish",
                              "Avg. World Governance Indicators" = "wgi_avg",
                              "Rice supply (in kg per person-year)" = "rice",
                              "Freedom to Trade Internationally" = "fi_ftradeint",
                              "Personal Autonomy and Individual Rights" = "fh_pair",
                              "Cool Water Index" = "cool_water_ind",
                              "Irrigation dependence" = "irrig_dep",
                              "Migration distance from East Africa" = "migr_dist_ea",
                              "Rational-secular values (Inglehart)" = "secular_rational",
                              "Cultural regions" = "culture_zone"
                              ))

write_csv(tab_missings, "results/tables/tab_missings.csv")

# plot predicted missingness
p_miss_gdp <- dfl_imp %>% 
  mutate(missing_flex = ifelse(ici(flex) == TRUE, 1, 0)) %>% 
  ggplot() + 
  geom_jitter(aes(x = gdppc_ln, y = missing_flex), alpha = 0.5, height = 0.03) + 
  theme_bw(11) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0,1), labels = c(0,1)) + 
  geom_smooth(aes(x = gdppc_ln, y = missing_flex), method = "glm", 
              method.args = list(family = "binomial"),
              col = "black") + 
  labs(y = "Probability of missing data on flexibility",
       x = "log GDP per capita (USD)") +
  theme(panel.grid = element_blank())

p_miss_glob <- dfl_imp %>% 
  mutate(missing_flex = ifelse(ici(flex) == TRUE, 1, 0)) %>% 
  ggplot() + 
  geom_jitter(aes(x = globaliz, y = missing_flex), alpha = 0.5, height = 0.03) + 
  theme_bw(11) + 
  scale_y_continuous(limits = c(0,1), breaks = c(0,1), labels = c(0,1)) +  
  geom_smooth(aes(x = globaliz, y = missing_flex), method = "glm",
              method.args = list(family = "binomial"),
              col = "black") + 
  labs(y = "Probability of missing data on flexibility",
       x = "Economic Globalization Index") +
  theme(panel.grid = element_blank())

png("results/figures/fig_missingness.png", res = 210, width = 1500, height = 700)
p_miss_gdp + p_miss_glob
dev.off()

# run multiple imputations algorithms
pred_matrix       <- matrix(1,ncol(dfl_imp),ncol(dfl_imp)) # define variables to be used for imputation
diag(pred_matrix) <- pred_matrix[,1] <- pred_matrix[,2] <- pred_matrix[1,] <- pred_matrix[2,] <- 0

# predictice mean matching imputation
set.seed(1234)
im_pmm <- mice(dfl_imp,
               m = 20,
               maxit = 10,
               defaultMethod = "pmm",
               print = FALSE,
               ls.meth = "ridge",
               ridge = 0.001,
               predictorMatrix = pred_matrix)

# bayesian normal regression imputation
set.seed(1234)
im_bar <- mice(dfl_imp,
               m = 20,
               maxit = 10,
               defaultMethod = "norm",
               print = FALSE,
               ls.meth = "ridge",
               ridge = 0.001,
               predictorMatrix = pred_matrix)

# run full model for both methods and genders
fullm_im_pmm_male   <- with(im_pmm, 
                            lm(as.formula(paste0("obesity_m ~", paste(c(cul_vars, adj_vars_m), collapse = "+"))),
                               weights = pop_log))

fullm_im_pmm_female <- with(im_pmm, 
                            lm(as.formula(paste0("obesity_f ~", paste(c(cul_vars, adj_vars_f), collapse = "+"))),
                               weights = pop_log))

fullm_im_bar_male   <- with(im_bar, 
                            lm(as.formula(paste0("obesity_m ~", paste(c(cul_vars, adj_vars_m), collapse = "+"))),
                               weights = pop_log))

fullm_im_bar_female <- with(im_bar, 
                            lm(as.formula(paste0("obesity_f ~", paste(c(cul_vars, adj_vars_f), collapse = "+"))),
                               weights = pop_log))

tab_imput <- map_dfr(list("PMM imputation male"   = fullm_im_pmm_male,
                          "PMM imputation female" = fullm_im_pmm_female ,
                          "BAR imputation male"   = fullm_im_bar_male   ,
                          "BAR imputation female" = fullm_im_bar_female), 
                     .id = "method", function(x){
           as_tibble(summary(pool(x))) %>% filter(term %in% c("indiv", "flex"))
         })

# export
write_csv(tab_imput, "results/tables/tab_imput.csv")

                     # create dot charts showing observed and imputed values
median_flex  <- apply(im_bar$imp$flex, 1, median)
median_indiv <- apply(im_bar$imp$indiv, 1, median)

dfl_imp$flex_imp <- dfl_imp$indiv_imp              <- "No"
dfl_imp$flex_imp[as.numeric(names(median_flex))]   <- "Yes"
dfl_imp$indiv_imp[as.numeric(names(median_indiv))] <- "Yes"

dfl_imp$flex[as.numeric(names(median_flex))]   <- median_flex
dfl_imp$indiv[as.numeric(names(median_indiv))] <- median_indiv

flexvals  <- tibble(row = rownames(im_bar$imp$flex),  im_bar$imp$flex)
indivvals <- tibble(row = rownames(im_bar$imp$indiv), im_bar$imp$indiv)

names(flexvals)[2:ncol(flexvals)]   <- paste0("flex_draw_",names(flexvals)[2:ncol(flexvals)])
names(indivvals)[2:ncol(indivvals)] <- paste0("indiv_draw_",names(indivvals)[2:ncol(indivvals)])

flexvals$country  <- dfl_imp$country[as.numeric(flexvals$row)]
indivvals$country <- dfl_imp$country[as.numeric(indivvals$row)]

p_imp_flex  <- plot_imp_values(dfl_imp, "flex", "Monumentalism \u2b9c \u2b9e Flexibility")
p_imp_indiv <- plot_imp_values(dfl_imp, "indiv", "Collectivism \u2b9c \u2b9e Individualism")

# export
png("results/figures/fig_fleximputed.png", res = 300, width = 1600, height = 3200)
p_imp_flex
dev.off()
png("results/figures/fig_indivimputed.png", res = 300, width = 1600, height = 3200)
p_imp_indiv
dev.off()

# create world maps showing available, imputed and still missing data
map_world <- map_data(map = "world")
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
maptheme  <- theme(axis.text = element_blank(), 
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   legend.position = "bottom",
                   panel.grid = element_blank())

map_world$country <- countrycode(map_world %>% 
                                   mutate(region = ifelse(region == "Micronesia",
                                                          "Micronesia (Federated States of)",
                                                          region)) %>% 
                                   pull(region), "country.name", "country.name")

map_world <- map_world %>% 
  left_join(dfl_imp %>% select(country, flex_imp, indiv_imp),
            by = "country")

map_world <- map_world %>% 
  mutate(pattern_flex = 
           case_when(flex_imp == "No" ~ "Observed",
                     flex_imp == "Yes" ~ "Imputed",
                     TRUE ~ "Insufficient data"),
         pattern_indiv = 
           case_when(indiv_imp == "No" ~ "Observed",
                     indiv_imp == "Yes" ~ "Imputed",
                     TRUE ~ "Insufficient data")) %>% 
  mutate(across(contains("pattern"), function(x){
    
    factor(x, ordered = TRUE, levels = c("Observed", "Imputed", "Insufficient data"))
    
  }))

map_data_flex <- ggplot(map_world, 
                        aes(x = long, y = lat, group = group, fill = pattern_flex)) + 
  geom_polygon(color = "black", lwd = 0.25) + mapcoords + maptheme + 
  scale_fill_manual(values = c("#b3e2cd", "#cbd5e8", "#fdcdac")) + 
  labs(fill = "",
       subtitle = "Flexibility") +
  theme(legend.position = "none") 

map_data_indiv <- ggplot(map_world, 
                         aes(x = long, y = lat, group = group, fill = pattern_indiv)) + 
  geom_polygon(color = "black", lwd = 0.25) + mapcoords + maptheme + 
  scale_fill_manual(values = c("#b3e2cd", "#cbd5e8", "#fdcdac")) + 
  labs(fill = "",
       subtitle = "Individualism")

# export
png("results/figures/fig_datamap.png", width = 2000, height = 2000, res = 290)
map_data_flex/map_data_indiv
dev.off()

