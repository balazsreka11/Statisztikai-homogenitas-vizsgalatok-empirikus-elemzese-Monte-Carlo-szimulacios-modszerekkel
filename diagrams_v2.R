library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(readxl)
library(skedastic)
library(tibble)
library(lmtest)
library(sandwich)
library(car)
library(knitr)
library(openxlsx)

showtext_auto()

# Common theme for all plots --------------------------------------------------

custom_theme <- theme_minimal(base_size = 14) +
  theme(
    text = element_text(
      family = "serif"
    ),
    axis.title = element_text(
      size = 16
    ),
    axis.text = element_text(
      size = 14
    ),
    legend.title = element_text(
      size = 14
    ),
    legend.text = element_text(
      size = 18
    ),
    strip.text = element_text(
      size = 14
    ),
    plot.title = element_text(
      size = 16,
      hjust = 0.5
    )
  )



# Global power functions --------------------------------------------------------------

# Load the simulation results summary
simulation_results_summary <- read.csv(
  "simulation_results_summary.csv"
)

# Keep only pairs with different distributions
df_diff <- simulation_results_summary %>%
  filter(Distribution1 != Distribution2)

# Calculate power at the 5% and 1% significance levels
# The summary file already contains the rejection rates,
# so we average them across the different distribution pairs.
power_df <- df_diff %>%
  group_by(Test, SampleSize) %>%
  summarise(
    `Power (5%)` = mean(Decision5Rate, na.rm = TRUE),
    `Power (1%)` = mean(Decision1Rate, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to long format
power_df <- power_df %>%
  pivot_longer(
    cols = c(`Power (5%)`, `Power (1%)`),
    names_to = "Significance Level",
    values_to = "Power"
  )


# Global power function at the 5% level

p5 <- power_df %>%
  filter(`Significance Level` == "Power (5%)") %>%
  ggplot(
    aes(
      x = SampleSize,
      y = Power,
      color = Test,
      group = Test
    )
  ) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  labs(
    x = "Sample size (n)",
    y = "Power (5%)",
    color = "Test"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  custom_theme +
  theme(
    legend.position = "right"
  )

print(p5)

# Global power function at the 1% level 
p1 <- power_df %>%
  filter(`Significance Level` == "Power (1%)") %>%
  ggplot(
    aes(
      x = SampleSize,
      y = Power,
      color = Test,
      group = Test
    )
  ) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  labs(
    x = "Sample size (n)",
    y = "Power (1%)",
    color = "Test"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  custom_theme +
  theme(
    legend.position = "right"
  )

print(p1)


# Power functions within the same distribution types ----------------------------------------

# Select only pairs with different distributions and reshape the data
power_df2 <- simulation_results_summary %>%
  filter(Distribution1 != Distribution2) %>%
  select(
    Group1,
    Group2,
    Test,
    SampleSize,
    Decision5Rate,
    Decision1Rate
  ) %>%
  pivot_longer(
    cols = c(Decision5Rate, Decision1Rate),
    names_to = "Decision",
    values_to = "Power"
  ) %>%
  mutate(
    Alpha = ifelse(
      Decision == "Decision5Rate",
      0.05,
      0.01
    )
  )

# Keep only distribution pairs belonging to the same distribution group
power_within <- power_df2 %>%
  filter(Group1 == Group2) %>%
  transmute(
    Group = Group1,
    Test,
    SampleSize,
    Alpha,
    Power
  )

# Calculate average power by group, test, sample size and significance level
power_within <- power_within %>%
  group_by(Group, Test, SampleSize, Alpha) %>%
  summarise(
    Power = mean(Power, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Group, Test, Alpha, SampleSize)

# Define the desired order of the distribution groups
plot_power_by_group <- function(dat, alpha_value) {
  
  groups_order <- c(
    "(-infinity;+infinity) symmetric",
    "(-infinity;+infinity) asymmetric",
    "(0;+infinity)",
    "(0;1)",
    "Normal mixture"
  )
  
  dat %>%
    filter(Alpha == alpha_value) %>%
    mutate(
      Group = factor(
        Group,
        levels = groups_order
      )
    ) %>%
    ggplot(
      aes(
        x = SampleSize,
        y = Power,
        color = Test,
        group = Test
      )
    ) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    facet_wrap(
      ~ Group,
      nrow = 2
    ) +
    coord_cartesian(
      ylim = c(0, 1)
    ) +
    labs(
      x = "Sample size (n)",
      y = expression("Power (" * 1 - beta * ")"),
      color = "Test"
    ) +
    custom_theme +
    theme(
      legend.position = "bottom"
    ) +
    guides(
      color = guide_legend(nrow = 2)
    )
}

# Create the 5% power plot
p5 <- plot_power_by_group(
  power_within,
  0.05
)

# Create the 1% power plot
p1 <- plot_power_by_group(
  power_within,
  0.01
)

# Display the plots
print(p5)
print(p1)


# Power functions between distribution groups ------------------------------------------------

# Define the order of the distribution groups
groups_order <- c(
  "(-infinity;+infinity) symmetric",
  "(-infinity;+infinity) asymmetric",
  "(0;+infinity)",
  "(0;1)",
  "Normal mixture"
)

# Create a standardized name for each pair of groups
pair_str <- function(a, b) {
  paste(sort(c(a, b)), collapse = "  vs  ")
}

# Create all possible pairs of distribution groups
all_pairs <- {
  ap <- character()
  
  for (i in seq_along(groups_order)) {
    for (j in (i + 1):length(groups_order)) {
      if (!is.na(j)) {
        ap <- c(
          ap,
          pair_str(groups_order[i], groups_order[j])
        )
      }
    }
  }
  
  ap
}

# Plot power functions between distribution groups
plot_between_groups <- function(alpha_value) {
  
  between <- power_df2 %>%
    filter(
      Alpha == alpha_value,
      Group1 != Group2
    ) %>%
    mutate(
      `Group pair` = mapply(
        pair_str,
        Group1,
        Group2
      )
    ) %>%
    group_by(
      `Group pair`,
      Test,
      SampleSize,
      Alpha
    ) %>%
    summarise(
      Power = mean(Power, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Keep only group pairs that are present in the data
  present_pairs <- all_pairs[
    all_pairs %in% unique(between$`Group pair`)
  ]
  
  if (length(present_pairs) == 0) {
    stop("No valid group pairs found.")
  }
  
  between <- between %>%
    mutate(
      `Group pair` = factor(
        `Group pair`,
        levels = present_pairs
      ),
      Test = factor(Test)
    ) %>%
    arrange(
      `Group pair`,
      Test,
      SampleSize
    )
  
  ggplot(
    between,
    aes(
      x = SampleSize,
      y = Power,
      color = Test,
      group = Test
    )
  ) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    facet_wrap(
      ~ `Group pair`,
      nrow = 3,
      ncol = 4,
      scales = "fixed"
    ) +
    coord_cartesian(
      ylim = c(0, 1)
    ) +
    labs(
      x = "Sample size (n)",
      y = expression("Power (" * 1 - beta * ")"),
      color = "Test"
    ) +
    custom_theme +
    theme(
      legend.position = "bottom"
    ) +
    guides(
      color = guide_legend(nrow = 2)
    )
}

# Create the 5% power plot
p5 <- plot_between_groups(0.05)

# Create the 1% power plot
p1 <- plot_between_groups(0.01)

# Display the plots
print(p5)
print(p1)



# Density plots ------------------------------------------------------------

# Density function of the mean P-value for different statistical tests
plot_pmean_density <- function(dat, n, same = TRUE, adjust = 3, bw = NULL) {
  
  # Filter by sample size and H0/H1
  sub <- dat %>%
    dplyr::filter(
      SampleSize == n,
      (Distribution1 == Distribution2) == same
    ) %>%
    dplyr::filter(!is.na(MeanPValue))
  
  if (nrow(sub) < 2) {
    stop("Too few observations for density estimation with this filter.")
  }
  
  x <- sub$MeanPValue
  
  bw_use <- if (!is.null(bw)) {
    bw
  } else {
    b <- tryCatch(
      stats::bw.nrd0(x),
      error = function(e) NA_real_
    )
    
    if (!is.finite(b) || b <= 0) 0.05 else b
  }
  
  # Draw density curves by test
  ggplot(
    sub,
    aes(
      x = MeanPValue,
      color = Test
    )
  ) +
    geom_density(
      linewidth = 0.6,
      adjust = adjust,
      bw = bw_use,
      trim = TRUE
    ) +
    
    # Add reference lines for the 1% and 5% significance levels
    geom_vline(
      xintercept = c(0.01, 0.05),
      linetype = "dashed",
      color = c("red", "blue"),
      linewidth = 0.5
    ) +
    
    coord_cartesian(
      xlim = c(0, 1)
    ) +
    
    labs(
      x = "Mean P-value",
      y = "Density",
      color = "Test"
    ) +
    
    custom_theme +
    
    theme(
      legend.position = "bottom"
    ) +
    
    guides(
      color = guide_legend(nrow = 2)
    )
}


# H0: identical distributions
p_h0_mean <- plot_pmean_density(
  simulation_results_summary,
  1000,
  same = TRUE
)

# H1: different distributions
p_h1_mean <- plot_pmean_density(
  simulation_results_summary,
  100,
  same = FALSE
)

print(p_h0_mean)
print(p_h1_mean)

# Density function of the mean P-value between two distribution groups -------------------

plot_pmean_density_between_groups <- function(
    dat,
    group_a,
    group_b,
    n = 100,
    adjust = 3,
    bw = 0.02,
    xlim = c(0, 1)
) {
  
  # Check whether the specified groups exist
  groups_all <- union(
    dat$Group1,
    dat$Group2
  )
  
  if (!group_a %in% groups_all || !group_b %in% groups_all) {
    stop("The specified group(s) are not present in Group1 or Group2.")
  }
  
  # Filter by sample size, group pair and H1
  sub <- dat %>%
    dplyr::filter(
      SampleSize == n,
      !is.na(MeanPValue),
      (
        (Group1 == group_a & Group2 == group_b) |
          (Group1 == group_b & Group2 == group_a)
      ),
      Distribution1 != Distribution2
    )
  
  if (nrow(sub) == 0) {
    stop("No data found for the specified conditions.")
  }
  
  # Automatically detect all tests present in the data
  tests <- unique(sub$Test)
  
  # Draw density curves for all available tests
  ggplot(
    sub,
    aes(
      x = MeanPValue,
      color = Test
    )
  ) +
    geom_density(
      linewidth = 0.6,
      adjust = adjust,
      bw = bw,
      trim = TRUE
    ) +
    
    
    # Add reference lines for the 1% and 5% significance levels
    geom_vline(
      xintercept = c(0.01, 0.05),
      linetype = "dashed",
      color = c("#D62828", "#5FA8D3"),
      linewidth = 0.5
    ) +
    
    coord_cartesian(
      xlim = xlim
    ) +
    
    labs(
      x = "Mean P-value",
      y = "Density",
      color = "Test"
    ) +
    
    custom_theme +
    
    theme(
      legend.position = "bottom"
    ) +
    
    guides(
      color = guide_legend(
        nrow = 2
      )
    )
}


# Example: density plot for two specified distribution groups

p_between_groups <- plot_pmean_density_between_groups(
  simulation_results_summary,
  group_a = "(0;1)",
  group_b = "(0;1)",
  n = 600
)

print(p_between_groups)


# Check the available distribution groups and tests 
unique(simulation_results_summary$Group1)
unique(simulation_results_summary$Test)

# Heatmap ----------------------------------------------------------------------

# Heatmap of mean P-values by distribution pairs
plot_p_heatmap <- function(
    dat,
    n,
    test,
    column
) {
  
  if (!column %in% names(dat)) {
    stop(
      paste0(
        "Column not found in the data: ",
        column
      )
    )
  }
  
  # Filter by sample size and test
  sub <- dat %>%
    filter(
      SampleSize == n,
      Test == test,
      !is.na(.data[[column]])
    ) %>%
    select(
      Distribution1,
      Distribution2,
      value = all_of(column)
    )
  
  if (nrow(sub) == 0) {
    stop("No data found for the specified conditions.")
  }
  
  # Set a consistent order for the distributions
  lev <- sort(
    unique(
      c(
        sub$Distribution1,
        sub$Distribution2
      )
    )
  )
  
  sub <- sub %>%
    mutate(
      Distribution1 = factor(
        Distribution1,
        levels = lev
      ),
      Distribution2 = factor(
        Distribution2,
        levels = lev
      )
    )
  
  # Draw the heatmap
  ggplot(
    sub,
    aes(
      x = Distribution1,
      y = Distribution2,
      fill = value
    )
  ) +
    geom_tile(
      color = "grey80"
    ) +
    
    scale_fill_gradient(
      name = column,
      low = "white",
      high = "#D62828",
      limits = c(0, 1)
    ) +
    
    coord_fixed() +
    
    labs(
      x = "Distribution 1",
      y = "Distribution 2"
    ) +
    
    custom_theme +
    
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
}

p_heatmap <- plot_p_heatmap(
  simulation_results_summary,
  n = 100,
  test = "Wasserstein",
  column = "MeanPValue"
)

print(p_heatmap)

unique(simulation_results_summary$Test)


# Distributions file----------------------------------------------------------------------

# distributions.xlsx
distributions <- read_excel("distributions.xlsx")

sample_sizes <- c(10, 50, 100, 150, 200, 250)

sim <- simulation_results_summary %>%
  filter(SampleSize %in% sample_sizes)

p_values <- sim %>%
  select(
    Distribution1,
    Distribution2,
    Test,
    SampleSize,
    MeanPValue
  ) %>%
  mutate(
    column_name = paste0(
      Test,
      "_p_n",
      SampleSize
    )
  ) %>%
  select(
    Distribution1,
    Distribution2,
    column_name,
    MeanPValue
  ) %>%
  pivot_wider(
    names_from = column_name,
    values_from = MeanPValue
  )

distributions_with_tests <- distributions %>%
  left_join(
    p_values,
    by = c("Distribution1", "Distribution2")
  )

cols_to_numeric <- c(
  "Skewness difference",
  "Kurtosis difference",
  "Mode difference"
)

distributions_with_tests[cols_to_numeric] <- lapply(
  distributions_with_tests[cols_to_numeric],
  function(x) {
    x <- as.character(x)
    x[x == "none"] <- NA
    as.numeric(x)
  }
)

numeric_df <- distributions_with_tests[sapply(distributions_with_tests, is.numeric)]

# Person-Correlation Matrix ------------------------------------------------------------------

corr_matrix <- cor(
  numeric_df,
  use = "complete.obs",
  method = "pearson"
)

print(corr_matrix)


plot_corr_matrix <- function(df, n) {
  
  p_cols <- grep(
    paste0("_p_n", n, "$"),
    names(df),
    value = TRUE
  )
  
  corr_cols <- c(
    "Overlap",
    "Median difference",
    "Skewness difference",
    "Kurtosis difference",
    "Mode difference",
    p_cols
  )
  
  corr_cols <- corr_cols[corr_cols %in% names(df)]
  
  numeric_df <- df[corr_cols]
  
  corr_matrix <- cor(
    numeric_df,
    use = "complete.obs",
    method = "pearson"
  )
  
  melt_corr <- reshape2::melt(corr_matrix)
  
  ggplot(
    melt_corr,
    aes(Var1, Var2, fill = value)
  ) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#5FA8D3",
      high = "#D62828",
      mid = "white",
      midpoint = 0,
      limit = c(-1, 1),
      name = "Correlation"
    ) +
    geom_text(
      aes(label = round(value, 2)),
      size = 5
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(
        size = 16,
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.text.y = element_text(
        size = 16
      ),
      legend.title = element_text(
        size = 17
      ),
      legend.text = element_text(
        size = 16
      ),
      plot.title = element_text(
        size = 20,
        hjust = 0.5
      )
    ) +
    labs(
      x = "",
      y = "",
      title = paste("Correlation Matrix - n =", n)
    )
}

plot_corr_matrix(distributions_with_tests, 100)


# Boxplot function ------------------------------------------------------------

plot_pvalue_boxplots <- function(df, n, grouping) {
  
  # Select p-value columns corresponding to the given sample size
  p_cols <- grep(
    paste0("_p_n", n, "$"),
    names(df),
    value = TRUE
  )
  
  # Select the grouping variable and p-value columns
  p_df <- df %>%
    select(
      all_of(grouping),
      all_of(p_cols)
    ) %>%
    pivot_longer(
      cols = -all_of(grouping),
      names_to = "Test",
      values_to = "P_value"
    )
  
  # Convert the grouping variable into a factor
  p_df[[grouping]] <- factor(
    p_df[[grouping]],
    levels = c(FALSE, TRUE),
    labels = c("False", "True")
  )
  
  # Create the boxplot
  ggplot(
    p_df,
    aes(
      x = .data[[grouping]],
      y = P_value,
      fill = .data[[grouping]]
    )
  ) +
    geom_boxplot(
      alpha = 0.7,
      outlier.alpha = 0.3
    ) +
    facet_wrap(
      ~ Test,
      scales = "free_y"
    ) +
    scale_fill_manual(
      values = c(
        "False" = "#5FA8D3",
        "True" = "#D62828"
      )
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 17),
      axis.title = element_text(size = 19),
      strip.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 17),
      plot.title = element_text(
        size = 22,
        hjust = 0.5
      )
    ) +
    labs(
      x = grouping,
      y = "P-value",
      title = paste("P-value distributions - n =", n)
    )
}

plot_pvalue_boxplots(
  distributions_with_tests,
  n = 100,
  grouping = "Same support"
)


# Linear regression models ------------------------------------------------------------

run_lm_models <- function(df, n) {
  
  # Select p-value columns corresponding to the given sample size
  p_cols <- grep(
    paste0("_p_n", n, "$"),
    names(df),
    value = TRUE
  )
  
  # Convert logical variables to factors
  df$`Same support` <- as.factor(df$`Same support`)
  df$`Same group` <- as.factor(df$`Same group`)
  
  # Variables used as predictors
  predictors <- paste(
    "`Overlap`",
    "`Median difference`",
    "`Same support`",
    "`Same group`",
    "`Skewness difference`",
    "`Kurtosis difference`",
    "`Mode difference`",
    sep = " + "
  )
  
  # Fit a linear model for each test
  models <- lapply(
    p_cols,
    function(test_col) {
      
      formula <- as.formula(
        paste0(
          "`", test_col, "` ~ ",
          predictors
        )
      )
      
      lm(
        formula,
        data = df
      )
    }
  )
  
  # Name models according to the corresponding test
  names(models) <- p_cols
  
  # Print model summaries
  for (test_name in names(models)) {
    cat("\n\n========================================\n")
    cat("Test:", test_name, "\n")
    cat("========================================\n\n")
    print(summary(models[[test_name]]))
  }
  
  return(models)
}

models_n10  <- run_lm_models(distributions_with_tests, 10)
models_n50  <- run_lm_models(distributions_with_tests, 50)
models_n100  <- run_lm_models(distributions_with_tests, 100)
models_n150 <- run_lm_models(distributions_with_tests, 150)
models_n200 <- run_lm_models(distributions_with_tests, 200)
models_n250 <- run_lm_models(distributions_with_tests, 250)

# White test for heteroskedasticity ------------------------------------------------

white_h0_results <- list()

for (n in sample_sizes) {
  
  # Select p-value columns for the given sample size
  p_cols <- grep(
    paste0("_p_n", n, "$"),
    names(distributions_with_tests),
    value = TRUE
  )
  
  # Convert logical variables to factors
  df_n <- distributions_with_tests
  
  df_n$`Same support` <- as.factor(df_n$`Same support`)
  df_n$`Same group` <- as.factor(df_n$`Same group`)
  
  # Predictors
  predictors <- paste(
    "`Overlap`",
    "`Median difference`",
    "`Same support`",
    "`Same group`",
    "`Skewness difference`",
    "`Kurtosis difference`",
    "`Mode difference`",
    sep = " + "
  )
  
  # Run White test for every test
  for (test_col in p_cols) {
    
    formula <- as.formula(
      paste0("`", test_col, "` ~ ", predictors)
    )
    
    model <- lm(
      formula,
      data = df_n
    )
    
    white_test <- white(
      model,
      interactions = FALSE
    )
    
    p_value <- white_test$p.value
    
    white_h0_results[[length(white_h0_results) + 1]] <- tibble(
      SampleSize = n,
      Test = test_col,
      White_p_value = p_value
    )
  }
}


# Combine all results
white_h0_results <- bind_rows(white_h0_results)


# Keep only cases where H0 is NOT rejected at the 5% level
white_h0_5pct <- white_h0_results %>%
  filter(White_p_value >= 0.05) %>%
  mutate(
    White_p_value = round(White_p_value, 4)
  )


# Print results
print(white_h0_5pct)

# Robust coefficient tests using HC3 standard errors -----------------------------------

robust_tests <- function(models) {
  
  results <- lapply(
    names(models),
    function(name) {
      
      model <- models[[name]]
      
      result <- tryCatch(
        {
          coeftest(
            model,
            vcov = vcovHC(
              model,
              type = "HC3"
            )
          )
        },
        error = function(e) {
          return(
            paste("Error:", e$message)
          )
        }
      )
      
      return(result)
    }
  )
  
  names(results) <- names(models)
  
  return(results)
}

robust_n10  <- robust_tests(models_n10)
robust_n50  <- robust_tests(models_n50)
robust_n100 <- robust_tests(models_n100)
robust_n150 <- robust_tests(models_n150)
robust_n200 <- robust_tests(models_n200)
robust_n250 <- robust_tests(models_n250)

# Unified regression tables ---------------------------------------------------

# Collect the models and the robust (HC3) coefficient tests by sample size
models_by_n <- list(
  "10"  = models_n10,
  "50"  = models_n50,
  "100" = models_n100,
  "150" = models_n150,
  "200" = models_n200,
  "250" = models_n250
)

robust_by_n <- list(
  "10"  = robust_n10,
  "50"  = robust_n50,
  "100" = robust_n100,
  "150" = robust_n150,
  "200" = robust_n200,
  "250" = robust_n250
)

# Significance stars following the summary.lm convention
signif_stars <- function(p_value) {
  
  if (is.na(p_value)) {
    return("")
  }
  
  if (p_value < 0.001) return("***")
  if (p_value < 0.01)  return("**")
  if (p_value < 0.05)  return("*")
  if (p_value < 0.1)   return(".")
  
  ""
}

# Format a coefficient together with its significance stars
format_coefficient <- function(estimate, p_value, digits = 4) {
  
  if (is.na(estimate)) {
    return(NA_character_)
  }
  
  paste0(
    formatC(
      estimate,
      format = "f",
      digits = digits
    ),
    signif_stars(p_value)
  )
}

# Format a plain number
format_number <- function(x, digits = 4) {
  
  if (is.na(x)) {
    return(NA_character_)
  }
  
  formatC(
    x,
    format = "f",
    digits = digits
  )
}

# Build one unified table: tests in the columns, coefficients and
# diagnostics in the rows
create_unified_table <- function(
    models,
    robust_results,
    digits = 4,
    robust_f = FALSE
) {
  
  # Row labels of the coefficient block
  variables <- c(
    "(Intercept)",
    "Overlap",
    "Median difference",
    "Same support",
    "Same group",
    "Skewness difference",
    "Kurtosis difference",
    "Mode difference"
  )
  
  # Test names taken from the p-value column names
  test_names <- sub(
    "_p_n[0-9]+$",
    "",
    names(models)
  )
  
  # Match a row label to the coefficient name used by lm
  # (numeric terms are backquoted, factor terms also carry the level)
  get_row <- function(label, row_names) {
    
    if (label == "(Intercept)") {
      return("(Intercept)")
    }
    
    matches <- grep(
      label,
      row_names,
      fixed = TRUE,
      value = TRUE
    )
    
    if (length(matches) == 1) {
      return(matches)
    }
    
    NA_character_
  }
  
  # Build one column per test
  columns <- lapply(
    seq_along(models),
    function(i) {
      
      model <- models[[i]]
      robust_model <- robust_results[[i]]
      
      # Coefficients with stars based on the HC3 robust p-values
      if (is.character(robust_model)) {
        
        coefficient_cells <- rep(
          NA_character_,
          length(variables)
        )
        
      } else {
        
        row_names <- rownames(robust_model)
        
        coefficient_cells <- vapply(
          variables,
          function(variable) {
            
            row <- get_row(variable, row_names)
            
            if (is.na(row)) {
              return(NA_character_)
            }
            
            format_coefficient(
              robust_model[row, "Estimate"],
              robust_model[row, "Pr(>|t|)"],
              digits
            )
          },
          character(1)
        )
      }
      
      model_summary <- summary(model)
      
      # R-squared
      r_squared <- model_summary$r.squared
      adj_r_squared <- model_summary$adj.r.squared
      
      # Global F-test p-value
      if (robust_f) {
        
        # Wald test with HC3 covariance matrix
        f_p_value <- tryCatch(
          {
            lmtest::waldtest(
              model,
              vcov = function(m) vcovHC(m, type = "HC3")
            )$`Pr(>F)`[2]
          },
          error = function(e) NA_real_
        )
        
      } else {
        
        fstat <- model_summary$fstatistic
        
        f_p_value <- if (is.null(fstat)) {
          NA_real_
        } else {
          unname(
            pf(
              fstat[1],
              fstat[2],
              fstat[3],
              lower.tail = FALSE
            )
          )
        }
      }
      
      # White-test p-value
      white_p_value <- tryCatch(
        {
          skedastic::white(
            model,
            interactions = FALSE
          )$p.value
        },
        error = function(e) NA_real_
      )
      
      unname(
        c(
          coefficient_cells,
          "",
          format_number(r_squared, digits),
          format_number(adj_r_squared, digits),
          format_number(f_p_value, digits),
          format_number(white_p_value, digits),
          as.character(nobs(model))
        )
      )
    }
  )
  
  names(columns) <- test_names
  
  # Row labels of the full table
  row_labels <- c(
    variables,
    "",
    "R-squared",
    "Adjusted R-squared",
    "Global F-test p-value",
    "White-test p-value",
    "Observations"
  )
  
  result <- data.frame(
    Variable = row_labels,
    columns,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Keep the test order identical across sample sizes
  result[, c("Variable", sort(test_names))]
}

# Create one table per sample size
regression_tables <- lapply(
  sample_sizes,
  function(n) {
    
    create_unified_table(
      models_by_n[[as.character(n)]],
      robust_by_n[[as.character(n)]]
    )
  }
)

names(regression_tables) <- paste0("n = ", sample_sizes)

# Print the tables
for (sheet_name in names(regression_tables)) {
  
  cat("\n\n========================================\n")
  cat("Regression table -", sheet_name, "\n")
  cat("========================================\n\n")
  
  print(
    knitr::kable(
      regression_tables[[sheet_name]],
      row.names = FALSE
    )
  )
}

# Export the tables to an xlsx file, one worksheet per sample size -------------

export_regression_tables <- function(
    tables,
    file = "regression_tables.xlsx"
) {
  
  wb <- createWorkbook()
  
  header_style <- createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "bottom",
    wrapText = TRUE
  )
  
  label_style <- createStyle(
    textDecoration = "bold"
  )
  
  value_style <- createStyle(
    halign = "center"
  )
  
  note_style <- createStyle(
    fontSize = 10,
    textDecoration = "italic"
  )
  
  for (sheet_name in names(tables)) {
    
    tab <- tables[[sheet_name]]
    
    addWorksheet(wb, sheet_name)
    
    writeData(
      wb,
      sheet_name,
      tab,
      headerStyle = header_style
    )
    
    # Bold row labels
    addStyle(
      wb,
      sheet_name,
      label_style,
      rows = 2:(nrow(tab) + 1),
      cols = 1,
      gridExpand = TRUE
    )
    
    # Centred values
    addStyle(
      wb,
      sheet_name,
      value_style,
      rows = 2:(nrow(tab) + 1),
      cols = 2:ncol(tab),
      gridExpand = TRUE
    )
    
    # Notes below the table
    notes <- c(
      "Signif. codes: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1",
      "Coefficients and significance stars are based on HC3 robust standard errors.",
      "Dependent variable: mean p-value of the given test."
    )
    
    writeData(
      wb,
      sheet_name,
      notes,
      startRow = nrow(tab) + 3,
      startCol = 1,
      colNames = FALSE
    )
    
    addStyle(
      wb,
      sheet_name,
      note_style,
      rows = (nrow(tab) + 3):(nrow(tab) + 2 + length(notes)),
      cols = 1,
      gridExpand = TRUE
    )
    
    setColWidths(
      wb,
      sheet_name,
      cols = 1:ncol(tab),
      widths = c(22, rep(19, ncol(tab) - 1))
    )
    
    freezePane(
      wb,
      sheet_name,
      firstActiveRow = 2,
      firstActiveCol = 2
    )
  }
  
  saveWorkbook(
    wb,
    file,
    overwrite = TRUE
  )
}

export_regression_tables(
  regression_tables,
  file = "regression_tables.xlsx"
)
