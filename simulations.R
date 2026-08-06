# Packages

# install.packages("extraDistr")
# install.packages("evd")
# install.packages("twosamples")
# install.packages("energy")
# install.packages("data.table")

library(extraDistr)
library(evd)
library(twosamples)
library(energy)
library(data.table)

setDTthreads(0)

#--------------------------------------------------------------------------------


# Mixture distribution

mixture_normal <- function(n, mu, sigma, eps) {
  
  n2 <- round(n * eps)
  n1 <- n - n2
  
  x <- c(
    rnorm(n1, mean = 0, sd = 1),
    rnorm(n2, mean = mu, sd = sigma)
  )
  
  sample(x)
}

# Distributions

DIST <- list(
  
  "Student's t (df = 1)" = function(n)
    rt(n, df = 1),
  
  "Student's t (df = 3)" = function(n)
    rt(n, df = 3),
  
  "Standard Logistic" = function(n)
    rlogis(n, location = 0, scale = 1),
  
  "Standard Laplace" = function(n)
    rlaplace(n, mu = 0, sigma = 1),

  "Gumbel (0,1)" = function(n)
    rgumbel(n, loc = 0, scale = 1),
  
  "Gumbel (0,2)" = function(n)
    rgumbel(n, loc = 0, scale = 2),
  
  "Gumbel (0,0.5)" = function(n)
    rgumbel(n, loc = 0, scale = 0.5),
  
  "Exponential (1)" = function(n)
    rexp(n, rate = 1),

  "Gamma (2,1)" = function(n)
    rgamma(n,
           shape = 2,
           scale = 1),
  
  "Gamma (0.5,1)" = function(n)
    rgamma(n,
           shape = 0.5,
           scale = 1),

  "Lognormal (0,1)" = function(n)
    rlnorm(n,
           meanlog = 0,
           sdlog = 1),
  
  "Lognormal (0,2)" = function(n)
    rlnorm(n,
           meanlog = 0,
           sdlog = 2),

  "Weibull (0.5,1)" = function(n)
    rweibull(n,
             shape = 1,
             scale = 0.5),
  
  "Weibull (2,1)" = function(n)
    rweibull(n,
             shape = 1,
             scale = 2),

  "Uniform (0,1)" = function(n)
    runif(n,
          min = 0,
          max = 1),

  "Beta (2,2)" = function(n)
    rbeta(n,
          shape1 = 2,
          shape2 = 2),
  
  "Beta (0.5,2)" = function(n)
    rbeta(n,
          shape1 = 0.5,
          shape2 = 2),
  
  "Beta (3,1.5)" = function(n)
    rbeta(n,
          shape1 = 3,
          shape2 = 1.5),
  
  "Beta (2,1)" = function(n)
    rbeta(n,
          shape1 = 2,
          shape2 = 1),

  "Mixture: 0.95*N(0,1)+0.05*N(3,1)" =
    function(n)
      mixture_normal(n,
                     mu = 3,
                     sigma = 1,
                     eps = 0.05),
  
  "Mixture: 0.90*N(0,1)+0.10*N(3,1)" =
    function(n)
      mixture_normal(n,
                     mu = 3,
                     sigma = 1,
                     eps = 0.10),
  
  "Mixture: 0.95*N(0,1)+0.05*N(0,9)" =
    function(n)
      mixture_normal(n,
                     mu = 0,
                     sigma = 3,
                     eps = 0.05),
  
  "Mixture: 0.90*N(0,1)+0.10*N(0,9)" =
    function(n)
      mixture_normal(n,
                     mu = 0,
                     sigma = 3,
                     eps = 0.10)
  
)

# Distribution groups

GROUP_MAP <- c(
  
  # (-infinity,+infinity) symmetric
  
  "Student's t (df = 1)" = "(-infinity;+infinity) symmetric",
  "Student's t (df = 3)" = "(-infinity;+infinity) symmetric",
  "Standard Logistic"     = "(-infinity;+infinity) symmetric",
  "Standard Laplace"      = "(-infinity;+infinity) symmetric",
  
  # (-infinity,+infinity) asymmetric
  
  "Gumbel (0,1)"   = "(-infinity;+infinity) asymmetric",
  "Gumbel (0,2)"   = "(-infinity;+infinity) asymmetric",
  "Gumbel (0,0.5)" = "(-infinity;+infinity) asymmetric",
  
  # (0,+infinity)
  
  "Exponential (1)" = "(0;+infinity)",
  "Gamma (2,1)"     = "(0;+infinity)",
  "Gamma (0.5,1)"   = "(0;+infinity)",
  "Lognormal (0,1)" = "(0;+infinity)",
  "Lognormal (0,2)" = "(0;+infinity)",
  "Weibull (0.5,1)" = "(0;+infinity)",
  "Weibull (2,1)"   = "(0;+infinity)",
  
  # (0,1)
  
  "Uniform (0,1)" = "(0;1)",
  "Beta (2,2)"    = "(0;1)",
  "Beta (0.5,2)"  = "(0;1)",
  "Beta (3,1.5)"  = "(0;1)",
  "Beta (2,1)"    = "(0;1)",
  
  # Normal mixtures
  
  "Mixture: 0.95*N(0,1)+0.05*N(3,1)" = "Normal mixture",
  "Mixture: 0.90*N(0,1)+0.10*N(3,1)" = "Normal mixture",
  "Mixture: 0.95*N(0,1)+0.05*N(0,9)" = "Normal mixture",
  "Mixture: 0.90*N(0,1)+0.10*N(0,9)" = "Normal mixture"
)

# Return the distribution group

classify_group <- function(name) {
  unname(GROUP_MAP[name])
}

#-----------------------------------------------------------------------------

chi_square_test <- function(sample1, sample2, breaks) {
  
  c1 <- hist(sample1, breaks = breaks, plot = FALSE)$counts
  c2 <- hist(sample2, breaks = breaks, plot = FALSE)$counts
  
  keep <- (c1 + c2) > 0
  
  tab <- rbind(
    c1[keep],
    c2[keep]
  )
  
  test <- suppressWarnings(chisq.test(tab, correct = FALSE))
  
  list(
    p.value = test$p.value,
    expected = test$expected,
    dropped = sum(!keep)
  )
}


p_value <- function(expr1, expr2, n) {
  
  n_boots <- 199
  
  sample1 <- DIST[[expr1]](n)
  sample2 <- DIST[[expr2]](n)
  
  combined <- c(sample1, sample2)
  
  ## Sturges-based Chi-square
  
  k <- round(1 + log2(n))
  
  breaks_st <- seq(
    min(combined),
    max(combined),
    length.out = k + 1
  )
  
  st <- chi_square_test(
    sample1,
    sample2,
    breaks_st
  )
  
  ## Quantile-based Chi-square
  
  breaks_dec <- unique(
    quantile(
      combined,
      probs = seq(0, 1, 0.1),
      names = FALSE
    )
  )
  
  dec <- chi_square_test(
    sample1,
    sample2,
    breaks_dec
  )
  

  if (mean(dec$expected < 5) > 0.20) {
    
    breaks_dec <- unique(
      quantile(
        combined,
        probs = seq(0, 1, 0.2),
        names = FALSE
      )
    )
    
    dec <- chi_square_test(
      sample1,
      sample2,
      breaks_dec
    )
  }
  
  ## Kolmogorov-Smirnov
  
  ks <- ks_test(sample1, sample2, nboots = n_boots)
  
  ## Kruskal-Wallis

  kw <- kruskal.test(sample1, sample2)
  
  ## Cramer-von Mises
  
  cvm <- cvm_test(sample1, sample2, nboots = n_boots)
  
  ## Anderson-Darling
  
  ad <- ad_test(sample1, sample2, nboots = n_boots)
  
  ## Kuiper
  
  kuiper <- kuiper_test(sample1, sample2, nboots = n_boots)
  
  ## Wasserstein
  
  wass <- wass_test(sample1, sample2, nboots = n_boots)
  
  ## Energy
  
  energy_test <- eqdist.etest(
    as.matrix(c(sample1, sample2)),
    sizes = c(length(sample1), length(sample2)),
    R = n_boots
  )
  
  list(
    
    "Chi-square (Sturges)" = st$p.value,
    
    "Chi-square (Quantile)" = dec$p.value,
    
    "Kolmogorov-Smirnov" = ks[[2]],
    
    "Kruskal-Wallis" = kw$p.value,
    
    "Cramer-von Mises" = cvm[[2]],
    
    "Anderson-Darling" = ad[[2]],
    
    "Kuiper" = kuiper[[2]],
    
    "Wasserstein" = wass[[2]],
    
    "Energy" = energy_test$p.value,
    
    "Dropped bins (Sturges)" = st$dropped
    
  )
}


#-----------------------------------------------------------------------------

results <- list()
idx <- 1

output_file <- "simulation_results.csv"

if (file.exists(output_file)) {
  file.remove(output_file)
}

first_write <- TRUE

# Sample sizes
sample_sizes <- c(seq(10, 500, by = 10),
                  seq(550, 1000, by = 50))

dist_names <- names(DIST)

test_names <- c(
  "Chi-square (Sturges)",
  "Chi-square (Quantile)",
  "Kolmogorov-Smirnov",
  "Kruskal-Wallis",
  "Cramer-von Mises",
  "Anderson-Darling",
  "Kuiper",
  "Wasserstein",
  "Energy"
)

for (n in sample_sizes) {
  
  for (name1 in dist_names) {
    
    for (name2 in dist_names) {
      
      for (seed in 0:99) {
        
        set.seed(seed)
        
        res <- p_value(name1, name2, n)
        
        results[[idx]] <- data.table(
          Distribution1 = name1,
          Distribution2 = name2,
          SampleSize    = n,
          Test          = test_names,
          PValue        = unlist(res[test_names], use.names = FALSE),
          Seed          = seed,
          DroppedBins = ifelse(
            test_names == "Chi-square (Sturges)",
            res[["Dropped bins (Sturges)"]],
            NA_integer_
          )
        )
        
        idx <- idx + 1
      }
    }
  }
  
  df_batch <- rbindlist(results)
  
  # Write to csv
  fwrite(
    df_batch,
    file = output_file,
    append = !first_write,
    col.names = first_write
  )
  
  first_write <- FALSE
  
  cat("Saved until:", n, "\n")
  
  results <- list()
  idx <- 1
  rm(df_batch)
  gc()
}

# Combine into one data frame
df <- fread(output_file)

head(df, 100)

# Decisions

df[, Decision5 := PValue < 0.05]
df[, Decision1 := PValue < 0.01]

# Distribution groups

df[, Group1 := classify_group(Distribution1)]
df[, Group2 := classify_group(Distribution2)]

# Summary table

df_summary <- df[
  ,
  .(
    Decision5Rate = mean(Decision5),
    Decision1Rate = mean(Decision1),
    MeanPValue = mean(PValue),
    MinPValue = min(PValue),
    MaxPValue = max(PValue)
  ),
  by = .(
    Distribution1,
    Distribution2,
    SampleSize,
    Test,
    Group1,
    Group2
  )
]

fwrite(
  df_summary,
  "simulation_results_summary.csv"
)
