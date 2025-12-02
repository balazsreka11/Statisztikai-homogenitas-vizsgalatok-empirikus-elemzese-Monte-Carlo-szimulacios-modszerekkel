# Első eloszláscsoport -----------------------------------------------------------------

# Working directory beállítása
# setwd("C:\\Users\\Asus\\Downloads")

library(ggplot2)
library(patchwork)

# Közös tartomány
x <- seq(-8, 8, length.out = 2000)

# Sűrűségfüggvények
dens_cauchy   <- dcauchy(x, location = 0, scale = 1)      
dens_t3       <- dt(x, df = 3)                           
dens_logistic <- dlogis(x, location = 0, scale = 1)       
dens_laplace  <- 0.5 * exp(-abs(x))                       

base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Times"),
    legend.position = "none",
    plot.margin = margin(10, 10, 30, 10)
  )

# Plotok
p1 <- ggplot() +
  geom_line(aes(x = x, y = dens_cauchy), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Student t-eloszlás (df=1)",
       x = "x", y = "f(x)") +
  base_theme

p2 <- ggplot() +
  geom_line(aes(x = x, y = dens_t3), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Student t-eloszlás (df=3)",
       x = "x", y = "f(x)") +
  base_theme

p3 <- ggplot() +
  geom_line(aes(x = x, y = dens_logistic), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Standard logisztikus",
       x = "x", y = "f(x)") +
  base_theme

p4 <- ggplot() +
  geom_line(aes(x = x, y = dens_laplace), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Standard Laplace",
       x = "x", y = "f(x)") +
  base_theme

# Elrendezés
final_plot <- (p1 + p2) / (p3 + p4) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times")  
  )

final_plot

# Második eloszláscsoport -----------------------------------------------------------------

# Közös tartomány
x <- seq(-6, 12, length.out = 2000)

# Sűrűségfüggvények
dgumbel <- function(x, alpha, beta) {
  (1/beta) * exp(-(x - alpha)/beta) * exp(-exp(-(x - alpha)/beta))
}

dens_g1   <- dgumbel(x, alpha = 0, beta = 1)
dens_g2   <- dgumbel(x, alpha = 0, beta = 2)
dens_g0.5 <- dgumbel(x, alpha = 0, beta = 0.5)

base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Times"),
    legend.position = "none",
    plot.margin = margin(10, 10, 30, 10)
  )

# Plotok
p1 <- ggplot() +
  geom_line(aes(x = x, y = dens_g1), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Gumbel eloszlás (0, 1)",
       x = "x",
       y = "f(x)") +
  base_theme

p2 <- ggplot() +
  geom_line(aes(x = x, y = dens_g2), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Gumbel eloszlás (0, 2)",
       x = "x",
       y = "f(x)") +
  base_theme

p3 <- ggplot() +
  geom_line(aes(x = x, y = dens_g0.5), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Gumbel eloszlás (0, 1/2)",
       x = "x",
       y = "f(x)") +
  base_theme

# Elrendezés
final_plot2 <- (p3 | p1 | p2) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times")
  )

final_plot2

# Harmadik eloszláscsoport -----------------------------------------------------------------

# Közös tartomány
x <- seq(0, 12, length.out = 2000)

# Sűrűségfüggvény
dens_exp <- dexp(x, rate = 1)
dens_gam_a2   <- dgamma(x, shape = 2,   scale = 1)
dens_gam_a0.5 <- dgamma(x, shape = 0.5, scale = 1)
dens_lnorm01 <- dlnorm(x, meanlog = 0, sdlog = 1)
dens_lnorm02 <- dlnorm(x, meanlog = 0, sdlog = 2)
dens_weib_a0.5 <- dweibull(x, shape = 1, scale = 0.5)
dens_weib_a2   <- dweibull(x, shape = 1,   scale = 2)

base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Times"),
    legend.position = "none",
    plot.margin = margin(10, 10, 30, 10)
  )

# Plotok
p_exp <- ggplot() +
  geom_line(aes(x = x, y = dens_exp), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Exponenciális (1)", x = "x", y = "f(x)") +
  base_theme

p_gam2 <- ggplot() +
  geom_line(aes(x = x, y = dens_gam_a2), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Gamma (2, 1)", x = "x", y = "f(x)") +
  base_theme

p_gam05 <- ggplot() +
  geom_line(aes(x = x, y = dens_gam_a0.5), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Gamma (1/2, 1)", x = "x", y = "f(x)") +
  base_theme

p_lnorm01 <- ggplot() +
  geom_line(aes(x = x, y = dens_lnorm01), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Lognormális (0,1)", x = "x", y = "f(x)") +
  base_theme

p_lnorm02 <- ggplot() +
  geom_line(aes(x = x, y = dens_lnorm02), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Lognormális (0,2)", x = "x", y = "f(x)") +
  base_theme

p_weib05 <- ggplot() +
  geom_line(aes(x = x, y = dens_weib_a0.5), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Weibull (1/2, 1)", x = "x", y = "f(x)") +
  base_theme

p_weib2 <- ggplot() +
  geom_line(aes(x = x, y = dens_weib_a2), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Weibull (2, 1)", x = "x", y = "f(x)") +
  base_theme

# Elrendezés
empty <- ggplot() + theme_void()

final_plot3 <- 
  (p_exp | p_gam2)   /
  (p_gam05 | p_lnorm01) /
  (p_lnorm02 | p_weib05) /
  (p_weib2 | empty) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times"))

final_plot3


# Negyedik eloszláscsoport -----------------------------------------------------------------

# Közös tartomány
x <- seq(0, 1, length.out = 2000)

# Sűrűségfüggvények
dens_unif <- dunif(x, min = 0, max = 1)
dens_b22   <- dbeta(x, shape1 = 2,   shape2 = 2)    
dens_b0_5_2<- dbeta(x, shape1 = 0.5, shape2 = 2)    
dens_b3_1_5<- dbeta(x, shape1 = 3,   shape2 = 1.5)   
dens_b2_1  <- dbeta(x, shape1 = 2,   shape2 = 1)     

base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Times"),
    legend.position = "none",
    plot.margin = margin(10, 10, 30, 10)
  )

# Plotok
p_unif <- ggplot() +
  geom_line(aes(x = x, y = dens_unif), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  labs(title = "Egyenletes [0,1]", x = "x", y = "f(x)") +
  base_theme

p_b22 <- ggplot() +
  geom_line(aes(x = x, y = dens_b22), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Beta (2, 2)", x = "x", y = "f(x)") +
  base_theme

p_b0_5_2 <- ggplot() +
  geom_line(aes(x = x, y = dens_b0_5_2), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Beta (1/2, 2)", x = "x", y = "f(x)") +
  base_theme

p_b3_1_5 <- ggplot() +
  geom_line(aes(x = x, y = dens_b3_1_5), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Beta (3, 3/2)", x = "x", y = "f(x)") +
  base_theme

p_b2_1 <- ggplot() +
  geom_line(aes(x = x, y = dens_b2_1), color = "black", size = 0.8) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  labs(title = "Beta (2, 1)", x = "x", y = "f(x)") +
  base_theme

# Elrendezés
final_plot4 <- (p_unif | p_b22 | p_b0_5_2) /
  (p_b3_1_5 | p_b2_1 | plot_spacer()) +
  theme(plot.title = element_text(hjust = 0.5, family = "Times"))

final_plot4

# Eredmények beolvasása -----------------------------------------------------------------

library(readr)

df <- read_csv("eredmenyek_osszefoglalo.csv")
head(df)

# Globális erőfüggvények ------------------------------------------------------------

library(dplyr)
library(tidyr)

custom_cols <- c(
  "#D62828",
  "#2A9D8F",
  "#5FA8D3",
  "#6A4C93"
)

# Különböző eloszlású sorok
df_diff <- df %>%
  filter(`Eloszlás 1` != `Eloszlás 2`)

# Erő számítása 5% és 1% szinten
power_df <- df_diff %>%
  group_by(Próba, Elemszám) %>%
  summarise(
    `Erő (5%)` = mean(`Döntés 5 arány`, na.rm = TRUE),
    `Erő (1%)` = mean(`Döntés 1 arány`, na.rm = TRUE),
    .groups = "drop"
  )

power_df <- power_df %>%
  pivot_longer(
    cols = c(`Erő (5%)`, `Erő (1%)`),
    names_to = "Szint",
    values_to = "Erő"
  )


# 5%-os erő grafikon
p5 <- power_df %>%
  filter(Szint == "Erő (5%)") %>%
  ggplot(aes(x = Elemszám, y = Erő, color = Próba)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_color_manual(values = custom_cols) +
  labs(
    x = "Elemszám (n)",
    y = "Erő (5%)",
    color = "Próba"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

print(p5)

# 1%-os erő grafikon
p1 <- power_df %>%
  filter(Szint == "Erő (1%)") %>%
  ggplot(aes(x = Elemszám, y = Erő, color = Próba)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_color_manual(values = custom_cols) +
  labs(
    x = "Elemszám (n)",
    y = "Erő (1%)",
    color = "Próba"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

print(p1)

# Csoportokon belüli erőfüggvények ------------------------------------------------------------

# Csak eltérő eloszlások kiválasztása, adatok átrendezése
power_df2 <- df %>%
  filter(`Eloszlás 1` != `Eloszlás 2`) %>%
  select(`Csoport 1`, `Csoport 2`, Próba, Elemszám, `Döntés 5 arány`, `Döntés 1 arány`) %>%
  pivot_longer(
    cols = c(`Döntés 5 arány`, `Döntés 1 arány`),
    names_to = "Döntés",
    values_to = "Erő"
  ) %>%
  mutate(Alfa = ifelse(Döntés == "Döntés 5 arány", 0.05, 0.01))

power_within <- power_df2 %>%
  filter(`Csoport 1` == `Csoport 2`) %>%
  transmute(Csoport = `Csoport 1`, Próba, Elemszám, Alfa, Erő)

# Átlagos erő számítása csoportonként, próbánként, elemszámonként, kétféle szignifikanciaszintre
power_within <- power_within %>%
  group_by(Csoport, Próba, Elemszám, Alfa) %>%
  summarise(
    Erő = mean(Erő, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Csoport, Próba, Alfa, Elemszám)

# Erőfüggvény rajzolása csoportonként, adott alfára
plot_power_by_group <- function(dat, alpha_value) {
  groups_order <- c("(-∞;+∞) szimmetrikus","(-∞;+∞) aszimmetrikus","(0;+∞)","(0;1)","kevert (normál keverék)")
  
  dat %>%
    filter(Alfa == alpha_value) %>%
    mutate(Csoport = factor(Csoport, levels = groups_order)) %>%
    ggplot(aes(x = Elemszám, y = Erő, color = Próba, group = Próba)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.6) +
    facet_wrap(~ Csoport, nrow = 2) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(values = custom_cols) +      
    labs(
      x = "Elemszám (n)",
      y = expression("Erő ("*1 - beta*")"),
      color = "Próba"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 1))
}

p5 <- plot_power_by_group(power_within, 0.05)
p1 <- plot_power_by_group(power_within, 0.01)

print(p5)
print(p1)

# Csoportok közötti erőfüggvények ------------------------------------------------------------

# csoportok sorrendje
groups_order <- c(
  "(-∞;+∞) szimmetrikus",
  "(-∞;+∞) aszimmetrikus",
  "(0;+∞)",
  "(0;1)",
  "kevert (normál keverék)"
)

pair_str <- function(a, b) paste(sort(c(a, b)), collapse = "  vs  ")

# összes lehetséges páros
all_pairs <- {
  ap <- character()
  for (i in seq_along(groups_order)) {
    for (j in (i+1):length(groups_order)) {
      if (!is.na(j)) ap <- c(ap, pair_str(groups_order[i], groups_order[j]))
    }
  }
  ap
}

# csoportok közötti erőfüggvények ábrázolása
plot_between_groups <- function(alpha_value) {
  
  between <- power_df2 %>%
    filter(Alfa == alpha_value, `Csoport 1` != `Csoport 2`) %>%
    mutate(`Csoport pár` = mapply(pair_str, `Csoport 1`, `Csoport 2`)) %>%
    group_by(`Csoport pár`, Próba, Elemszám, Alfa) %>%
    summarise(
      Erő = mean(Erő, na.rm = TRUE),
      .groups = "drop"
    )
  
  present_pairs <- all_pairs[all_pairs %in% unique(between$`Csoport pár`)]
  if (length(present_pairs) == 0) stop("Nincs páros, ami megfelel.")
  
  between <- between %>%
    mutate(
      `Csoport pár` = factor(`Csoport pár`, levels = present_pairs),
      Próba = factor(Próba)
    ) %>%
    arrange(`Csoport pár`, Próba, Elemszám)
  
  ggplot(between, aes(x = Elemszám, y = Erő, color = Próba, group = Próba)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 1.6) +
    facet_wrap(~ `Csoport pár`, nrow = 3, ncol = 4, scales = "fixed") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(values = custom_cols) +
    labs(
      x = "Elemszám (n)",
      y = expression("Erő (" * 1 - beta * ")"),
      color = "Próba"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(nrow = 1))
}

p5 <- plot_between_groups(0.05)
p1 <- plot_between_groups(0.01)

print(p5)
print(p1)

# Sűrűségfüggvény-ábrák ------------------------------------------------------------

# P-érték átlagának sűrűségfüggvénye különböző próbákra
plot_pmean_density <- function(dat, n, same = TRUE, adjust = 3, bw = NULL) {
  
  # szűrés elemszámra és H0/H1-ra
  sub <- dat %>%
    dplyr::filter(Elemszám == n, (`Eloszlás 1` == `Eloszlás 2`) == same) %>%
    dplyr::filter(!is.na(`P-Érték átlag`))
  
  if (nrow(sub) < 2) {
    stop("Túl kevés adat a denzitás becsléséhez ezen a szűrésen.")
  }
  
  x <- sub$`P-Érték átlag`
  bw_use <- if (!is.null(bw)) bw else {
    b <- tryCatch(stats::bw.nrd0(x), error = function(e) NA_real_)
    if (!is.finite(b) || b <= 0) 0.05 else b
  }
  
  # sűrűség-görbe rajzolása próbánként
  ggplot(sub, aes(x = `P-Érték átlag`, color = Próba)) +
    geom_density(
      linewidth = 1,
      adjust = adjust,
      bw = bw_use,
      trim = TRUE
    ) +
    geom_vline(xintercept = c(0.01, 0.05),
               linetype = "dashed",
               color = c("red", "blue"),
               linewidth = 0.7) +
    scale_color_manual(
      values = custom_cols,
      guide = guide_legend(override.aes = list(
        linetype = 1,
        size = 1.2
      ))
    ) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(
      x = "P-érték átlag",
      y = "Sűrűség",
      color = "Próba"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 10),
      plot.title   = element_text(hjust = 0.5)
    )
}

# H0: azonos eloszlások (same = TRUE)
p_h0_mean <- plot_pmean_density(df, 1000, same = TRUE)

# H1: különböző eloszlások (same = FALSE)
p_h1_mean <- plot_pmean_density(df, 100, same = FALSE)

print(p_h0_mean)
print(p_h1_mean)

# Sűrűségfüggvény-ábrák függvénnyel ------------------------------------------------------------

# P-értékátlag sűrűségfüggvénye két megadott csoportra
plot_pmean_density_between_groups <- function(
    df,
    group_a,
    group_b,
    n = 100,
    adjust = 3,
    bw = 0.02,
    xlim = c(0, 1)
) {
  custom_cols <- c(
    "Khi-négyzet (decilis)" = "#D62828",
    "Khi-négyzet (Sturges)" = "#2A9D8F",
    "Kolmogorov–Smirnov" = "#5FA8D3",
    "Kruskal–Wallis"     = "#6A4C93"
  )
  
  
  # ellenőrzés: a megadott csoportok szerepelnek-e
  groups_all <- union(df$`Csoport 1`, df$`Csoport 2`)
  if (!group_a %in% groups_all || !group_b %in% groups_all) {
    stop("A megadott csoport(ok) nem szerepelnek a Csoport 1/2 oszlopokban.")
  }
  
  # szűrés elemszámra, csoportpárra és H1-re (különböző eloszlások)
    dat <- df %>%
    dplyr::filter(
      Elemszám == n,
      !is.na(`P-Érték átlag`),
      ((`Csoport 1` == group_a & `Csoport 2` == group_b) |
       (`Csoport 1` == group_b & `Csoport 2` == group_a)),
      `Eloszlás 1` != `Eloszlás 2`
    )
  
  if (nrow(dat) == 0) stop("Nincs adat a megadott feltételekre.")
  
  # sűrűségfüggvény rajzolása próbánként
  ggplot(dat, aes(x = `P-Érték átlag`, color = Próba)) +
    geom_density(
      linewidth = 1.2,
      adjust = adjust,
      bw = bw,
      trim = TRUE
    ) +
    scale_color_manual(values = custom_cols) +
    guides(
      color = guide_legend(
        title = "Próba",
        override.aes = list(
          linetype = 1,
          linewidth = 1 
        )
      )
    ) +
    geom_vline(
      xintercept = c(0.01, 0.05),
      linetype   = "dashed",
      color      = c("#D62828", "#5FA8D3"),
      linewidth  = 0.7
    ) +
    coord_cartesian(xlim = xlim) +
    labs(
      x = "P-érték átlag",
      y = "Sűrűség"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "right",
      plot.title      = element_text(hjust = 0.5),
      legend.title    = element_text(size = 11),
      legend.text     = element_text(size = 10)
    )
}


# példa hívás
plot_pmean_density_between_groups(
  df,
  "(0;1)",
  "(0;1)",
  n = 600
)

# elérhető csoportok ellenőrzése
unique(df$`Csoport 1`)
unique(df$Próba)

# Hőtérkép ------------------------------------------------------------

# P-érték hőtérképe eloszláspárok szerint
plot_p_heatmap <- function(df, n, proba, column) {
  
  if (!column %in% names(df)) {
    stop(paste0("Nincs ilyen oszlop az adatokban: ", column))
  }
  
  # szűrés elemszámra és próbára 
  dat <- df %>%
    filter(
      Elemszám == n,
      Próba == proba,
      !is.na(.data[[column]])
    ) %>%
    select(`Eloszlás 1`, `Eloszlás 2`, value = all_of(column))
  
  # sorrend egységesítése
  lev <- sort(unique(c(dat$`Eloszlás 1`, dat$`Eloszlás 2`)))
  dat <- dat %>%
    mutate(
      `Eloszlás 1` = factor(`Eloszlás 1`, levels = lev),
      `Eloszlás 2` = factor(`Eloszlás 2`, levels = lev)
    )
  
  # hőtérkép rajzolása
  ggplot(dat, aes(x = `Eloszlás 1`, y = `Eloszlás 2`, fill = value)) +
    geom_tile(color = "grey80") +
    scale_fill_gradient(
      name = column,
      low = "white",
      high = "#D62828",
      limits = c(0, 1)
    ) +
    coord_fixed() +
    labs(
      x = "Eloszlás 1",
      y = "Eloszlás 2"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
}


plot_p_heatmap(df, n = 100, proba = "Khi-négyzet (Sturges)", column = "P-Érték átlag")

unique(df$`Próba`)


# Elméleti eloszlások jellemzői ------------------------------------------------------------

library(readxl)

# Excel beolvasása
df_elemzes <- read_excel("elemzes.xlsx")

str(df_elemzes)

df_elemzes[c("Ferdeségkülönbség","Csúcsosságkülönbség","Móduszkülönbség")] <-
  lapply(df_elemzes[c("Ferdeségkülönbség","Csúcsosságkülönbség","Móduszkülönbség")],
         function(x) ifelse(x == "nincs", NA, x))


df_elemzes[c("Ferdeségkülönbség","Csúcsosságkülönbség","Móduszkülönbség")] <- 
  lapply(df_elemzes[c("Ferdeségkülönbség","Csúcsosságkülönbség","Móduszkülönbség")], as.numeric)

numeric_df <- df_elemzes[sapply(df_elemzes, is.numeric)][ , 2:10]

# Korrelációs mátrix ------------------------------------------------------------

# Pearson-korrelációs mátrix számítása
corr_matrix <- cor(numeric_df, use = "complete.obs", method = "pearson")
print(corr_matrix)

library(reshape2)

melt_corr <- melt(corr_matrix)

# # korrelációs hőtérkép felirattal
ggplot(melt_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#5FA8D3", high = "#D62828", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Korreláció") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "")

# Dobozábrák ------------------------------------------------------------

# P-értékek alakulása aszerint, hogy a tartomány egyezik-e
p_df <- df_elemzes %>%
  select(
    `Tartomány egyezik`,
    `Khi-négyzet (Sturges)`,
    `Khi-négyzet (decilis)`,
    `Kolmogorov–Smirnov`,
    `Kruskal–Wallis`
  ) %>%
  mutate(
    `Tartomány egyezik` = factor(
      `Tartomány egyezik`,
      levels = c(FALSE, TRUE),
      labels = c("Hamis", "Igaz")
    )
  ) %>%
  pivot_longer(
    cols = -`Tartomány egyezik`,
    names_to = "Proba",
    values_to = "P_ertek"
  )

ggplot(p_df, aes(x = `Tartomány egyezik`, y = P_ertek, fill = `Tartomány egyezik`)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~ Proba, scales = "free_y") +
  scale_fill_manual(values = c("Hamis" = "#5FA8D3", "Igaz" = "#D62828")) +
  theme_minimal() +
  labs(
    x = "",
    y = "P-érték"
  )


# P-értékek alakulása aszerint, hogy a csoport egyezik-e
p_df <- df_elemzes %>%
  select(
    `Csoport egyezik`,
    `Khi-négyzet (Sturges)`,
    `Khi-négyzet (decilis)`,
    `Kolmogorov–Smirnov`,
    `Kruskal–Wallis`
  ) %>%
  mutate(
    `Csoport egyezik` = factor(
      `Csoport egyezik`,
      levels = c(FALSE, TRUE),
      labels = c("Hamis", "Igaz")
    )
  ) %>%
  pivot_longer(
    cols = -`Csoport egyezik`,
    names_to = "Proba",
    values_to = "P_ertek"
  )

ggplot(p_df, aes(x = `Csoport egyezik`, y = P_ertek, fill = `Csoport egyezik`)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~ Proba, scales = "free_y") +
  scale_fill_manual(values = c("Hamis" = "#5FA8D3", "Igaz" = "#D62828")) +
  theme_minimal() +
  labs(
    x = "Csoport egyezik",
    y = "P-érték"
  )


# Regressziós modellek ------------------------------------------------------------

# Logikai változók faktorrá alakítása
df_elemzes$`Tartomány egyezik` <- as.factor(df_elemzes$`Tartomány egyezik`)

df_elemzes$`Csoport egyezik` <- as.factor(df_elemzes$`Csoport egyezik`)

# Lineáris modellek a négy próbára
deciles_model <- lm(`Khi-négyzet (decilis)` ~  `Átfedés (OVL)` + `Mediánkülönbség` + `Tartomány egyezik` +
                      `Csoport egyezik` + `Ferdeségkülönbség` + `Csúcsosságkülönbség` + `Móduszkülönbség`, data = df_elemzes)

sturges_model <- lm(`Khi-négyzet (Sturges)` ~  `Átfedés (OVL)` + `Mediánkülönbség` + `Tartomány egyezik` +
                      `Csoport egyezik` + `Ferdeségkülönbség` + `Csúcsosságkülönbség` + `Móduszkülönbség`, data = df_elemzes)

ks_model <- lm(`Kolmogorov–Smirnov` ~  `Átfedés (OVL)` + `Mediánkülönbség` + `Tartomány egyezik` +
                 `Csoport egyezik` + `Ferdeségkülönbség` + `Csúcsosságkülönbség` + `Móduszkülönbség`, data = df_elemzes)


kw_model <- lm(`Kruskal–Wallis` ~  `Átfedés (OVL)` + `Mediánkülönbség` + `Tartomány egyezik` +
                 `Csoport egyezik` + `Ferdeségkülönbség` + `Csúcsosságkülönbség` + `Móduszkülönbség`, data = df_elemzes)

str(df_elemzes)
summary(deciles_model)
summary(sturges_model)
summary(ks_model)
summary(kw_model)

# White-teszt heteroszkedaszticitásra (interakciók nélkül)
library(skedastic)

white(deciles_model, interactions=FALSE)
white(sturges_model, interactions=FALSE)
white(ks_model, interactions=FALSE)
white(kw_model, interactions=FALSE)

# Robusztus szórásmátrixszal korrigált t-próbák
library(lmtest)
library(car)

coeftest(deciles_model, vcov = hccm(deciles_model))
coeftest(sturges_model, vcov = hccm(sturges_model))
coeftest(ks_model, vcov = hccm(ks_model))
coeftest(kw_model, vcov = hccm(kw_model))

# További használt ábrázolások ------------------------------------------------------------

# csak (0;1) csoport, 5%-os szint
p_01_5 <- power_within %>%
  dplyr::filter(Csoport == "(0;1)", Alfa == 0.05) %>%
  ggplot(aes(x = Elemszám, y = Erő, color = Próba, group = Próba)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(values = custom_cols) +
  labs(
    x = "Elemszám (n)",
    y = expression("Erő ("*1 - beta*")"),
    color = "Próba"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

print(p_01_5)

# kevert (normál keverék) csoport, 5%-os szint
p_mix_5 <- power_within %>%
  dplyr::filter(Csoport == "kevert (normál keverék)", Alfa == 0.05) %>%
  ggplot(aes(x = Elemszám, y = Erő, color = Próba, group = Próba)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(values = custom_cols) +
  labs(
    x = "Elemszám (n)",
    y = expression("Erő ("*1 - beta*")"),
    color = "Próba"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

print(p_mix_5)

# (-∞;+∞) szimmetrikus vs (0;1), 5%-os szignifikanciaszint
pair_sym_01 <- pair_str("(-∞;+∞) szimmetrikus", "(0;1)")

between_sym_01_5 <- power_df2 %>%
  dplyr::filter(
    Alfa == 0.05,
    `Csoport 1` != `Csoport 2`
  ) %>%
  dplyr::mutate(`Csoport pár` = mapply(pair_str, `Csoport 1`, `Csoport 2`)) %>%
  dplyr::filter(`Csoport pár` == pair_sym_01) %>%
  dplyr::group_by(Próba, Elemszám) %>%
  dplyr::summarise(
    Erő = mean(Erő, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Elemszám)

p_sym_01_5 <- ggplot(between_sym_01_5,
                     aes(x = Elemszám, y = Erő,
                         color = Próba, group = Próba)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(values = custom_cols) +
  labs(
    x = "Elemszám (n)",
    y = expression("Erő ("*1 - beta*")"),
    color = "Próba"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title       = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(nrow = 1))

print(p_sym_01_5)

  