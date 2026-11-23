# ============================================================
# ZOOM GRAFY TYDENNICH VYNOSU
# ============================================================
# Puvodni grafy s plnym rozsahem zustavaji zachovane.
# Tyto verze pouzivaji spolecny rozsah -30 % az +30 %, aby
# byly rozdily mezi stabilnimi a kvantovymi firmami citelnejsi.
# coord_cartesian pouze oreze zobrazeni; statistiky se pocitaji
# ze vsech pozorovani vcetne extremnich hodnot.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(quantmod)
})

setwd("C:/Users/revel/OneDrive/Desktop/StatistikaFinancichTrhu")

return_data <- readRDS("data/processed/returns/all_returns.rds")

zoom_groups <- list(
  quantum_computing = c("IONQ", "RGTI", "QBTS", "IBM"),
  stable_companies = c("KO", "PEP", "PG", "JNJ")
)

zoom_labels <- c(
  quantum_computing = "Kvantove pocitace",
  stable_companies = "Stabilni firmy"
)

zoom_limit <- 30
zoom_dir <- "data/processed/statistics/graphs/zoom_comparison"
dir.create(zoom_dir, recursive = TRUE, showWarnings = FALSE)

zoom_returns <- bind_rows(
  lapply(names(return_data), function(ticker_name) {
    x <- return_data[[ticker_name]]

    data.frame(
      ticker = ticker_name,
      Date = as.Date(index(x)),
      weekly_return = as.numeric(x)
    )
  })
) %>%
  filter(
    ticker %in% unlist(zoom_groups, use.names = FALSE),
    !is.na(weekly_return)
  ) %>%
  mutate(
    group = case_when(
      ticker %in% zoom_groups$quantum_computing ~ "Kvantove pocitace",
      ticker %in% zoom_groups$stable_companies ~ "Stabilni firmy"
    ),
    group = factor(group, levels = c("Kvantove pocitace", "Stabilni firmy")),
    ticker = factor(
      ticker,
      levels = c(zoom_groups$quantum_computing, zoom_groups$stable_companies)
    )
  )

normal_curves_zoom <- zoom_returns %>%
  group_by(ticker, group) %>%
  group_modify(~ {
    x_values <- seq(-zoom_limit, zoom_limit, length.out = 400)

    tibble(
      weekly_return = x_values,
      density = dnorm(
        x_values,
        mean = mean(.x$weekly_return),
        sd = sd(.x$weekly_return)
      )
    )
  }) %>%
  ungroup()

common_caption <- paste0(
  "Zobrazeny rozsah: -", zoom_limit, " % az +", zoom_limit,
  " %. Vypocty zahrnuji i hodnoty mimo zobrazeny rozsah."
)

# ------------------------------------------------------------
# 1. SPOLECNE SROVNANI OBOU SKUPIN
# ------------------------------------------------------------

histogram_comparison_zoom <- ggplot(
  zoom_returns,
  aes(x = weekly_return)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 2,
    boundary = 0,
    fill = "lightblue",
    color = "white"
  ) +
  geom_density(
    color = "blue",
    linewidth = 0.8
  ) +
  geom_line(
    data = normal_curves_zoom,
    aes(x = weekly_return, y = density),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ ticker, ncol = 4, scales = "fixed") +
  coord_cartesian(xlim = c(-zoom_limit, zoom_limit)) +
  labs(
    title = "Rozdeleni tydennich vynosu: kvantove vs. stabilni firmy",
    subtitle = paste(
      "Horni rada: kvantove firmy | Dolni rada: stabilni firmy |",
      "Stejne meritko os a sirka intervalu 2 procentni body"
    ),
    x = "Tydenni vynos (%)",
    y = "Hustota",
    caption = common_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(zoom_dir, "histogram_quantum_vs_stable_zoom.png"),
  histogram_comparison_zoom,
  width = 18,
  height = 9,
  dpi = 300
)

boxplot_comparison_zoom <- ggplot(
  zoom_returns,
  aes(x = ticker, y = weekly_return, fill = group)
) +
  geom_boxplot(
    color = "darkblue",
    outlier.color = "red",
    outlier.alpha = 0.35,
    outlier.size = 1.2
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed"
  ) +
  coord_cartesian(ylim = c(-zoom_limit, zoom_limit)) +
  scale_fill_manual(
    values = c(
      "Kvantove pocitace" = "#9BD3E1",
      "Stabilni firmy" = "#A9D39E"
    )
  ) +
  labs(
    title = "Boxplot tydennich vynosu: kvantove vs. stabilni firmy",
    subtitle = "Spolecne meritko zvyraznuje rozdil v typickem rozptylu",
    x = "Akcie",
    y = "Tydenni vynos (%)",
    fill = "Skupina",
    caption = common_caption
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(zoom_dir, "boxplot_quantum_vs_stable_zoom.png"),
  boxplot_comparison_zoom,
  width = 15,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------
# 2. ZOOM VERZE PUVODNICH GRAFU PRO KAZDOU SKUPINU
# ------------------------------------------------------------

for (group_name in names(zoom_groups)) {
  group_tickers <- zoom_groups[[group_name]]
  group_label <- unname(zoom_labels[group_name])

  group_data <- zoom_returns %>%
    filter(ticker %in% group_tickers) %>%
    mutate(ticker = factor(ticker, levels = group_tickers))

  group_curves <- normal_curves_zoom %>%
    filter(ticker %in% group_tickers) %>%
    mutate(ticker = factor(ticker, levels = group_tickers))

  histogram_zoom <- ggplot(group_data, aes(x = weekly_return)) +
    geom_histogram(
      aes(y = after_stat(density)),
      binwidth = 2,
      boundary = 0,
      fill = "lightblue",
      color = "white"
    ) +
    geom_density(color = "blue", linewidth = 0.8) +
    geom_line(
      data = group_curves,
      aes(x = weekly_return, y = density),
      color = "red",
      linetype = "dashed",
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ ticker, ncol = 2, scales = "fixed") +
    coord_cartesian(xlim = c(-zoom_limit, zoom_limit)) +
    labs(
      title = paste("Rozdeleni tydennich vynosu -", group_label),
      subtitle = "Zoom se spolecnym meritkem panelu",
      x = "Tydenni vynos (%)",
      y = "Hustota",
      caption = common_caption
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())

  ggsave(
    file.path(zoom_dir, paste0(group_name, "_histogram_zoom.png")),
    histogram_zoom,
    width = 13,
    height = 9,
    dpi = 300
  )

  boxplot_zoom <- ggplot(
    group_data,
    aes(x = ticker, y = weekly_return)
  ) +
    geom_boxplot(
      fill = "lightblue",
      color = "darkblue",
      outlier.color = "red",
      outlier.alpha = 0.35,
      outlier.size = 1.2
    ) +
    geom_hline(
      yintercept = 0,
      color = "black",
      linetype = "dashed"
    ) +
    coord_cartesian(ylim = c(-zoom_limit, zoom_limit)) +
    labs(
      title = paste("Boxplot tydennich vynosu -", group_label),
      subtitle = "Zoom hlavni casti rozdeleni",
      x = "Akcie",
      y = "Tydenni vynos (%)",
      caption = common_caption
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())

  ggsave(
    file.path(zoom_dir, paste0(group_name, "_boxplot_zoom.png")),
    boxplot_zoom,
    width = 12,
    height = 7,
    dpi = 300
  )
}

cat("Zoom grafy byly ulozeny do:", zoom_dir, "\n")

