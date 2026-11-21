
# ============================================================
# STATISTIKA FINANCNICH TRHU
# STAHOVANI HISTORICKYCH DAT Z YAHOO FINANCE
# ============================================================


# ------------------------------------------------------------
# 1. NASTAVENI PROJEKTU
# ------------------------------------------------------------

setwd("C:/Users/revel/OneDrive/Desktop/StatistikaFinancichTrhu")

# Nacteni knihoven
library(quantmod)
library(tidyverse)
library(lubridate)
library(moments)

# Slozka pro surova data
raw_dir <- "data/raw"

# Vytvoreni slozky, pokud jeste neexistuje
dir.create(
  raw_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 2. SEZNAM AKCII A INDEXU
# ------------------------------------------------------------

tickers <- c(

  # Polovodice
  "NVDA",
  "AMD",
  "INTC",
  "AVGO",
  "TSM",

  # Pametove technologie
  "MU",
  "005930.KS",
  "000660.KS",
  "SNDK",

  # Kvantove pocitace
  "IONQ",
  "RGTI",
  "QBTS",
  "IBM",

  # Stabilni dividendove spolecnosti
  "KO",
  "PEP",
  "PG",
  "JNJ",

  # Trzni benchmarky
  "^GSPC",
  "^NDX"
)


# ------------------------------------------------------------
# 3. NASTAVENI OBDOBI
# ------------------------------------------------------------

start_date <- "2000-01-01"

# Yahoo Finance nezahrnuje koncove datum,
# proto pouzijeme zitrejsi datum.
end_date <- Sys.Date() + 1


# ------------------------------------------------------------
# 4. PRIPRAVA PROMENNYCH
# ------------------------------------------------------------

# Seznam pro ulozeni stazenych dat
stock_data <- list()

# Tabulka pro evidenci stahovani
download_log <- data.frame(
  ticker = character(),
  status = character(),
  observations = integer(),
  first_date = character(),
  last_date = character(),
  downloaded_at = character(),
  error = character(),
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 5. STAHOVANI DAT
# ------------------------------------------------------------

for (ticker in tickers) {

  cat("\n====================================\n")
  cat("STAHUJI:", ticker, "\n")
  cat("====================================\n")

  # Pokus o stazeni dat
  stock <- tryCatch({

    getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = start_date,
      to = end_date,
      periodicity = "weekly",
      auto.assign = FALSE
    )

  }, error = function(e) {

    cat("CHYBA:", conditionMessage(e), "\n")

    return(NULL)

  })


  # ----------------------------------------------------------
  # 6. KONTROLA STAZENYCH DAT
  # ----------------------------------------------------------

  if (is.null(stock) || NROW(stock) == 0) {

    cat("NEUSPESNE STAZENI:", ticker, "\n")

    download_log <- rbind(
      download_log,
      data.frame(
        ticker = ticker,
        status = "FAILED",
        observations = 0,
        first_date = NA_character_,
        last_date = NA_character_,
        downloaded_at = as.character(Sys.time()),
        error = "Data nebyla stazena",
        stringsAsFactors = FALSE
      )
    )

    next
  }


  # Ulozeni dat do seznamu
  stock_data[[ticker]] <- stock


  # ----------------------------------------------------------
  # 7. ULOZENI DAT DO CSV
  # ----------------------------------------------------------

  # Uprava tickeru pro nazev souboru
  filename <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    ticker
  )

  filepath <- file.path(
    raw_dir,
    paste0(filename, ".csv")
  )

  # Prevod xts objektu na beznou tabulku
  csv_data <- data.frame(
    Date = as.Date(index(stock)),
    coredata(stock)
  )

  # Ulozeni do CSV
  write.csv(
    csv_data,
    file = filepath,
    row.names = FALSE
  )


  # ----------------------------------------------------------
  # 8. ZAZNAM DO EVIDENCE
  # ----------------------------------------------------------

  download_log <- rbind(
    download_log,
    data.frame(
      ticker = ticker,
      status = "SUCCESS",
      observations = NROW(stock),
      first_date = as.character(as.Date(first(index(stock)))),
      last_date = as.character(as.Date(last(index(stock)))),
      downloaded_at = as.character(Sys.time()),
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  )


  # Vypis informaci o stazenych datech
  cat("USPESNE STAZENO:", ticker, "\n")
  cat("Pocet zaznamu:", NROW(stock), "\n")
  cat("Prvni datum:", as.character(as.Date(first(index(stock)))), "\n")
  cat("Posledni datum:", as.character(as.Date(last(index(stock)))), "\n")

  # Pauza mezi pozadavky
  Sys.sleep(1)

}


# ------------------------------------------------------------
# 9. ULOZENI EVIDENCE STAHOVANI
# ------------------------------------------------------------

write.csv(
  download_log,
  file = file.path(raw_dir, "download_log.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# 10. ULOZENI VSECH DAT VE FORMATU RDS
# ------------------------------------------------------------

saveRDS(
  stock_data,
  file = file.path(raw_dir, "all_stock_data.rds")
)


# ------------------------------------------------------------
# 11. ZAVERECNE SHRNUTI
# ------------------------------------------------------------

successful <- sum(download_log$status == "SUCCESS")

failed <- sum(download_log$status == "FAILED")

cat("\n\n")
cat("============================================\n")
cat("STAHOVANI DOKONCENO\n")
cat("============================================\n")

cat("Celkem instrumentu:", length(tickers), "\n")
cat("Uspesne stazeno:", successful, "\n")
cat("Neuspesne:", failed, "\n")

cat("Data ulozena ve slozce:", raw_dir, "\n")

cat("\nPREHLED STAHOVANI:\n")

print(download_log)

cat("\n============================================\n")
cat("KONEC PROGRAMU\n")
cat("============================================\n")



# ============================================================
# KONTROLA CHYBEJICICH HODNOT
# ============================================================

missing_report <- data.frame()

for (ticker in names(stock_data)) {

  stock <- stock_data[[ticker]]

  # Pocet chybejicich hodnot v jednotlivych sloupcich
  missing_counts <- colSums(is.na(stock))

  # Celkovy pocet chybejicich hodnot
  total_missing <- sum(missing_counts)

  # Pocet radku, ve kterych chybi alespon jedna hodnota
  incomplete_rows <- sum(!complete.cases(stock))

  # Pridani vysledku do reportu
  missing_report <- rbind(
    missing_report,
    data.frame(
      ticker = ticker,
      observations = NROW(stock),
      missing_values = total_missing,
      incomplete_rows = incomplete_rows
    )
  )

  # Vypis pouze instrumentu s chybejicimi hodnotami
  if (total_missing > 0) {

    cat("\n====================================\n")
    cat("INSTRUMENT:", ticker, "\n")
    cat("====================================\n")

    print(missing_counts[missing_counts > 0])

    # Vypis prvnich 10 dat, kde neco chybi
    missing_dates <- index(stock)[
      !complete.cases(stock)
    ]

    cat("\nPrvni chybejici zaznamy:\n")
    print(head(missing_dates, 10))
  }
}


# ------------------------------------------------------------
# SOUHRN
# ------------------------------------------------------------

cat("\n====================================\n")
cat("SOUHRN CHYBEJICICH HODNOT\n")
cat("====================================\n")

print(missing_report)

# Ulozeni reportu
write.csv(
  missing_report,
  "data/raw/missing_values_report.csv",
  row.names = FALSE
)




# ============================================================
# 1. PRIPRAVA ZPRACOVANYCH DAT
# ============================================================

library(quantmod)

# Cesty ke slozkam
raw_dir <- "data/raw"
processed_dir <- "data/processed"

# Vytvoreni slozky processed
dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. NACTENI STAZENYCH DAT
# ============================================================

stock_data <- readRDS(
  file.path(raw_dir, "all_stock_data.rds")
)


# ============================================================
# 3. CISTENI DAT
# ============================================================

clean_stock_data <- list()

cleaning_report <- data.frame(
  ticker = character(),
  original_rows = integer(),
  removed_rows = integer(),
  remaining_rows = integer(),
  missing_values = integer(),
  stringsAsFactors = FALSE
)


for (ticker in names(stock_data)) {

  cat("\nZpracovavam:", ticker, "\n")

  stock <- stock_data[[ticker]]

  # Pocet puvodnich radku
  original_rows <- NROW(stock)

  # Odstraneni pouze radku, kde chybi vsechny hodnoty
  # (napriklad neobchodovany posledni tyden)
  stock_clean <- stock[
    !apply(is.na(stock), 1, all)
  ]

  # Pocet odstraněnych radku
  removed_rows <- original_rows - NROW(stock_clean)

  # Pocet zbyvajicich chybejicich hodnot
  remaining_missing <- sum(is.na(stock_clean))

  # Ulozeni vycistenych dat do seznamu
  clean_stock_data[[ticker]] <- stock_clean


  # Prevod tickeru na nazev souboru
  filename <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    ticker
  )

  # Prevod na beznou tabulku
  csv_data <- data.frame(
    Date = as.Date(index(stock_clean)),
    coredata(stock_clean)
  )

  # Ulozeni CSV
  write.csv(
    csv_data,
    file = file.path(
      processed_dir,
      paste0(filename, ".csv")
    ),
    row.names = FALSE
  )


  # Zaznam do reportu
  cleaning_report <- rbind(
    cleaning_report,
    data.frame(
      ticker = ticker,
      original_rows = original_rows,
      removed_rows = removed_rows,
      remaining_rows = NROW(stock_clean),
      missing_values = remaining_missing,
      stringsAsFactors = FALSE
    )
  )

  cat("Puvodni radky:", original_rows, "\n")
  cat("Odstranene radky:", removed_rows, "\n")
  cat("Zbyvajici radky:", NROW(stock_clean), "\n")
  cat("Chybejici hodnoty:", remaining_missing, "\n")

}


# ============================================================
# 4. ULOZENI VYCISTENYCH DAT
# ============================================================

saveRDS(
  clean_stock_data,
  file = file.path(
    processed_dir,
    "all_stock_data_clean.rds"
  )
)


# ============================================================
# 5. ULOZENI REPORTU
# ============================================================

write.csv(
  cleaning_report,
  file = file.path(
    processed_dir,
    "cleaning_report.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 6. ZAVERECNY VYPIS
# ============================================================

cat("\n====================================\n")
cat("CISTENI DAT DOKONCENO\n")
cat("====================================\n")

print(cleaning_report)

cat("\nVycistena data jsou ulozena v:", processed_dir, "\n")




# ==========================================
# 02 - KONTROLA CENOVYCH DAT
# ==========================================

library(quantmod)

# Nacteni ocistenych dat
stock_data <- readRDS(
  "data/processed/all_stock_data_clean.rds"
)

# Tabulka pro vysledky kontroly
price_report <- data.frame(
  ticker = character(),
  adjusted_column = character(),
  close_column = character(),
  adjusted_missing = integer(),
  close_missing = integer(),
  adjusted_equals_close = logical(),
  first_price = numeric(),
  last_price = numeric(),
  stringsAsFactors = FALSE
)

# Kontrola kazdeho instrumentu
for (ticker in names(stock_data)) {

  stock <- stock_data[[ticker]]

  # Najdeme sloupce Adjusted a Close
  adjusted_col <- grep(
    "Adjusted$",
    colnames(stock),
    value = TRUE
  )

  close_col <- grep(
    "Close$",
    colnames(stock),
    value = TRUE
  )

  # Overime existenci obou sloupcu
  if (length(adjusted_col) != 1 ||
      length(close_col) != 1) {

    stop(paste(
      "Chybi nebo je vice odpovidajicich sloupcu u:",
      ticker
    ))
  }

  adjusted <- stock[, adjusted_col]
  close <- stock[, close_col]

  # Pocet chybejicich hodnot
  adjusted_missing <- sum(is.na(adjusted))
  close_missing <- sum(is.na(close))

  # Zjistime, zda jsou ceny totozne
  same_prices <- isTRUE(
    all.equal(
      as.numeric(adjusted),
      as.numeric(close)
    )
  )

  # Pridani vysledku do reportu
  price_report <- rbind(
    price_report,
    data.frame(
      ticker = ticker,
      adjusted_column = adjusted_col,
      close_column = close_col,
      adjusted_missing = adjusted_missing,
      close_missing = close_missing,
      adjusted_equals_close = same_prices,
      first_price = as.numeric(first(adjusted)),
      last_price = as.numeric(last(adjusted)),
      stringsAsFactors = FALSE
    )
  )
}

# Ulozeni reportu
write.csv(
  price_report,
  "data/processed/price_check_report.csv",
  row.names = FALSE
)

# Vypis vysledku
print(price_report)

cat("\nKONTROLA CEN DOKONCENA\n")




# ==========================================
# KONTROLA SK HYNIX
# ==========================================

stock_data <- readRDS(
  "data/processed/all_stock_data_clean.rds"
)

hynix <- stock_data[["000660.KS"]]

# Zobrazime prvnich 15 zaznamu
cat("PRVNICH 15 ZAZNAMU:\n")
print(head(hynix[, c("000660.KS.Close",
                     "000660.KS.Adjusted")], 15))

# Zobrazime poslednich 10 zaznamu
cat("\nPOSLEDNICH 10 ZAZNAMU:\n")
print(tail(hynix[, c("000660.KS.Close",
                     "000660.KS.Adjusted")], 10))

# Zakladni statistiky
cat("\nMINIMALNI CLOSE:\n")
print(min(hynix$`000660.KS.Close`, na.rm = TRUE))

cat("\nMINIMALNI ADJUSTED:\n")
print(min(hynix$`000660.KS.Adjusted`, na.rm = TRUE))

cat("\nPOCET ZAPORNYCH ADJUSTED HODNOT:\n")
print(sum(hynix$`000660.KS.Adjusted` < 0, na.rm = TRUE))




# ==========================================
# 03 - VYPOCET TYDENNICH VYNOSU
# ==========================================

library(quantmod)

# Nacteni ocistenych cenovych dat
stock_data <- readRDS(
  "data/processed/all_stock_data_clean.rds"
)

# Slozka pro vysledky
returns_dir <- "data/processed/returns"

dir.create(
  returns_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Seznam pro ulozeni vynosu
return_data <- list()

# Report vysledku
returns_report <- data.frame(
  ticker = character(),
  observations = integer(),
  first_date = as.Date(character()),
  last_date = as.Date(character()),
  mean_return = numeric(),
  sd_return = numeric(),
  min_return = numeric(),
  max_return = numeric(),
  stringsAsFactors = FALSE
)

# Zpracovani jednotlivych instrumentu
for (ticker in names(stock_data)) {

  cat("\nZpracovavam:", ticker, "\n")

  stock <- stock_data[[ticker]]

  # Najdeme sloupec Close
  close_col <- grep(
    "Close$",
    colnames(stock),
    value = TRUE
  )

  if (length(close_col) != 1) {
    stop(paste("Problem se sloupcem Close:", ticker))
  }

  # Extrakce uzaviracich cen
  prices <- as.numeric(stock[, close_col])

  # Kontrola cen
  if (any(is.na(prices)) || any(prices <= 0)) {
    stop(paste("Neplatne ceny u:", ticker))
  }

  # Vypocet tydennich procentualnich vynosu
  returns <- c(
    NA_real_,
    (prices[-1] / prices[-length(prices)] - 1) * 100
  )

  # Vytvoreni casove rady
  returns_xts <- xts::xts(
    returns,
    order.by = index(stock)
  )

  colnames(returns_xts) <- ticker

  # Prvni vynos nelze vypocitat - odstraneni prvniho NA
  returns_xts <- returns_xts[-1, , drop = FALSE]

  # Ulozeni do seznamu
  return_data[[ticker]] <- returns_xts

  # Ulozeni samostatneho CSV
  filename <- gsub("[^A-Za-z0-9_-]", "_", ticker)

  csv_data <- data.frame(
    Date = as.Date(index(returns_xts)),
    Return = as.numeric(returns_xts)
  )

  write.csv(
    csv_data,
    file = file.path(
      returns_dir,
      paste0(filename, "_returns.csv")
    ),
    row.names = FALSE
  )

  # Statistiky vynosu
  returns_values <- as.numeric(returns_xts)

  returns_report <- rbind(
    returns_report,
    data.frame(
      ticker = ticker,
      observations = length(returns_values),
      first_date = as.Date(first(index(returns_xts))),
      last_date = as.Date(last(index(returns_xts))),
      mean_return = mean(returns_values),
      sd_return = sd(returns_values),
      min_return = min(returns_values),
      max_return = max(returns_values),
      stringsAsFactors = FALSE
    )
  )

  cat("Pocet vynosu:", length(returns_values), "\n")
}

# Ulozeni vsech casovych rad
saveRDS(
  return_data,
  file = file.path(returns_dir, "all_returns.rds")
)

# Ulozeni statistickeho reportu
write.csv(
  returns_report,
  file = file.path(returns_dir, "returns_report.csv"),
  row.names = FALSE
)

cat("\n====================================\n")
cat("VYPOCET VYNOSU DOKONCEN\n")
cat("====================================\n")

print(returns_report)




# ==========================================
# 04 - POPISNA STATISTIKA JEDNOTLIVYCH AKCII
# ==========================================

library(quantmod)
library(moments)

# Nacteni vypocitanych tydennich vynosu
return_data <- readRDS(
  "data/processed/returns/all_returns.rds"
)

# Slozka pro vysledky
stats_dir <- "data/processed/statistics"

dir.create(
  stats_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Tabulka pro popisnou statistiku
descriptive_stats <- data.frame(
  ticker = character(),
  observations = integer(),
  first_date = as.Date(character()),
  last_date = as.Date(character()),
  mean = numeric(),
  variance = numeric(),
  standard_deviation = numeric(),
  skewness = numeric(),
  excess_kurtosis = numeric(),
  minimum = numeric(),
  maximum = numeric(),
  stringsAsFactors = FALSE
)

# Zpracovani kazde akcie
for (ticker in names(return_data)) {

  cat("\nAnalyzuji:", ticker, "\n")

  # Nacteni casove rady vynosu
  returns_xts <- return_data[[ticker]]

  # Prevod na numericky vektor
  values <- as.numeric(returns_xts)

  # Kontrola dat
  if (any(is.na(values))) {
    stop(paste("Chybejici hodnoty u:", ticker))
  }

  if (length(values) < 2) {
    stop(paste("Nedostatek pozorovani u:", ticker))
  }

  # Vypocet statistickych charakteristik
  average <- mean(values)

  variance_value <- var(values)

  standard_deviation_value <- sd(values)

  skewness_value <- moments::skewness(values)

  # moments::kurtosis vraci klasickou spicatost,
  # proto odecitame 3 a ziskame excesni spicatost.
  excess_kurtosis_value <- moments::kurtosis(values) - 3

  minimum_value <- min(values)

  maximum_value <- max(values)

  # Pridani vysledku
  descriptive_stats <- rbind(
    descriptive_stats,
    data.frame(
      ticker = ticker,
      observations = length(values),
      first_date = as.Date(first(index(returns_xts))),
      last_date = as.Date(last(index(returns_xts))),
      mean = average,
      variance = variance_value,
      standard_deviation = standard_deviation_value,
      skewness = skewness_value,
      excess_kurtosis = excess_kurtosis_value,
      minimum = minimum_value,
      maximum = maximum_value
    )
  )

  cat("Prumer:", average, "\n")
  cat("Rozptyl:", variance_value, "\n")
  cat("Smerodatna odchylka:", standard_deviation_value, "\n")
  cat("Sikmost:", skewness_value, "\n")
  cat("Excesni spicatost:", excess_kurtosis_value, "\n")
  cat("Minimum:", minimum_value, "\n")
  cat("Maximum:", maximum_value, "\n")
}

# Ulozeni vysledku do CSV
write.csv(
  descriptive_stats,
  file = file.path(
    stats_dir,
    "individual_descriptive_statistics.csv"
  ),
  row.names = FALSE
)

# Ulozeni i do RDS pro dalsi zpracovani
saveRDS(
  descriptive_stats,
  file = file.path(
    stats_dir,
    "individual_descriptive_statistics.rds"
  )
)

# Vypis vysledne tabulky
cat("\n========================================\n")
cat("POPISNA STATISTIKA DOKONCENA\n")
cat("========================================\n")

print(descriptive_stats)





# ==========================================
# 06 - HISTOGRAMY, BOXPLOTY A BODOVE GRAFY
# ==========================================

library(ggplot2)
library(dplyr)
library(quantmod)

# Nacteni tydennich vynosu
return_data <- readRDS(
  "data/processed/returns/all_returns.rds"
)

# Slozka pro grafy
plots_dir <- "data/processed/statistics/graphs"

dir.create(
  plots_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==========================================
# 1. PRIPRAVA DAT
# ==========================================

returns_long <- bind_rows(
  lapply(names(return_data), function(ticker) {

    returns_xts <- return_data[[ticker]]

    data.frame(
      ticker = ticker,
      Date = as.Date(index(returns_xts)),
      return = as.numeric(returns_xts)
    )
  })
)

# ==========================================
# 2. HISTOGRAM S HUSTOTOU A NORMALNIM
#    ROZDELENIM
# ==========================================

# Histogram bude mit na ose Y hustotu,
# aby bylo mozne porovnat obe krivky.

p_histogram_curves <- ggplot(
  returns_long,
  aes(x = return)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "lightblue",
    color = "white"
  ) +
  geom_density(
    color = "blue",
    linewidth = 0.9
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(returns_long$return),
      sd = sd(returns_long$return)
    ),
    color = "red",
    linetype = "dashed",
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ ticker,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Rozdeleni tydennich vynosu",
    subtitle = paste(
      "Modra: empiricka hustota |",
      "Cervena: normalni rozdeleni"
    ),
    x = "Tydenni vynos (%)",
    y = "Hustota"
  ) +
  theme_minimal()

# Ulozeni histogramu
ggsave(
  file.path(plots_dir, "08_histograms_with_curves.png"),
  p_histogram_curves,
  width = 16,
  height = 14,
  dpi = 300
)


# ==========================================
# 3. BOXPLOT
# ==========================================

p_boxplot <- ggplot(
  returns_long,
  aes(
    x = reorder(ticker, return, FUN = median),
    y = return
  )
) +
  geom_boxplot(
    fill = "lightblue",
    color = "darkblue",
    outlier.color = "red",
    outlier.alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Boxplot tydennich vynosu",
    subtitle = "Porovnani rozdeleni a extremnich hodnot",
    x = "Instrument",
    y = "Tydenni vynos (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "09_boxplot.png"),
  p_boxplot,
  width = 12,
  height = 9,
  dpi = 300
)


# ==========================================
# 4. BODOVY GRAF V CASE
# ==========================================

p_scatter <- ggplot(
  returns_long,
  aes(
    x = Date,
    y = return
  )
) +
  geom_point(
    color = "steelblue",
    alpha = 0.5,
    size = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ ticker,
    scales = "free_x",
    ncol = 3
  ) +
  labs(
    title = "Vyvoj tydennich vynosu v case",
    subtitle = "Kazdy bod predstavuje jeden tyden",
    x = "Datum",
    y = "Tydenni vynos (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "10_scatter_returns_over_time.png"),
  p_scatter,
  width = 16,
  height = 14,
  dpi = 300
)


# ==========================================
# 5. KONEC
# ==========================================

cat("\n====================================\n")
cat("DALSÍ GRAFY BYLY VYTVORENY\n")
cat("====================================\n")

print(list.files(plots_dir))

# Zobrazeni grafu v R
print(p_histogram_curves)
print(p_boxplot)
print(p_scatter)



# ==========================================
# 2. HISTOGRAMY S KRIVKAMI PRO KAZDOU AKCII
# ==========================================

p_histogram_curves <- ggplot(
  returns_long,
  aes(x = return)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "lightblue",
    color = "white"
  ) +
  geom_density(
    color = "blue",
    linewidth = 0.9
  ) +
  stat_function(
    fun = function(x) {
      dnorm(
        x,
        mean = mean(returns_long$return),
        sd = sd(returns_long$return)
      )
    },
    color = "red",
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ ticker,
    scales = "free",
    ncol = 3
  ) +
  labs(
    title = "Rozdeleni tydennich vynosu",
    subtitle = "Modra: empiricka hustota | Cervena: normalni rozdeleni",
    x = "Tydenni vynos (%)",
    y = "Hustota"
  ) +
  theme_minimal()





library(ggplot2)
library(dplyr)
library(quantmod)

# =====================================================
# 1. NACHTENI DAT
# =====================================================

return_data <- readRDS("data/processed/returns/all_returns.rds")

# Hlavni slozka pro grafy
base_dir <- "data/processed/statistics/graphs"

# =====================================================
# 2. ROZDELENI AKCII DO SKUPIN
# =====================================================

groups <- list(
  semiconductors = c("NVDA", "AMD", "INTC", "AVGO", "TSM"),

  memory_technologies = c(
    "MU", "005930.KS", "000660.KS", "SNDK"
  ),

  quantum_computing = c(
    "IONQ", "RGTI", "QBTS", "IBM"
  ),

  stable_companies = c(
    "KO", "PEP", "PG", "JNJ"
  )
)

# =====================================================
# 3. PREVOD DAT DO TABULKY
# =====================================================

returns_long <- bind_rows(
  lapply(names(return_data), function(ticker) {

    x <- return_data[[ticker]]

    data.frame(
      ticker = ticker,
      Date = as.Date(index(x)),
      return = as.numeric(x)
    )
  })
)

# =====================================================
# 4. VYTVORENI GRAFU PRO KAZDOU SKUPINU
# =====================================================

for (group_name in names(groups)) {

  # Vybereme akcie patrici do aktualni skupiny
  group_tickers <- groups[[group_name]]

  group_data <- returns_long %>%
    filter(ticker %in% group_tickers) %>%
    mutate(ticker = factor(ticker, levels = group_tickers))

  # Vytvoreni slozek
  group_dir <- file.path(base_dir, group_name)
  scatter_dir <- file.path(base_dir, "scatter", group_name)

  dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(scatter_dir, recursive = TRUE, showWarnings = FALSE)

  # ===================================================
  # 4A. HISTOGRAM S EMPIRICKOU A NORMALNI KRIVKOU
  # ===================================================

  normal_curves <- bind_rows(
    lapply(split(group_data, group_data$ticker), function(df) {

      m <- mean(df$return)
      s <- sd(df$return)

      x_values <- seq(
        min(df$return),
        max(df$return),
        length.out = 300
      )

      data.frame(
        ticker = df$ticker[1],
        return = x_values,
        density = dnorm(x_values, mean = m, sd = s)
      )
    })
  )

  histogram <- ggplot(group_data, aes(x = return)) +

    geom_histogram(
      aes(y = after_stat(density)),
      bins = 40,
      fill = "lightblue",
      color = "white"
    ) +

    geom_density(
      color = "blue",
      linewidth = 0.8
    ) +

    geom_line(
      data = normal_curves,
      aes(x = return, y = density),
      color = "red",
      linetype = "dashed",
      linewidth = 0.8
    ) +

    facet_wrap(
      ~ticker,
      scales = "free",
      ncol = 2
    ) +

    labs(
      title = paste("Rozdělení týdenních výnosů –", group_name),
      subtitle = "Modrá: empirická hustota | Červená: normální rozdělení",
      x = "Týdenní výnos (%)",
      y = "Hustota"
    ) +

    theme_minimal()

  ggsave(
    filename = file.path(group_dir, "histogram.png"),
    plot = histogram,
    width = 12,
    height = 8,
    dpi = 300
  )

  # ===================================================
  # 4B. VERTIKALNI BOXPLOT
  # ===================================================

  boxplot <- ggplot(
    group_data,
    aes(x = ticker, y = return)
  ) +

    geom_boxplot(
      fill = "lightblue",
      color = "darkblue",
      outlier.color = "red",
      outlier.alpha = 0.5
    ) +

    geom_hline(
      yintercept = 0,
      color = "red",
      linetype = "dashed"
    ) +

    labs(
      title = paste("Boxplot týdenních výnosů –", group_name),
      x = "Akcie",
      y = "Týdenní výnos (%)"
    ) +

    theme_minimal()

  ggsave(
    filename = file.path(group_dir, "boxplot.png"),
    plot = boxplot,
    width = 12,
    height = 7,
    dpi = 300
  )

  # ===================================================
  # 4C. SAMOSTATNY SCATTER PLOT PRO KAZDOU AKCII
  # ===================================================

  for (ticker_name in group_tickers) {

    stock_data <- group_data %>%
      filter(ticker == ticker_name)

    scatter <- ggplot(
      stock_data,
      aes(x = Date, y = return)
    ) +

      geom_point(
        color = "steelblue",
        alpha = 0.6,
        size = 1
      ) +

      geom_hline(
        yintercept = 0,
        color = "red",
        linetype = "dashed"
      ) +

      labs(
        title = paste("Vývoj týdenních výnosů –", ticker_name),
        x = "Datum",
        y = "Týdenní výnos (%)"
      ) +

      theme_minimal()

    ggsave(
      filename = file.path(
        scatter_dir,
        paste0(ticker_name, ".png")
      ),
      plot = scatter,
      width = 12,
      height = 6,
      dpi = 300
    )
  }

  cat("Dokoncena skupina:", group_name, "\n")
}

cat("Vsechny grafy byly vytvoreny.\n")




library(ggplot2)
library(dplyr)
library(lubridate)
library(quantmod)

# =====================================================
# 1. NACHTENI TYDENNICH VYNOSU
# =====================================================

return_data <- readRDS(
  "data/processed/returns/all_returns.rds"
)

# =====================================================
# 2. DEFINICE ODVETVI
# =====================================================

groups <- list(

  semiconductors = c(
    "NVDA", "AMD", "INTC", "AVGO", "TSM"
  ),

  memory_technologies = c(
    "MU", "005930.KS", "000660.KS", "SNDK"
  ),

  quantum_computing = c(
    "IONQ", "RGTI", "QBTS", "IBM"
  ),

  stable_companies = c(
    "KO", "PEP", "PG", "JNJ"
  )
)

# =====================================================
# 3. PREVOD DAT DO TABULKY
# =====================================================

returns_long <- bind_rows(
  lapply(names(return_data), function(ticker) {

    x <- return_data[[ticker]]

    data.frame(
      ticker = ticker,
      Date = as.Date(index(x)),
      weekly_return = as.numeric(x)
    )
  })
)

# =====================================================
# 4. VYPOCET MESICNICH VYNOSU
# =====================================================

monthly_returns <- returns_long %>%

  mutate(
    month = floor_date(Date, unit = "month")
  ) %>%

  group_by(ticker, month) %>%

  summarise(
    monthly_return = (
      prod(1 + weekly_return / 100) - 1
    ) * 100,

    .groups = "drop"
  )

# =====================================================
# 5. SLOZKA PRO VYSLEDNE GRAFY
# =====================================================

output_dir <- "data/processed/statistics/graphs/time_series/monthly_returns"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# 6. VYTVORENI CASOVYCH RAD PRO JEDNOTLIVA ODVETVI
# =====================================================

for (group_name in names(groups)) {

  # Vybereme akcie aktualniho odvetvi

  group_data <- monthly_returns %>%

    filter(ticker %in% groups[[group_name]]) %>%

    mutate(
      ticker = factor(
        ticker,
        levels = groups[[group_name]]
      )
    ) %>%

    arrange(month)

  # Vytvoreni grafu

  p <- ggplot(
    group_data,
    aes(
      x = month,
      y = monthly_return,
      color = ticker,
      group = ticker
    )
  ) +

    geom_line(
      linewidth = 0.7,
      alpha = 0.85
    ) +

    geom_hline(
      yintercept = 0,
      color = "black",
      linetype = "dashed",
      linewidth = 0.5
    ) +

    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y"
    ) +

    labs(
      title = paste(
        "Časová řada měsíčních výnosů –",
        group_name
      ),

      subtitle = "Každá čára představuje jednu společnost",

      x = "Datum",
      y = "Měsíční výnos (%)",
      color = "Společnost"
    ) +

    theme_minimal() +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),

      legend.position = "bottom",

      legend.title = element_text(
        face = "bold"
      ),

      legend.text = element_text(
        size = 10
      ),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )

  # Ulozeni grafu

  ggsave(
    filename = file.path(
      output_dir,
      paste0(group_name, "_monthly_returns.png")
    ),

    plot = p,
    width = 14,
    height = 7,
    dpi = 300
  )

  cat(
    "Vytvoren graf:",
    group_name,
    "\n"
  )
}

cat("Vsechny mesicni casove rady byly vytvoreny.\n")



library(ggplot2)
library(dplyr)
library(lubridate)
library(quantmod)

# =====================================================
# 1. NACHTENI DAT
# =====================================================

return_data <- readRDS(
  "data/processed/returns/all_returns.rds"
)

# =====================================================
# 2. DEFINICE ODVETVI
# =====================================================

groups <- list(

  semiconductors = c(
    "NVDA", "AMD", "INTC", "AVGO", "TSM"
  ),

  memory_technologies = c(
    "MU", "005930.KS", "000660.KS", "SNDK"
  ),

  quantum_computing = c(
    "IONQ", "RGTI", "QBTS", "IBM"
  ),

  stable_companies = c(
    "KO", "PEP", "PG", "JNJ"
  )
)

# =====================================================
# 3. PREVOD DAT DO TABULKY
# =====================================================

returns_long <- bind_rows(
  lapply(names(return_data), function(ticker) {

    x <- return_data[[ticker]]

    data.frame(
      ticker = ticker,
      Date = as.Date(index(x)),
      weekly_return = as.numeric(x)
    )
  })
) %>%
  filter(!is.na(weekly_return)) %>%
  arrange(ticker, Date)

# =====================================================
# 4. VYSTUPNI SLOZKY
# =====================================================

base_dir <- paste0(
  "data/processed/statistics/graphs/risk_analysis"
)

dir.create(
  base_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# 5. ROLLING VOLATILITY
# =====================================================

# Rizikovost vypocitana z poslednich 12 tydnu.
# Vysledkem je smerodatna odchylka v procentech.

rolling_data <- returns_long %>%

  group_by(ticker) %>%

  group_modify(~ {

    stock_data <- arrange(.x, Date)

    stock_data$rolling_volatility <- zoo::rollapplyr(
      stock_data$weekly_return,
      width = 12,
      FUN = sd,
      fill = NA,
      align = "right"
    )

    stock_data
  }) %>%

  ungroup() %>%

  filter(!is.na(rolling_volatility))

# =====================================================
# 6. ANALYZA JEDNOTLIVYCH ODVETVI
# =====================================================

for (group_name in names(groups)) {

  cat("\nZpracovavam odvetvi:", group_name, "\n")

  tickers <- groups[[group_name]]

  group_data <- returns_long %>%
    filter(ticker %in% tickers)

  # ---------------------------------------------------
  # 6A. SPOLECNE CASOVE OBDOBI
  # ---------------------------------------------------

  # Najdeme datum, od ktereho maji vsechny akcie data.
  # Tim zajistime spravedlivejsi porovnani.

  date_ranges <- group_data %>%

    group_by(ticker) %>%

    summarise(
      first_date = min(Date),
      last_date = max(Date),
      .groups = "drop"
    )

  common_start <- max(date_ranges$first_date)
  common_end <- min(date_ranges$last_date)

  comparable_data <- group_data %>%

    filter(
      Date >= common_start,
      Date <= common_end
    )

  cat(
    "Spolecne obdobi:",
    as.character(common_start),
    "az",
    as.character(common_end),
    "\n"
  )

  # ---------------------------------------------------
  # 6B. RISK - RETURN
  # ---------------------------------------------------

  risk_return <- comparable_data %>%

    group_by(ticker) %>%

    summarise(
      average_return = mean(weekly_return),
      volatility = sd(weekly_return),
      observations = n(),
      .groups = "drop"
    )

  p_risk_return <- ggplot(
    risk_return,
    aes(
      x = volatility,
      y = average_return,
      color = ticker
    )
  ) +

    geom_point(size = 4) +

    geom_text(
      aes(label = ticker),
      nudge_y = 0.05,
      show.legend = FALSE,
      size = 3.5
    ) +

    labs(
      title = paste(
        "Vztah rizika a výnosnosti –",
        group_name
      ),

      subtitle = paste(
        "Spolecne obdobi:",
        common_start,
        "-",
        common_end
      ),

      x = "Rizikovost – směrodatná odchylka (%)",
      y = "Průměrný týdenní výnos (%)",
      color = "Společnost"
    ) +

    theme_minimal() +

    theme(
      plot.title = element_text(
        face = "bold"
      ),

      legend.position = "bottom"
    )

  ggsave(
    file.path(
      base_dir,
      paste0(group_name, "_risk_return.png")
    ),
    p_risk_return,
    width = 11,
    height = 7,
    dpi = 300
  )

  # Ulozeni vypoctenych hodnot

  write.csv(
    risk_return,
    file.path(
      base_dir,
      paste0(group_name, "_risk_return.csv")
    ),
    row.names = FALSE
  )

  # ---------------------------------------------------
  # 6C. ROLLING VOLATILITY
  # ---------------------------------------------------

  volatility_data <- rolling_data %>%

    filter(ticker %in% tickers)

  p_volatility <- ggplot(
    volatility_data,
    aes(
      x = Date,
      y = rolling_volatility,
      color = ticker,
      group = ticker
    )
  ) +

    geom_line(
      linewidth = 0.7,
      alpha = 0.85
    ) +

    labs(
      title = paste(
        "Vývoj rizikovosti –",
        group_name
      ),

      subtitle = "Klouzavá směrodatná odchylka posledních 12 týdnů",

      x = "Datum",
      y = "Rizikovost (%)",
      color = "Společnost"
    ) +

    theme_minimal() +

    theme(
      plot.title = element_text(
        face = "bold"
      ),

      legend.position = "bottom"
    )

  ggsave(
    file.path(
      base_dir,
      paste0(group_name, "_rolling_volatility.png")
    ),
    p_volatility,
    width = 14,
    height = 7,
    dpi = 300
  )

  # ---------------------------------------------------
  # 6D. ROCNI RISK - RETURN
  # ---------------------------------------------------

  annual_data <- comparable_data %>%

    mutate(
      year = year(Date)
    ) %>%

    group_by(ticker, year) %>%

    summarise(
      average_return = mean(weekly_return),
      volatility = sd(weekly_return),
      observations = n(),
      .groups = "drop"
    )

  p_annual <- ggplot(
    annual_data,
    aes(
      x = volatility,
      y = average_return,
      color = year,
      group = ticker
    )
  ) +

    geom_path(
      color = "grey60",
      linewidth = 0.6
    ) +

    geom_point(
      size = 3
    ) +

    geom_text(
      aes(label = year),
      size = 2.5,
      vjust = -0.9,
      show.legend = FALSE
    ) +

    facet_wrap(
      ~ticker
    ) +

    scale_color_gradient(
      low = "blue",
      high = "red"
    ) +

    labs(
      title = paste(
        "Roční vývoj vztahu rizika a výnosnosti –",
        group_name
      ),

      subtitle = "Každý bod představuje jeden kalendářní rok",

      x = "Rizikovost – směrodatná odchylka (%)",
      y = "Průměrný týdenní výnos (%)",
      color = "Rok"
    ) +

    theme_minimal() +

    theme(
      plot.title = element_text(
        face = "bold"
      ),

      legend.position = "bottom"
    )

  ggsave(
    file.path(
      base_dir,
      paste0(group_name, "_annual_risk_return.png")
    ),
    p_annual,
    width = 14,
    height = 9,
    dpi = 300
  )

  # Ulozeni rocni statistiky

  write.csv(
    annual_data,
    file.path(
      base_dir,
      paste0(group_name, "_annual_risk_return.csv")
    ),
    row.names = FALSE
  )

  cat("Dokonceno:", group_name, "\n")
}

cat("\nVsechny grafy rizikovosti byly vytvoreny.\n")





# ============================================================
# 09 - SOUHRNNA MAPA RIZIKOVOSTI A VYNOSNOSTI AKCII
# ============================================================

library(tidyverse)
library(ggplot2)
library(xts)
library(zoo)

# ------------------------------------------------------------
# 1. PRACOVNI ADRESAR
# ------------------------------------------------------------

setwd("C:/Users/revel/OneDrive/Desktop/StatistikaFinancichTrhu")

# ------------------------------------------------------------
# 2. NACITANI DAT
# ------------------------------------------------------------

returns_list <- readRDS(
  "data/processed/returns/all_returns.rds"
)

# ------------------------------------------------------------
# 3. PREVOD SEZNAMU CASOVYCH RAD NA JEDNU TABULKU
# ------------------------------------------------------------

returns_data <- map_dfr(
  names(returns_list),
  function(ticker_name) {

    stock_returns <- returns_list[[ticker_name]]

    if (inherits(stock_returns, "xts") ||
        inherits(stock_returns, "zoo")) {

      stock_data <- tibble(
        Date = as.Date(zoo::index(stock_returns)),
        weekly_return = as.numeric(
          zoo::coredata(stock_returns)[, 1]
        )
      )

    } else if (is.data.frame(stock_returns)) {

      stock_data <- as_tibble(stock_returns)

      date_column <- intersect(
        c("Date", "date", "index"),
        names(stock_data)
      )[1]

      return_column <- intersect(
        c("return", "Return", "returns", "Returns"),
        names(stock_data)
      )[1]

      if (is.na(date_column) || is.na(return_column)) {
        stop(paste("Neznamy format dat pro:", ticker_name))
      }

      stock_data <- tibble(
        Date = as.Date(stock_data[[date_column]]),
        weekly_return = as.numeric(stock_data[[return_column]])
      )

    } else {
      stop(paste("Nepodporovany format dat pro:", ticker_name))
    }

    stock_data %>%
      mutate(ticker = ticker_name) %>%
      select(ticker, Date, weekly_return)
  }
)

returns_data <- returns_data %>%
  filter(
    !is.na(Date),
    !is.na(weekly_return)
  )

# ------------------------------------------------------------
# 4. DEFINICE SKUPIN
# ------------------------------------------------------------

groups <- tibble(
  ticker = c(
    "NVDA", "AMD", "INTC", "AVGO", "TSM",
    "MU", "005930.KS", "000660.KS", "SNDK",
    "IONQ", "RGTI", "QBTS",
    "IBM", "KO", "PEP", "PG", "JNJ",
    "^GSPC", "^NDX"
  ),

  group = c(
    rep("Polovodiče", 5),
    rep("Paměťové technologie", 4),
    rep("Kvantové počítače", 3),
    rep("Stabilní společnosti", 5),
    rep("Tržní indexy", 2)
  )
)

# ------------------------------------------------------------
# 5. VYPOCET STATISTIK
# ------------------------------------------------------------

statistics <- returns_data %>%
  group_by(ticker) %>%
  arrange(Date, .by_group = TRUE) %>%
  summarise(

    observations = n(),

    first_date = min(Date),
    last_date = max(Date),

    years = as.numeric(last_date - first_date) / 365.25,

    growth_factor = prod(1 + weekly_return / 100),

    cumulative_return = (growth_factor - 1) * 100,

    CAGR = if_else(
      years > 0 & growth_factor > 0,
      (growth_factor^(1 / years) - 1) * 100,
      NA_real_
    ),

    annualized_volatility = sd(weekly_return) * sqrt(52),

    .groups = "drop"
  ) %>%
  left_join(groups, by = "ticker") %>%
  filter(
    !is.na(CAGR),
    !is.na(annualized_volatility)
  )

# ------------------------------------------------------------
# 6. VYTVORENI VYSTUPNI SLOZKY
# ------------------------------------------------------------

output_dir <- "data/processed/statistics/risk_return"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 7. ULOZENI TABULKY
# ------------------------------------------------------------

write_csv(
  statistics,
  file.path(output_dir, "risk_return_data.csv")
)

# ------------------------------------------------------------
# 8. VYTVORENI GRAFU
# ------------------------------------------------------------

risk_return_plot <- ggplot(
  statistics,
  aes(
    x = CAGR,
    y = annualized_volatility,
    color = group
  )
) +

  geom_vline(
    xintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.7
  ) +

  geom_point(
    size = 4,
    alpha = 0.85
  ) +

  geom_text(
    aes(label = ticker),
    vjust = -1,
    size = 3.5,
    check_overlap = TRUE
  ) +

  scale_color_manual(
    values = c(
      "Polovodiče" = "#2878B5",
      "Paměťové technologie" = "#E58B24",
      "Kvantové počítače" = "#9467BD",
      "Stabilní společnosti" = "#2CA02C",
      "Tržní indexy" = "#555555"
    )
  ) +

  labs(
    title = "Rizikovost a výnosnost akciových společností",
    subtitle = "Porovnání všech sledovaných společností",
    x = "Anualizovaný složený výnos CAGR (%)",
    y = "Anualizovaná volatilita (%)",
    color = "Skupina společností",
    caption = paste(
      "Zdroj: vlastní zpracování týdenních cenových výnosů.",
      "Každá společnost využívá celou dostupnou historii."
    )
  ) +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# 9. ZOBRAZENI GRAFU
# ------------------------------------------------------------

print(risk_return_plot)

# ------------------------------------------------------------
# 10. ULOZENI GRAFU
# ------------------------------------------------------------

ggsave(
  filename = file.path(output_dir, "risk_return_map.png"),
  plot = risk_return_plot,
  width = 14,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------
# 11. VYSLEDEK
# ------------------------------------------------------------

cat("\n====================================\n")
cat("ANALYZA DOKONCENA\n")
cat("====================================\n")
cat("Pocet zpracovanych spolecnosti:", nrow(statistics), "\n")
cat("Graf: risk_return_map.png\n")
cat("Tabulka: risk_return_data.csv\n")
cat("Ulozeno do:", normalizePath(output_dir), "\n")






# ============================================================
# 09 - SOUHRNNA MAPA RIZIKOVOSTI A VYNOSNOSTI AKCII
# ============================================================

library(tidyverse)
library(ggplot2)
library(xts)
library(zoo)

# ------------------------------------------------------------
# 1. PRACOVNI ADRESAR
# ------------------------------------------------------------

setwd("C:/Users/revel/OneDrive/Desktop/StatistikaFinancichTrhu")

# ------------------------------------------------------------
# 2. INSTALACE A NACITANI BALICKU
# ------------------------------------------------------------

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
}

library(ggrepel)

# ------------------------------------------------------------
# 3. NACITANI DAT
# ------------------------------------------------------------

returns_list <- readRDS(
  "data/processed/returns/all_returns.rds"
)

# ------------------------------------------------------------
# 4. PREVOD CASOVYCH RAD NA JEDNU TABULKU
# ------------------------------------------------------------

returns_data <- purrr::map_dfr(
  names(returns_list),
  function(ticker_name) {

    stock_returns <- returns_list[[ticker_name]]

    if (inherits(stock_returns, "xts") ||
        inherits(stock_returns, "zoo")) {

      stock_data <- tibble(
        Date = as.Date(zoo::index(stock_returns)),
        weekly_return = as.numeric(
          zoo::coredata(stock_returns)[, 1]
        )
      )

    } else if (is.data.frame(stock_returns)) {

      stock_data <- as_tibble(stock_returns)

      date_column <- intersect(
        c("Date", "date", "index"),
        names(stock_data)
      )[1]

      return_column <- intersect(
        c("return", "Return", "returns", "Returns"),
        names(stock_data)
      )[1]

      if (is.na(date_column) || is.na(return_column)) {
        stop(paste("Neznamy format dat pro:", ticker_name))
      }

      stock_data <- tibble(
        Date = as.Date(stock_data[[date_column]]),
        weekly_return = as.numeric(stock_data[[return_column]])
      )

    } else {
      stop(paste("Nepodporovany format dat pro:", ticker_name))
    }

    stock_data %>%
      mutate(ticker = ticker_name) %>%
      select(ticker, Date, weekly_return)
  }
)

returns_data <- returns_data %>%
  filter(
    !is.na(Date),
    !is.na(weekly_return)
  )

# ------------------------------------------------------------
# 5. DEFINICE SKUPIN AKCII
# ------------------------------------------------------------

groups <- tibble(
  ticker = c(
    "NVDA", "AMD", "INTC", "AVGO", "TSM",
    "MU", "005930.KS", "000660.KS", "SNDK",
    "IONQ", "RGTI", "QBTS",
    "IBM", "KO", "PEP", "PG", "JNJ",
    "^GSPC", "^NDX"
  ),

  group = c(
    rep("Polovodiče", 5),
    rep("Paměťové technologie", 4),
    rep("Kvantové počítače", 3),
    rep("Stabilní společnosti", 5),
    rep("Tržní indexy", 2)
  )
)

# ------------------------------------------------------------
# 6. VYPOCET STATISTIK
# ------------------------------------------------------------

statistics <- returns_data %>%
  group_by(ticker) %>%
  arrange(Date, .by_group = TRUE) %>%
  summarise(

    observations = n(),

    first_date = min(Date),
    last_date = max(Date),

    years = as.numeric(last_date - first_date) / 365.25,

    growth_factor = prod(1 + weekly_return / 100),

    cumulative_return = (growth_factor - 1) * 100,

    CAGR = if_else(
      years > 0 & growth_factor > 0,
      (growth_factor^(1 / years) - 1) * 100,
      NA_real_
    ),

    annualized_volatility = sd(weekly_return) * sqrt(52),

    .groups = "drop"
  ) %>%
  left_join(groups, by = "ticker") %>%
  filter(
    !is.na(CAGR),
    !is.na(annualized_volatility)
  )

# ------------------------------------------------------------
# 7. VYTVORENI VYSTUPNI SLOZKY
# ------------------------------------------------------------

output_dir <- "data/processed/statistics/risk_return"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 8. ULOZENI VYPOCTENYCH STATISTIK
# ------------------------------------------------------------

write_csv(
  statistics,
  file.path(output_dir, "risk_return_data.csv")
)

# ------------------------------------------------------------
# 9. VYTVORENI PREHLEDNEHO GRAFU
# ------------------------------------------------------------

risk_return_plot <- ggplot(
  statistics,
  aes(
    x = CAGR,
    y = annualized_volatility,
    color = group
  )
) +

  # Nulovy vynos
  geom_vline(
    xintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.7
  ) +

  # Body jednotlivych spolecnosti
  geom_point(
    size = 4,
    alpha = 0.9
  ) +

  # Popisky s automatickym rozmistenim
  geom_label_repel(
    aes(label = ticker),
    size = 3.5,
    fontface = "bold",
    fill = "white",
    alpha = 0.9,
    box.padding = 0.7,
    point.padding = 0.5,
    max.overlaps = Inf,
    min.segment.length = 0,
    show.legend = FALSE
  ) +

  # Pseudo-logaritmicka osa vynosu
  scale_x_continuous(
    trans = scales::pseudo_log_trans(sigma = 10),
    breaks = c(-100, -50, -20, -10, 0, 10, 20, 50, 100, 200, 400)
  ) +

  # Pseudo-logaritmicka osa rizika
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 10),
    breaks = c(10, 20, 30, 40, 60, 80, 100, 120, 160)
  ) +

  # Barvy skupin
  scale_color_manual(
    values = c(
      "Polovodiče" = "#2878B5",
      "Paměťové technologie" = "#E58B24",
      "Kvantové počítače" = "#9467BD",
      "Stabilní společnosti" = "#2CA02C",
      "Tržní indexy" = "#555555"
    )
  ) +

  # Nazvy a popisky
  labs(
    title = "Rizikovost a výnosnost akciových společností",

    subtitle = "Porovnání všech 19 sledovaných společností",

    x = "Anualizovaný složený výnos CAGR (%)",

    y = "Anualizovaná volatilita (%)",

    color = "Skupina společností",

    caption = paste(
      "Zdroj: vlastní zpracování týdenních cenových výnosů.",
      "Obě osy používají pseudo-logaritmickou transformaci.",
      "Každá společnost využívá celou dostupnou historii."
    )
  ) +

  # Vzhled grafu
  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),

    plot.subtitle = element_text(
      size = 12
    ),

    legend.position = "bottom",

    panel.grid.minor = element_blank(),

    plot.margin = margin(20, 25, 20, 20)
  )

# ------------------------------------------------------------
# 10. ZOBRAZENI GRAFU
# ------------------------------------------------------------

print(risk_return_plot)

# ------------------------------------------------------------
# 11. ULOZENI GRAFU
# ------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "risk_return_map.png"
  ),

  plot = risk_return_plot,

  width = 16,
  height = 11,
  dpi = 300
)

# ------------------------------------------------------------
# 12. VYPIS VYSLEDKU
# ------------------------------------------------------------

cat("\n========================================\n")
cat("ANALYZA DOKONCENA\n")
cat("========================================\n")

cat("Pocet zpracovanych spolecnosti:", nrow(statistics), "\n")

cat(
  "Graf:",
  file.path(output_dir, "risk_return_map.png"),
  "\n"
)

cat(
  "Tabulka:",
  file.path(output_dir, "risk_return_data.csv"),
  "\n"
)

cat("========================================\n")
