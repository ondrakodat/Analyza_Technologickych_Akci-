
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
