
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
