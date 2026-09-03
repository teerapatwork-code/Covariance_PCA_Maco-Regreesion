#!/usr/bin/env Rscript
# Fetch daily OHLCV + adjusted prices for the tracked universe via tidyquant.
# Usage: Rscript refresh_prices.R [START_DATE]   (default START_DATE = 2010-01-01)

args <- commandArgs(trailingOnly = TRUE)
start_date <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "2010-01-01"

tickers <- c("^GSPC", "AMZN", "UNH", "GOOGL", "SOFI", "PLTR", "AXON", "MSFT", "NVDA", "META")

if (!requireNamespace("tidyquant", quietly = TRUE)) {
  message("Installing tidyquant (first run)...")
  install.packages("tidyquant", repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(tidyquant)
  library(dplyr)
  library(readr)
})

# Resolve project root by walking up from the script dir to the repo marker.
find_root <- function(start) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in 1:12) {
    if (any(file.exists(file.path(d, c(".git", ".gitignore"))))) return(d)
    p <- dirname(d); if (p == d) break; d <- p
  }
  getwd()
}
this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(this_file)) dirname(normalizePath(this_file, mustWork = FALSE)) else getwd()
root <- find_root(script_dir)
data_dir <- file.path(root, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

message(sprintf("Fetching %d symbols from %s ...", length(tickers), start_date))
prices <- tq_get(tickers, get = "stock.prices", from = start_date)

if (is.null(prices) || nrow(prices) == 0) {
  stop("No data returned. Check network / ticker symbols.")
}

write_csv(prices, file.path(data_dir, "prices_long.csv"))

safe_name <- function(s) gsub("[^A-Za-z0-9]+", "", s)
for (sym in unique(prices$symbol)) {
  write_csv(filter(prices, symbol == sym),
            file.path(data_dir, paste0(safe_name(sym), ".csv")))
}

manifest <- prices |>
  group_by(symbol) |>
  summarise(rows = n(), first_date = min(date), last_date = max(date), .groups = "drop") |>
  mutate(refreshed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
write_csv(manifest, file.path(data_dir, "manifest.csv"))

missing <- setdiff(tickers, unique(prices$symbol))
if (length(missing)) message("WARNING: no data for: ", paste(missing, collapse = ", "))

message("Wrote ", data_dir)
print(as.data.frame(manifest))
