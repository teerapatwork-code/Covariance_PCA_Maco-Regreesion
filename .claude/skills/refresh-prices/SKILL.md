---
name: refresh-prices
description: Pull adjusted and raw daily OHLCV for the tracked ticker universe (S&P 500 + AMZN, UNH, GOOGL, SOFI, PLTR, AXON, MSFT, NVDA, META) via R and tidyquant, writing CSVs and a last-updated manifest into data/. Use when the user asks to fetch, refresh, download, or update price/market data.
disable-model-invocation: true
---

# refresh-prices

Fetches daily price history for the tracked universe using R + `tidyquant` (`tq_get`, Yahoo Finance source).

## Universe

`^GSPC` (S&P 500 index) plus `AMZN`, `UNH`, `GOOGL`, `SOFI`, `PLTR`, `AXON`, `MSFT`, `NVDA`, `META`.
The canonical list lives in `refresh_prices.R` (`tickers` vector) — edit it there if the universe changes.

## Steps

1. Locate `Rscript`. It is **not on PATH**; use the full path (quote it):
   `C:\Program Files\R\R-4.6.1\bin\Rscript.exe` (or the newest `C:\Program Files\R\R-*\bin\Rscript.exe`).
2. Run the script from the project root:
   ```
   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" .claude/skills/refresh-prices/refresh_prices.R
   ```
   Optional args: `$ARGUMENTS` is passed through as a start date, e.g. `/refresh-prices 2015-01-01` (default: `2010-01-01`).
3. The script installs `tidyquant` on first run if missing, then writes:
   - `data/prices_long.csv` — all tickers, tidy long format (symbol, date, open, high, low, close, volume, adjusted)
   - `data/<SYMBOL>.csv` — one file per ticker
   - `data/manifest.csv` — symbol, rows, first_date, last_date, refreshed_at
4. Show the user the manifest contents and flag any ticker whose `last_date` is stale or whose row count is 0.

## Notes

- Adjusted close is split/dividend-adjusted; use it for returns and backtests. Raw OHLCV is kept for reference.
- Yahoo data can have gaps or late revisions near the current date — re-running is idempotent (full overwrite).
