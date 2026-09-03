#!/usr/bin/env Rscript
# Exploratory analysis: macro-factor effects on the tracked equities via
# (1) return covariance / correlation matrix, (2) PCA, (3) per-stock macro
# factor regressions with Newey-West (HAC) standard errors.
#
# Usage: Rscript analysis/eda_macro_pca.R
# Inputs:  data/prices_long.csv  (from /refresh-prices)
# Outputs: analysis/output/*.csv  and  analysis/output/*.png

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(tibble)
  library(purrr)
  library(lubridate)
  library(scales)
  library(tidyquant)   # tq_get (prices + FRED)
  library(lmtest)      # coeftest
  library(sandwich)    # NeweyWest
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
outdir <- file.path(root, "analysis", "output")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

FRED_FROM   <- "2015-01-01"
ROLL_WIN    <- 52L                         # weeks
MACRO_COLS  <- c("d_rate", "d_slope", "d_infl", "d_credit", "r_vix", "r_oil", "r_usd")
ROLL_FAC    <- c("d_rate", "d_credit", "r_vix", "GSPC")

# ----------------------------------------------------------------------------
# 1. Weekly stock returns
# ----------------------------------------------------------------------------
prices <- readr::read_csv(file.path(root, "data", "prices_long.csv"),
                          show_col_types = FALSE) |>
  mutate(symbol = ifelse(symbol == "^GSPC", "GSPC", symbol),
         week = floor_date(date, "week", week_start = 1))

weekly_rets <- prices |>
  group_by(symbol, week) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  group_by(symbol) |>
  arrange(week, .by_group = TRUE) |>
  mutate(ret = log(adjusted / lag(adjusted))) |>
  ungroup() |>
  filter(is.finite(ret)) |>
  select(symbol, week, ret)

rets_wide <- pivot_wider(weekly_rets, names_from = symbol, values_from = ret)

# ----------------------------------------------------------------------------
# 2. Weekly macro factors (FRED via tidyquant, no API key)
# ----------------------------------------------------------------------------
# BAA10Y = Moody's Baa corp yield minus 10y Treasury (full history on FRED);
# the ICE BofA HY OAS series only serves ~2 years through this endpoint.
fred_ids <- c(DGS10 = "DGS10", T10Y2Y = "T10Y2Y", T10YIE = "T10YIE",
              CREDIT = "BAA10Y", VIX = "VIXCLS",
              WTI = "DCOILWTICO", USD = "DTWEXBGS")

macro_raw <- tq_get(unname(fred_ids), get = "economic.data", from = FRED_FROM)
name_map  <- setNames(names(fred_ids), unname(fred_ids))

.cov <- macro_raw |> group_by(symbol) |>
  summarise(n = n(), first = min(date), last = max(date), .groups = "drop") |>
  arrange(desc(first)) |> as.data.frame()
message("FRED series coverage:"); print(.cov, row.names = FALSE)

macro_weekly <- macro_raw |>
  mutate(series = name_map[symbol],
         week = floor_date(date, "week", week_start = 1),
         # WTI printed negative on 2020-04-20; drop non-positive so log() is defined
         price = ifelse(series == "WTI" & price <= 0, NA_real_, price)) |>
  group_by(series, week) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(series, week, price) |>
  pivot_wider(names_from = series, values_from = price) |>
  arrange(week) |>
  tidyr::fill(everything(), .direction = "down")

macro_factors <- macro_weekly |>
  transmute(
    week,
    d_rate   = DGS10  - lag(DGS10),      # ppt change, 10y yield
    d_slope  = T10Y2Y - lag(T10Y2Y),     # ppt change, 10y-2y slope
    d_infl   = T10YIE - lag(T10YIE),     # ppt change, 10y breakeven inflation
    d_credit = CREDIT - lag(CREDIT),     # ppt change, Baa-10y credit spread
    r_vix    = log(VIX / lag(VIX)),      # log change, VIX
    r_oil    = log(WTI / lag(WTI)),      # log return, WTI crude
    r_usd    = log(USD / lag(USD))       # log return, broad USD index
  ) |>
  filter(if_all(-week, is.finite))

# Winsorize factors at 1/99 pct to blunt data-error / crisis outliers
winsor <- function(x, p = 0.01) {
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}
macro_factors <- macro_factors |> mutate(across(all_of(MACRO_COLS), winsor))

# ----------------------------------------------------------------------------
# Analysis routine, run per sample
# ----------------------------------------------------------------------------
star <- function(p) cut(p, c(-Inf, .01, .05, .1, Inf), c("***", "**", "*", ""))

run_analysis <- function(stock_syms, label) {
  message("\n==================  ", label, "  ==================")
  df <- rets_wide |>
    select(week, all_of(stock_syms)) |>
    inner_join(macro_factors, by = "week") |>
    filter(if_all(-week, is.finite))
  message(sprintf("weeks: %d   (%s to %s)   stocks: %d",
                  nrow(df), min(df$week), max(df$week), length(stock_syms)))

  R <- as.matrix(df[, stock_syms])

  ## --- 2a. Covariance / correlation matrix ---------------------------------
  C <- cor(R)
  readr::write_csv(as.data.frame(round(cov(R) * 52, 6)) |>
                     tibble::rownames_to_column("symbol"),
                   file.path(outdir, sprintf("cov_annualized_%s.csv", label)))
  readr::write_csv(as.data.frame(round(C, 4)) |>
                     tibble::rownames_to_column("symbol"),
                   file.path(outdir, sprintf("corr_%s.csv", label)))

  cm <- as.data.frame(C) |> tibble::rownames_to_column("a") |>
    pivot_longer(-a, names_to = "b", values_to = "rho")
  p_corr <- ggplot(cm, aes(b, a, fill = rho)) +
    geom_tile() + geom_text(aes(label = sprintf("%.2f", rho)), size = 3) +
    scale_fill_gradient2(limits = c(-1, 1), low = "#2166ac",
                         mid = "white", high = "#b2182b") +
    labs(title = paste("Weekly return correlation -", label), x = NULL, y = NULL) +
    theme_minimal()
  ggsave(file.path(outdir, sprintf("corr_heatmap_%s.png", label)),
         p_corr, width = 8, height = 6.5, dpi = 120)

  ## --- 2b. PCA (on correlation matrix, i.e. scaled returns) ---------------
  pc <- prcomp(R, center = TRUE, scale. = TRUE)
  ve <- pc$sdev^2 / sum(pc$sdev^2)
  k  <- min(3L, ncol(R))
  message("variance explained PC1..PC3: ",
          paste(sprintf("%.1f%%", 100 * ve[seq_len(k)]), collapse = "  "))

  scree <- tibble(pc = factor(seq_along(ve)), ve = ve, cum = cumsum(ve))
  p_scree <- ggplot(scree, aes(pc, ve)) +
    geom_col(fill = "#4575b4") +
    geom_line(aes(y = cum, group = 1), colour = "#d73027") +
    geom_point(aes(y = cum), colour = "#d73027") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("PCA scree -", label), x = "component",
         y = "variance explained (bars) / cumulative (line)") +
    theme_minimal()
  ggsave(file.path(outdir, sprintf("pca_scree_%s.png", label)),
         p_scree, width = 7, height = 4.5, dpi = 120)

  load_df <- as.data.frame(pc$rotation[, seq_len(k), drop = FALSE]) |>
    tibble::rownames_to_column("symbol")
  readr::write_csv(load_df, file.path(outdir, sprintf("pca_loadings_%s.csv", label)))

  ll <- load_df |> pivot_longer(-symbol, names_to = "pc", values_to = "loading")
  p_load <- ggplot(ll, aes(reorder(symbol, loading), loading, fill = loading > 0)) +
    geom_col(show.legend = FALSE) + coord_flip() +
    facet_wrap(~pc, nrow = 1) +
    scale_fill_manual(values = c(`TRUE` = "#1a9850", `FALSE` = "#d73027")) +
    labs(title = paste("PCA loadings -", label), x = NULL, y = "loading") +
    theme_minimal()
  ggsave(file.path(outdir, sprintf("pca_loadings_%s.png", label)),
         p_load, width = 9, height = 4.5, dpi = 120)

  ## --- 2c. Label the PCs by regressing scores on macro factors -----------
  scores <- as.data.frame(pc$x[, seq_len(k), drop = FALSE])
  names(scores) <- paste0("PC", seq_len(k))
  sdf <- bind_cols(df["week"], scores) |> inner_join(macro_factors, by = "week")

  pc_labels <- purrr::map_dfr(seq_len(k), function(j) {
    resp <- paste0("PC", j)
    m <- lm(reformulate(MACRO_COLS, resp), data = sdf)
    ct <- coeftest(m, vcov. = NeweyWest(m, lag = 4, prewhite = FALSE))
    tibble(pc = resp, term = rownames(ct), beta = ct[, 1],
           tstat = ct[, 3], p = ct[, 4],
           adj_r2 = summary(m)$adj.r.squared)
  })
  readr::write_csv(pc_labels, file.path(outdir, sprintf("pca_macro_labels_%s.csv", label)))

  ## --- 3. Per-stock macro factor regressions ----------------------------
  betas <- purrr::map_dfr(stock_syms, function(s) {
    rhs <- if (s == "GSPC") MACRO_COLS else c(MACRO_COLS, "GSPC")
    m  <- lm(reformulate(rhs, s), data = df)
    ct <- coeftest(m, vcov. = NeweyWest(m, lag = 4, prewhite = FALSE))
    tibble(stock = s, term = rownames(ct), beta = ct[, 1], se = ct[, 2],
           tstat = ct[, 3], p = ct[, 4],
           r2 = summary(m)$r.squared, adj_r2 = summary(m)$adj.r.squared,
           n = nobs(m))
  })
  readr::write_csv(betas, file.path(outdir, sprintf("macro_betas_%s.csv", label)))

  hm <- betas |> filter(term %in% MACRO_COLS) |> mutate(sig = star(p))
  p_beta <- ggplot(hm, aes(term, stock, fill = tstat)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f%s", beta, sig)), size = 2.9) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b") +
    labs(title = paste("Macro factor betas (HAC t as colour) -", label),
         subtitle = "cell = beta + significance;  *** .01  ** .05  * .10",
         x = NULL, y = NULL) +
    theme_minimal()
  ggsave(file.path(outdir, sprintf("macro_betas_heatmap_%s.png", label)),
         p_beta, width = 9, height = 5.5, dpi = 120)

  ## --- 3b. Rolling 52-week betas --------------------------------------
  roll <- purrr::map_dfr(setdiff(stock_syms, "GSPC"), function(s) {
    n <- nrow(df); if (n < ROLL_WIN + 4) return(NULL)
    purrr::map_dfr(seq.int(ROLL_WIN, n), function(i) {
      w  <- df[(i - ROLL_WIN + 1):i, ]
      cf <- coef(lm(reformulate(ROLL_FAC, s), data = w))
      tibble(week = df$week[i], stock = s, factor = ROLL_FAC,
             beta = unname(cf[ROLL_FAC]))
    })
  })
  if (nrow(roll)) {
    readr::write_csv(roll, file.path(outdir, sprintf("rolling_betas_%s.csv", label)))
    p_roll <- ggplot(roll, aes(week, beta, colour = stock)) +
      geom_line(linewidth = 0.4) +
      facet_wrap(~factor, scales = "free_y") +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      labs(title = paste0("Rolling ", ROLL_WIN, "-week betas - ", label),
           x = NULL, y = "beta") +
      theme_minimal()
    ggsave(file.path(outdir, sprintf("rolling_betas_%s.png", label)),
           p_roll, width = 10, height = 6, dpi = 120)
  }

  ## --- console summary ----------------------------------------------
  top <- betas |> filter(term %in% MACRO_COLS) |>
    group_by(stock) |> slice_max(abs(tstat), n = 1) |> ungroup() |>
    transmute(stock, strongest = term, beta = round(beta, 3),
              t = round(tstat, 2), adj_r2 = round(adj_r2, 2))
  print(as.data.frame(top), row.names = FALSE)
  invisible(betas)
}

# ----------------------------------------------------------------------------
tickers_all  <- c("AMZN", "AXON", "GOOGL", "META", "MSFT", "NVDA", "UNH",
                  "SOFI", "PLTR", "GSPC")
tickers_long <- setdiff(tickers_all, c("SOFI", "PLTR"))

run_analysis(tickers_all,  "2021plus_all10")
run_analysis(tickers_long, "2015plus_long8")

message("\nWrote outputs to ", outdir)
