#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(ggplot2)
})

if (!exists("nit_summary")) stop("nit_summary not found in environment")

price_tbl <- tibble(
  year = c(2014,2015,2016,2017,2018,2019,2021,2022,2024),
  corn_price = c(3.50,3.65,3.05,3.15,3.23,3.83,5.20,6.57,4.35),
  n_price    = c(0.58,0.51,0.42,0.32,0.35,0.28,0.70,0.98,0.51)
)

logistic_fun <- function(N, a, x0, b) a / (1 + exp((x0 - N) / b))

fit_sigmoid <- function(df) {
  valid <- complete.cases(df$n_rate, df$mean_yield)
  df <- df[valid, , drop = FALSE]
  if (nrow(df) < 3) stop("Not enough data to fit sigmoid")
  tryCatch(
    nls(mean_yield ~ SSlogis(n_rate, Asym, xmid, scal), data = df),
    error = function(...) {
      nls(mean_yield ~ Asym / (1 + exp((xmid - n_rate) / scal)),
          data = df,
          start = list(
            Asym = max(df$mean_yield, na.rm = TRUE),
            xmid = stats::weighted.mean(df$n_rate, df$mean_yield),
            scal = 30
          ),
          control = nls.control(maxiter = 200, warnOnly = TRUE))
    }
  )
}

deriv_fun <- function(N, coefs) {
  a <- coefs["Asym"]; x0 <- coefs["xmid"]; b <- coefs["scal"]
  exp_term <- exp((x0 - N) / b)
  (a * exp_term) / (b * (1 + exp_term)^2)
}

solve_eonr <- function(coefs, prices) {
  target <- prices$n_price / prices$corn_price
  f <- function(N) deriv_fun(N, coefs) - target
  f0 <- f(0)
  f400 <- f(400)
  if (is.na(f0) || is.na(f400)) return(NA_real_)
  if (f0 * f400 > 0) {
    return(if (abs(f0) < abs(f400)) 0 else 400)
  }
  uniroot(f, interval = c(0, 400))$root
}

prediction_grid <- tibble(n_rate = 0:400)

compute_trial <- function(df) {
  study <- df$StudyID_Yr[1]
  yr <- as.integer(sub(".*_(\\d{4})$", "\\1", study))
  price_row <- price_tbl %>% filter(year == yr)
  if (!nrow(price_row)) return(NULL)
  model <- fit_sigmoid(df)
  coefs <- coef(model)
  eonr_val <- solve_eonr(coefs, price_row)
  preds <- prediction_grid %>%
    mutate(
      pred_yield = logistic_fun(n_rate, coefs["Asym"], coefs["xmid"], coefs["scal"]),
      pred_profit = pred_yield * price_row$corn_price - n_rate * price_row$n_price
    )
  list(
    tibble(
      StudyID_Yr = study,
      a_param = coefs["Asym"],
      x0_param = coefs["xmid"],
      b_param = coefs["scal"],
      EONR_lb_ac = eonr_val,
      yield_at_EONR = logistic_fun(eonr_val, coefs["Asym"], coefs["xmid"], coefs["scal"]),
      profit_at_EONR = logistic_fun(eonr_val, coefs["Asym"], coefs["xmid"], coefs["scal"]) * price_row$corn_price - eonr_val * price_row$n_price
    ),
    preds = preds,
    observed = df
  )
}

results <- nit_summary %>%
  drop_na(StudyID_Yr, n_rate, mean_yield) %>%
  group_split(StudyID_Yr) %>%
  map(compute_trial) %>%
  keep(~ !is.null(.x))

summary_tbl <- map_dfr(results, 1)

out_dir <- file.path("Documents", "Kounsar H", "N trials", "eonr_newstep", "predictions_all")
plot_dir <- file.path(out_dir, "graphs")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

readr::write_csv(summary_tbl, file.path(out_dir, "prediction_summary_all.csv"))

walk(results, function(obj) {
  trial <- obj[[1]]$StudyID_Yr[1]
  eonr_line <- obj[[1]]$EONR_lb_ac[1]
  ymax <- max(obj$preds$pred_yield, obj$observed$mean_yield, na.rm = TRUE)
  p <- ggplot(obj$preds, aes(n_rate, pred_yield)) +
    geom_line(color = "#1b9e77", linewidth = 1.1) +
    geom_point(data = obj$observed, aes(n_rate, mean_yield), color = "#d95f02", size = 2, alpha = 0.85) +
    geom_vline(xintercept = eonr_line, linetype = "dashed", color = "#7570b3") +
    annotate("text", x = eonr_line, y = ymax * 0.95,
             label = sprintf("EONR = %.1f lb/ac", eonr_line),
             hjust = -0.05, vjust = 1, size = 3, color = "#7570b3") +
    scale_x_continuous(limits = c(0, 400), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(0, ymax * 1.05), expand = expansion(mult = 0)) +
    labs(title = paste("Predicted vs observed:", trial), x = "N rate (lb/ac)", y = "Yield (bu/ac)") +
    theme_classic(base_size = 11) +
    theme(axis.line = element_line(color = "black"), panel.grid = element_blank())
  ggsave(file.path(plot_dir, paste0("prediction_", trial, ".png")), p,
         width = 6.5, height = 4.5, dpi = 320, bg = "white")
})

summary_tbl
