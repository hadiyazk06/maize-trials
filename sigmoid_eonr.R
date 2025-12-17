#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(purrr); library(ggplot2)
})

DATA_PATH <- ("//home//schnablelab//Documents//N trials//data files//Ndata2.0.xlsx")
OUTPUT_DIR <- file.path('Documents','N trials','eonr_outputs')
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

target_ids <- c('331_2018','1724_2022')
price_table <- tibble::tribble(
  ~year, ~corn, ~nitrogen,
  2018, 3.23, 0.35,
  2022, 6.57, 0.98
)

sigmoid <- function(x, Asym, xmid, scal) SSlogis(x, Asym, xmid, scal)
deriv_sigmoid <- function(x, Asym, xmid, scal) {
  exp_term <- exp((xmid - x) / scal)
  (Asym / scal) * exp_term / (1 + exp_term)^2
}

fit_sigmoid_eonr <- function(df, corn_price, n_price) {
  fit <- nls(yield ~ SSlogis(n_rate, Asym, xmid, scal), data = df)
  params <- coef(fit)
  grid <- seq(min(df$n_rate), max(df$n_rate), length.out = 500)
  yhat <- sigmoid(grid, params['Asym'], params['xmid'], params['scal'])
  profit <- corn_price * yhat - n_price * grid
  idx <- which.max(profit)
  list(
    grid = grid,
    fit_yields = yhat,
    EONR = grid[idx],
    yield = yhat[idx],
    marginal_yield = deriv_sigmoid(grid[idx], params['Asym'], params['xmid'],
                                   params['scal']),
    marginal_value = deriv_sigmoid(grid[idx], params['Asym'], params['xmid'],
                                   params['scal']) * corn_price
  )
}

raw <- read_excel(DATA_PATH, sheet = 'RD', skip = 2, na = '.') %>%
  slice(-(1:2)) %>%
  rename(year = StudyYear) %>%
  select(n_rate, yield, StudyID_Yr, year) %>%
  filter(!is.na(n_rate), !is.na(yield), yield > 0, StudyID_Yr %in% target_ids)
getwd(
)
results <- raw %>%
  group_split(StudyID_Yr) %>%
  map_dfr(function(df) {
    yr <- unique(df$year)
    price <- filter(price_table, year == yr)
    fit <- fit_sigmoid_eonr(df, price$corn, price$nitrogen)
    plot_df <- data.frame(n_rate = fit$grid, yield = fit$fit_yields)
    plot_path <- file.path(OUTPUT_DIR, sprintf('%s_sigmoid.png',
                                               unique(df$StudyID_Yr)))
     plot_df <- data.frame(n_rate = fit$grid, yield = fit$fit_yields)
      range_span <- diff(range(df$n_rate))
      dx <- if (range_span > 0) min(max(range_span * 0.1, 0.5), range_span)
  else 1
      min_n <- min(df$n_rate)
      max_n <- max(df$n_rate)
      x_start <- max(min_n, fit$EONR - dx / 2)
      x_end <- min(max_n, fit$EONR + dx / 2)
      dx_actual <- x_end - x_start
      if (dx_actual <= 0) {
        x_start <- fit$EONR
        x_end <- fit$EONR
        dx_actual <- 1
      }
      slope_df <- tibble(
        x = x_start,
        xend = x_end,
        y = fit$yield - fit$marginal_yield * dx_actual / 2,
        yend = fit$yield + fit$marginal_yield * dx_actual / 2
      )
      range_y <- diff(range(df$yield))
      y_offset <- if (range_y > 0) 0.07 * range_y else 5
      label_df <- tibble(
        n_rate = min(max_n, fit$EONR + dx * 0.6),
        yield = fit$yield + y_offset,
        label = sprintf('Marginal yield = %.2f bu/lb N', fit$marginal_yield)
      )
      eonr_point <- tibble(n_rate = fit$EONR, yield = fit$yield)
      plot_path <- file.path(OUTPUT_DIR, sprintf('%s_sigmoid.png',
  unique(df$StudyID_Yr)))
      p <- ggplot(df, aes(n_rate, yield)) +
        geom_point(size = 2, alpha = 0.75) +
        geom_line(data = plot_df, aes(n_rate, yield), color = 'darkorange',
  linewidth = 1) +
        geom_point(data = eonr_point, color = 'firebrick', size = 2.5) +
        geom_segment(
          data = slope_df,
          aes(x = x, xend = xend, y = y, yend = yend),
          inherit.aes = FALSE,
          color = 'steelblue',
          linewidth = 1
        ) +
        geom_text(
          data = label_df,
          aes(n_rate, yield, label = label),
          inherit.aes = FALSE,
          hjust = 0,
          vjust = -0.25,
          size = 3.2,
          color = 'steelblue'
        ) +
        geom_vline(xintercept = fit$EONR, color = 'firebrick', linetype =
  'dashed') +
        labs(
          title = sprintf('Sigmoid N response: %s', unique(df$StudyID_Yr)),
          subtitle = sprintf('EONR = %.1f lb/ac', fit$EONR),
          x = 'N rate (lb/ac)',
          y = 'Yield (bu/ac)'
        ) +theme_classic(base_size = 13) + theme(
          plot.background = element_rect(fill = 'white', color = NA),
          panel.background = element_rect(fill = 'white', color = NA),
          panel.grid = element_blank(),
          axis.text = element_text(color = 'black'),
          axis.title = element_text(color = 'black'),
          legend.background = element_rect(fill = 'white', color = NA),
          legend.key = element_rect(fill = 'white', color = NA))
      
      
      
      P
      
       white_bg_theme <- theme_classic(base_size = 13) + theme(
         plot.background = element_rect(fill = 'white', color = NA),
         panel.background = element_rect(fill = 'white', color = NA),
         panel.grid = element_blank(),
         axis.text = element_text(color = 'black'),
           axis.title = element_text(color = 'black'),
             legend.background = element_rect(fill = 'white', color = NA),
               legend.key = element_rect(fill = 'white', color = NA))
       
       ggsave(
         "/home/schnablelab/Documents/N trials/sigmoid_plot.png",
         plot = p,
         width = 10,
         height = 7,
         dpi = 400,
         bg = "white"
       )
       
       file.exists("/home/schnablelab/Documents/N trials/scatterplots/sigmoid_plot.png")
       
       
       
       
       
       
       
#############################SIGMOID with all trials#########################
       
       
       #!/usr/bin/env Rscript
         suppressPackageStartupMessages({
           library(readxl)
           library(dplyr)
           library(purrr)
           library(ggplot2)
         })
       
       DATA_PATH <- file.path('Documents', 'N trials', 'data files', 'Ndata2.0.xlsx')
       OUTPUT_DIR <- file.path('Documents', 'N trials', 'eonr_outputs')
       dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
       
       # Consistent white background so exported figures match manuscript styling
       white_bg_theme <- theme_classic(base_size = 13) +
         theme(
           plot.background = element_rect(fill = 'white', color = NA),
           panel.background = element_rect(fill = 'white', color = NA),
           panel.grid = element_blank(),
           axis.text = element_text(color = 'black'),
           axis.title = element_text(color = 'black'),
           legend.background = element_rect(fill = 'white', color = NA),
           legend.key = element_rect(fill = 'white', color = NA)
         )
       
       target_ids <- c(
         '1111_2014', '1400_2019', '1407_2019', '141_2016', '1582_2021', '1724_2022',
         '1862_2024', '215_2017', '286_2018', '296_2018', '329_2018', '331_2018',
         '339_2018', '75_2015'
       )
       
       price_table <- tibble::tribble(
         ~year, ~corn, ~nitrogen,
         2014, 3.50, 0.58,
         2015, 3.65, 0.51,
         2016, 3.05, 0.42,
         2017, 3.15, 0.32,
         2018, 3.23, 0.35,
         2019, 3.83, 0.28,
         2021, 5.20, 0.70,
         2022, 6.57, 0.98,
         2024, 4.35, 0.51
       )
       
       sigmoid <- function(x, Asym, xmid, scal) SSlogis(x, Asym, xmid, scal)
       deriv_sigmoid <- function(x, Asym, xmid, scal) {
         exp_term <- exp((xmid - x) / scal)
         (Asym / scal) * exp_term / (1 + exp_term)^2
       }
       
       fit_sigmoid_eonr <- function(df, corn_price, n_price) {
         fit <- try(
           nls(
             yield ~ SSlogis(n_rate, Asym, xmid, scal),
             data = df,
             control = nls.control(maxiter = 2000, warnOnly = TRUE, minFactor = 1e-8)
           ),
           silent = TRUE
         )
         
         if (inherits(fit, 'try-error')) {
           max_y <- max(df$yield, na.rm = TRUE)
           range_n <- range(df$n_rate, na.rm = TRUE)
           diff_n <- diff(range_n)
           start <- list(
             Asym = max_y * 1.05,
             xmid = median(df$n_rate, na.rm = TRUE),
             scal = max(diff_n / 4, 1)
           )
           lower <- c(Asym = 0.01, xmid = range_n[1], scal = 1e-3)
           upper <- c(Asym = max_y * 5, xmid = range_n[2] + max(diff_n, 1) * 2, scal
                      = max(diff_n, 1) * 10)
           
           fit <- try(
             nls(
               yield ~ Asym / (1 + exp((xmid - n_rate)/scal)),
               data = df,
               start = start,
               algorithm = 'port',
               lower = lower,
               upper = upper,
               control = nls.control(maxiter = 2000, warnOnly = TRUE, minFactor =
                                       1e-8)
             ),
             silent = TRUE
           )
         }
         
         if (inherits(fit, 'try-error')) {
           return(NULL)
         }
         
         params <- coef(fit)
         grid <- seq(min(df$n_rate), max(df$n_rate), length.out = 500)
         yields <- sigmoid(grid, params['Asym'], params['xmid'], params['scal'])
         profits <- corn_price * yields - n_price * grid
         idx <- which.max(profits)
         list(
           model = fit,
           params = params,
           grid = grid,
           fit_yields = yields,
           EONR = grid[idx],
           yield = yields[idx],
           marginal_yield = deriv_sigmoid(grid[idx], params['Asym'], params['xmid'],
                                          params['scal']),
           marginal_value = deriv_sigmoid(grid[idx], params['Asym'], params['xmid'],
                                          params['scal']) * corn_price
         )
       }
       
       raw <- read_excel(DATA_PATH, sheet = 'RD', skip = 2, na = '.') %>%
         slice(-(1:2)) %>%
         rename(year = StudyYear) %>%
         select(n_rate, yield, StudyID_Yr, year) %>%
         filter(!is.na(n_rate), !is.na(yield), yield > 0, StudyID_Yr %in% target_ids)
       
       results_list <- raw %>%
         group_split(StudyID_Yr) %>%
         map(function(df) {
           yr <- unique(df$year)
           prices <- filter(price_table, year == yr)
           stopifnot(nrow(prices) == 1)
           fit <- fit_sigmoid_eonr(df, prices$corn, prices$nitrogen)
           if (is.null(fit)) {
             message(sprintf('Skipping %s (%s): sigmoid fit failed to converge',
                             unique(df$StudyID_Yr), yr))
             return(NULL)
           }
           plot_df <- data.frame(n_rate = fit$grid, yield = fit$fit_yields)
           range_span <- diff(range(df$n_rate))
           dx <- if (range_span > 0) min(max(range_span * 0.1, 0.5), range_span)
           else 1
           min_n <- min(df$n_rate)
           max_n <- max(df$n_rate)
           x_start <- max(min_n, fit$EONR - dx / 2)
           x_end <- min(max_n, fit$EONR + dx / 2)
           dx_actual <- x_end - x_start
           if (dx_actual <= 0) {
             x_start <- fit$EONR
             x_end <- fit$EONR
             dx_actual <- 1
           }
           slope_df <- tibble(
             x = x_start,
             xend = x_end,
             y = fit$yield - fit$marginal_yield * dx_actual / 2,
             yend = fit$yield + fit$marginal_yield * dx_actual / 2
           )
           range_y <- diff(range(df$yield))
           y_offset <- if (range_y > 0) 0.07 * range_y else 5
           label_df <- tibble(
             n_rate = min(max_n, fit$EONR + dx * 0.6),
             yield = fit$yield + y_offset,
             label = sprintf('Marginal yield = %.2f bu/lb N', fit$marginal_yield)
           )
           eonr_point <- tibble(n_rate = fit$EONR, yield = fit$yield)
           plot_path <- file.path(OUTPUT_DIR, sprintf('%s_sigmoid.png',
                                                      unique(df$StudyID_Yr)))
           p <- ggplot(df, aes(n_rate, yield)) +
             geom_point(size = 2, alpha = 0.75) +
             geom_line(data = plot_df, aes(n_rate, yield), color = 'darkorange',
                       linewidth = 1) +
             geom_point(data = eonr_point, color = 'firebrick', size = 2.5) +
             geom_segment(
               data = slope_df,
               aes(x = x, xend = xend, y = y, yend = yend),
               inherit.aes = FALSE,
               color = 'steelblue',
               linewidth = 1
             ) +
             geom_text(
               data = label_df,
               aes(n_rate, yield, label = label),
               inherit.aes = FALSE,
               hjust = 0,
               vjust = -0.25,
               size = 3.2,
               color = 'steelblue'
             ) +
             geom_vline(xintercept = fit$EONR, color = 'firebrick', linetype =
                          'dashed') +
             labs(
               title = sprintf('Sigmoid N response: %s', unique(df$StudyID_Yr)),
               subtitle = sprintf('EONR = %.1f lb/ac', fit$EONR),
               x = 'N rate (lb/ac)',
               y = 'Yield (bu/ac)'
             ) +
             white_bg_theme
           ggsave(plot_path, p, width = 6, height = 4, dpi = 300)
           
           tibble(
             StudyID_Yr = unique(df$StudyID_Yr),
             year = yr,
             corn_price = prices$corn,
             nitrogen_price = prices$nitrogen,
             EONR_lb_ac = fit$EONR,
             yield_at_EONR_bu_ac = fit$yield,
             marginal_yield_bu_per_lbN = fit$marginal_yield,
             value_of_last_lb = fit$marginal_value
           )
         })
       
       results <- results_list %>%
         purrr::compact() %>%
         dplyr::bind_rows()
       
       if (nrow(results) == 0) {
         stop('No sigmoid fits converged. Check data or starting values.')
       }
       
       print(results)
       write.csv(results, file.path(OUTPUT_DIR, 'sigmoid_pair_summary.csv'),row.names = FALSE)
       
       
       