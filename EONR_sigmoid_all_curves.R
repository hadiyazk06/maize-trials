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
OUTPUT_PATH_COMBINED <- file.path(OUTPUT_DIR, 'all_sigmoid_curves_combined.png')
OUTPUT_PATH_FACETED <- file.path(OUTPUT_DIR, 'all_sigmoid_curves_faceted.png')
OUTPUT_PATH_FACETED_NOTITLE <- file.path(OUTPUT_DIR, 'all_sigmoid_curves_faceted_plain.png')
OUTPUT_PATH_YEARWISE <- file.path(OUTPUT_DIR, 'all_sigmoid_curves_yearwise.png')

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

fit_sigmoid <- function(df) {
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
    upper <- c(Asym = max_y * 5, xmid = range_n[2] + max(diff_n, 1) * 2, scal = max(diff_n, 1) * 10)

    fit <- try(
      nls(
        yield ~ Asym / (1 + exp((xmid - n_rate)/scal)),
        data = df,
        start = start,
        algorithm = 'port',
        lower = lower,
        upper = upper,
        control = nls.control(maxiter = 2000, warnOnly = TRUE, minFactor = 1e-8)
      ),
      silent = TRUE
    )
  }

  if (inherits(fit, 'try-error')) {
    return(NULL)
  }
  fit
}

data_all <- read_excel(DATA_PATH, sheet = 'RD', skip = 2, na = '.') %>%
  slice(-(1:2)) %>%
  rename(year = StudyYear) %>%
  select(n_rate, yield, StudyID_Yr, year) %>%
  filter(!is.na(n_rate), !is.na(yield), yield > 0, StudyID_Yr %in% target_ids)

splits <- group_split(data_all, StudyID_Yr)

fit_results <- map(splits, function(df) {
  fit <- fit_sigmoid(df)
  if (is.null(fit)) {
    return(NULL)
  }
  current_year <- unique(df$year)
  params <- coef(fit)
  grid <- seq(min(df$n_rate), max(df$n_rate), length.out = 400)
  curve <- tibble(
    StudyID_Yr = unique(df$StudyID_Yr),
    year = current_year,
    n_rate = grid,
    yield = SSlogis(grid, params['Asym'], params['xmid'], params['scal'])
  )
  prices <- filter(price_table, year == current_year)
  stopifnot(nrow(prices) == 1)
  profits <- prices$corn * curve$yield - prices$nitrogen * curve$n_rate
  eonr <- curve$n_rate[which.max(profits)]
  list(
    points = df,
    curve = curve,
    eonr = tibble(StudyID_Yr = unique(df$StudyID_Yr), year = current_year, x = eonr)
  )
}) %>%
  purrr::compact()

if (length(fit_results) == 0) {
  stop('No sigmoid fits converged for the requested studies.')
}

data_points <- map_dfr(fit_results, ~ .x$points) %>%
  mutate(year = factor(year))
curve_points <- map_dfr(fit_results, ~ .x$curve) %>%
  mutate(year = factor(year))
eonr_lines <- map_dfr(fit_results, ~ .x$eonr) %>%
  mutate(year = factor(year))
x_limit <- c(0, 250)
y_limit <- c(0, max(data_points$yield, na.rm = TRUE) * 1.05)

combined_plot <- ggplot() +
  geom_point(data = data_points, aes(n_rate, yield, color = StudyID_Yr), alpha = 0.45, size = 1.4) +
  geom_line(data = curve_points, aes(n_rate, yield, color = StudyID_Yr), linewidth = 0.9) +
  geom_vline(
    data = eonr_lines,
    aes(xintercept = x, color = StudyID_Yr),
    linetype = 'dashed',
    linewidth = 0.6
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = 'right',
    legend.title = element_blank()
  ) +
  labs(title = 'Yield Respond curves vs EONR')

faceted_plot <- ggplot() +
  geom_point(data = data_points, aes(n_rate, yield), alpha = 0.65, size = 1.3, color = 'grey35') +
  geom_line(data = curve_points, aes(n_rate, yield), linewidth = 0.95, color = '#E07B39') +
  geom_vline(
    data = eonr_lines,
    aes(xintercept = x),
    linetype = 'dashed',
    linewidth = 0.55,
    color = '#B22222'
  ) +
  facet_wrap(~StudyID_Yr, scales = 'free') +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(color = 'black'),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = 'bold', size = 10),
    legend.position = 'none',
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  labs(
    title = 'Yield Respond curves vs EONR (by StudyID)',
    x = 'N rate (lb/ac)',
    y = 'Yield (bu/ac)'
  ) +
  coord_cartesian(xlim = x_limit, ylim = y_limit)

faceted_plain_plot <- ggplot() +
  geom_point(data = data_points, aes(n_rate, yield), alpha = 0.7, size = 1.3, color = 'grey19') +
  geom_line(data = curve_points, aes(n_rate, yield), linewidth = 0.75, color = 'navy') +
  geom_vline(
    data = eonr_lines,
    aes(xintercept = x),
    linetype = 'dashed',
    linewidth = 0.4,
    color = 'firebrick'
  ) +
  facet_wrap(~StudyID_Yr, scales = 'free') +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  labs(title = 'Yield Respond curves vs EONR') +
  coord_cartesian(xlim = x_limit, ylim = y_limit)

yearwise_plot <- ggplot() +
  geom_point(data = data_points, aes(n_rate, yield, color = StudyID_Yr), alpha = 0.5, size = 1.2) +
  geom_line(data = curve_points, aes(n_rate, yield, color = StudyID_Yr), linewidth = 0.85, alpha = 0.95) +
  geom_vline(
    data = eonr_lines,
    aes(xintercept = x),
    linetype = 'dashed',
    linewidth = 0.55,
    color = '#B22222'
  ) +
  facet_wrap(~year, scales = 'free') +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(color = 'black'),
    plot.title = element_text(hjust = 0.5),
    legend.position = 'bottom',
    legend.title = element_blank(),
    strip.text = element_text(face = 'bold', size = 11),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  labs(
    title = 'Yield Respond curves vs EONR (by Year)',
    x = 'N rate (lb/ac)',
    y = 'Yield (bu/ac)'
  ) +
  coord_cartesian(xlim = x_limit, ylim = y_limit)

ggsave(OUTPUT_PATH_COMBINED, combined_plot, width = 8, height = 5, dpi = 300)
ggsave(OUTPUT_PATH_FACETED, faceted_plot, width = 16, height = 9, dpi = 300)
ggsave(OUTPUT_PATH_FACETED_NOTITLE, faceted_plain_plot, width = 9, height = 7.5, dpi = 300)
ggsave(OUTPUT_PATH_YEARWISE, yearwise_plot, width = 14, height = 7, dpi = 300)

# Maintain legacy filename for backward compatibility
file.copy(OUTPUT_PATH_COMBINED, file.path(OUTPUT_DIR, 'all_sigmoid_curves.png'), overwrite = TRUE)
