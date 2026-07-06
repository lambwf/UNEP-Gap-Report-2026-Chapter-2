#' Helper Functions for UNEP Gap Report Standard Figures
#'
#' This script contains reusable helper functions to simplify and harmonize
#' the UNEP-Gap-Report-Standard-Figures.Rmd script.

#' Calculate Growth Rate Label
#'
#' Computes the growth rate for a given time range and formats it as a labeled string
#' (e.g., "+1.2%/yr" or "-0.5%/yr").
#'
#' @param data A data frame with columns `year` and `value`.
#' @param year_range A vector of length 2 specifying the start and end years for the calculation.
#' @param suffix A string to append to the label (default: "%/yr").
#' @param round_digits Number of decimal places to round the growth rate (default: 1).
#' @return A data frame with the first and last year's values and the formatted growth rate label.
#' @examples
#' label_long_term <- calculate_growth_label(data_total, c(1990, 2015), suffix = "%/yr")
calculate_growth_label <- function(data, year_range, suffix = "%/yr", round_digits = 1) {
  data %>%
    filter(year >= year_range[1] & year <= year_range[2]) %>%
    group_by(year) %>%
    summarise(value = sum(value)) %>%
    mutate(change = growth_rate(year, value)$rate * 100) %>%
    mutate(change = round(change, round_digits)) %>%
    mutate(change = ifelse(change > 0, paste0("+", change, suffix), paste0("-", abs(change), suffix))) %>%
    slice(c(1, n()))
}

#' Calculate Label Positions
#'
#' Computes the vertical positions for labels in stacked bar charts.
#' Positions are calculated as cumulative sums minus half the value for centering.
#'
#' @param data A data frame with columns for the grouping variable, `year`, and `value`.
#' @param group_var The name of the column to use for grouping (e.g., "gas", "sector_lv2").
#' @param year The year to filter for (default: maximum year in the data).
#' @return A data frame with the grouping variable and calculated label positions.
#' @examples
#' label_gases <- calculate_label_positions(data_total, "gas")
calculate_label_positions <- function(data, group_var, year = max(data$year, na.rm = TRUE)) {
  data %>%
    filter(year == year) %>%
    arrange(desc(!!sym(group_var))) %>%
    mutate(label_position = value) %>%
    mutate(label_position = cumsum(label_position)) %>%
    mutate(label_position = label_position - (value / 2)) %>%
    select(!!sym(group_var), label_position)
}

#' Save Figure
#'
#' Saves a ggplot object to multiple file formats (PNG, PDF, SVG) with consistent settings.
#'
#' @param plot A ggplot object to save.
#' @param filename_prefix The base filename without extension (e.g., "UNEP-EGR-2026-total-gas-trend").
#' @param path The directory path to save files to (default: "results/standard/").
#' @param formats A character vector of file formats to save (default: c("png", "pdf", "svg")).
#' @param dpi Resolution for PNG files (default: 300).
#' @param height Figure height in inches (default: 4).
#' @param width Figure width in inches (default: 8).
#' @return Invisibly returns the input plot.
#' @examples
#' save_figure(plot, "UNEP-EGR-2026-total-gas-trend", height = 4, width = 8)
save_figure <- function(plot, filename_prefix, path = "results/standard/", 
                        formats = c("png", "pdf", "svg"), dpi = 300, 
                        height = 4, width = 8) {
  for (fmt in formats) {
    if (fmt == "pdf") {
      ggsave(plot, filename = file.path(path, paste0(filename_prefix, ".", fmt)), 
             device = cairo_pdf, height = height, width = width)
    } else {
      ggsave(plot, filename = file.path(path, paste0(filename_prefix, ".", fmt)), 
             device = fmt, height = height, width = width, dpi = dpi)
    }
  }
  invisible(plot)
}

#' Save Data to Excel Workbook
#'
#' Saves a data frame to an Excel workbook with an optional info sheet.
#'
#' @param data The data frame to save in the "data" sheet.
#' @param filename_prefix The base filename without extension (e.g., "UNEP-EGR-2026-total-gas-trend").
#' @param path The directory path to save the file to (default: "results/standard/").
#' @param info_df A data frame for the "info" sheet (default: NULL). If NULL, no info sheet is created.
#' @param overwrite Logical indicating whether to overwrite existing files (default: TRUE).
#' @return Invisibly returns the input data.
#' @examples
#' info_df <- data.frame(a = c("Author", "Contact"), b = c("William F. Lamb", "william.lamb@pik-potsdam.de"))
#' save_data(data_total, "UNEP-EGR-2026-total-gas-trend", info_df = info_df)
save_data <- function(data, filename_prefix, path = "results/standard/", 
                      info_df = NULL, overwrite = TRUE) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "data")
  
  if (!is.null(info_df)) {
    openxlsx::addWorksheet(wb, "info")
    openxlsx::writeData(wb, sheet = "info", info_df, colNames = FALSE, rowNames = FALSE)
  }
  
  openxlsx::writeData(wb, sheet = "data", data, colNames = TRUE, rowNames = FALSE)
  openxlsx::saveWorkbook(wb, file.path(path, paste0(filename_prefix, ".xlsx")), overwrite = overwrite)
  invisible(data)
}
