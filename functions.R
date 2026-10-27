

calculate_growth_label <- function(data, year_range) {
  data %>%
    filter(year >= year_range[1] & year <= year_range[2]) %>%
    group_by(year) %>%
    summarise(value = sum(value)) %>%
    mutate(change = growth_rate(year, value)$rate * 100) %>%
    mutate(change = round(change, 1)) %>%
    mutate(change = ifelse(change > 0, paste0("+", change, "%/yr"), paste0("-", abs(change), "%/yr"))) %>%
    slice(c(1, n()))
}


calculate_label_positions <- function(data, group_var) {
  data %>% 
    ungroup() %>% 
    filter(year==max(data$year)) %>% 
    arrange(desc(!!sym(group_var))) %>%
    mutate(label_position=value) %>%
    mutate(label_position=cumsum(label_position)) %>%
    mutate(label_position=label_position-(value/2)) %>%
    select(!!sym(group_var),label_position)
}


save_figure <- function(plot, filename_prefix, height, width) {
  
  ggsave(plot, filename = paste0("results/standard/", filename_prefix, ".pdf"),
         device = cairo_pdf, height = height, width = width)
  
  ggsave(plot, filename = paste0("results/standard/", filename_prefix, ".png"), 
         device = "png", height = height, width = width, dpi = 300)
  
  ggsave(plot, filename = paste0("results/standard/", filename_prefix, ".svg"),
         device = "svg", height = height, width = width)
  
}


save_data <- function(data, sources, cite, filename_prefix) {
  
  wb <- createWorkbook()
  addWorksheet(wb,"info")
  addWorksheet(wb,"data")
  
  info = data.frame(a=c("Author","Contact","Data sources","See also"),
                    b=c("William F. Lamb",
                        "william.lamb@pik-potsdam.de",
                        paste(sources,collapse="; "),
                        ref_lamb))
  
  writeData(wb, sheet = "info",info,colNames = F, rowNames = F)
  writeData(wb, sheet = "data",data,colNames = T, rowNames = F)
  
  saveWorkbook(wb,paste0("results/standard/",filename_prefix,".xlsx"),overwrite=T)
  
}


add_growth_bracket <- function(plot, label_data, y_offset = 10) {
  start_year <- first(label_data$year)
  end_year <- last(label_data$year)
  y_pos <- last(label_data$value) + y_offset
  change_label <- label_data$change
  
  plot +
    annotate("segment", x = start_year, xend = end_year, y = y_pos, yend = y_pos,
             color = "#636363", size = 0.25) +
    annotate("segment", x = start_year, xend = start_year, y = y_pos, yend = y_pos - 2,
             color = "#636363", size = 0.25) +
    annotate("segment", x = end_year, xend = end_year, y = y_pos, yend = y_pos - 2,
             color = "#636363", size = 0.25) +
    annotate("text", x = (start_year + end_year)/2, y = y_pos + 2,
             label = change_label, vjust = 0, size = 3.5, color = "#636363")
}
