quick_plot <- function(variable, data) {
  
  right_direction <- data %>% 
    select(start_date,{{variable}}, weight) %>%
    filter(!is.na({{variable}})) %>% 
    mutate(start_date = as.Date(start_date)) %>% 
    mutate({{variable}} := haven::as_factor({{variable}})) %>% 
    count(start_date, {{variable}}, wt = weight) %>% 
    group_by(start_date) %>% 
    mutate(percent = n/sum(n)*100) %>% 
    ungroup()
  
  
  recessions.df = read.table(textConnection(
    "Peak, Trough
2020-02-01, 2020-04-01"), sep=',',
colClasses=c('Date', 'Date'), header=TRUE)
  
  
  right_direction %>% 
    ggplot() +
    geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5) +
    geom_line(aes(start_date, percent, color = {{variable}}),
              show.legend = T,
              lwd = 1) +
    scale_y_continuous(labels = function(y){glue("{y}%")}) +
    scale_x_date(date_breaks = "2 months") +
    labs(subtitle = "Grey shading indicates the Covid recession",
         caption = "Plot: @jakepscott2020 | Data: Democracy Fund",
         y = NULL,
         x = NULL) +
    theme_minimal(base_family = "roboto", 
                  base_size = 12) +
    theme(legend.position = "top")
  
}

quick_plot(news_sources_cnn, data)
