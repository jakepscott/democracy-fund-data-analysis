# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(haven)
library(here)
library(janitor)
library(sjlabelled)
library(glue)
library(ggtext)
library(showtext)

# Load Data ---------------------------------------------------------------

#Create function that looks in given folder and loads the DTa within it
load_func <- function(folder){
  given_file <- list.files(here(glue("data/Nationscape-DataRelease_WeeklyMaterials_DTA_200910_161918/Nationscape-DataRelease_WeeklyMaterials_DTA/{folder}"))) %>% 
    as_tibble() %>% 
    filter(str_detect(value, "dta"))
  
  read_dta(here(glue("data/Nationscape-DataRelease_WeeklyMaterials_DTA_200910_161918/Nationscape-DataRelease_WeeklyMaterials_DTA/{folder}/{given_file}"))) %>% 
    clean_names()
    
}

#Get list of folders for 2020
folders_2020 <- list.files("data/Nationscape-DataRelease_WeeklyMaterials_DTA_200910_161918/Nationscape-DataRelease_WeeklyMaterials_DTA/phase_2_v20200814/") %>% 
  as_tibble() %>% 
  filter(!str_detect(value, "csv")) %>% 
  mutate(value = glue("phase_2_v20200814/{value}"))

#Get list of folders for 2019
folders_2019 <- list.files("data/Nationscape-DataRelease_WeeklyMaterials_DTA_200910_161918/Nationscape-DataRelease_WeeklyMaterials_DTA/phase_1_v20200814/") %>% 
  as_tibble() %>% 
  filter(!str_detect(value, "csv")) %>% 
  mutate(value = glue("phase_1_v20200814/{value}"))

# Join 2020 and 2019 folders
folders <- folders_2019 %>% 
  bind_rows(folders_2020)

# Load all the dta files in all the folders
  data <- map_dfr(folders$value, load_func)


# Right track plot --------------------------------------------------------

right_direction <- data %>% 
  select(start_date,right_track, weight) %>%
  filter(!is.na(right_track)) %>% 
  mutate(start_date = as.Date(start_date)) %>% 
  mutate(right_track = haven::as_factor(right_track)) %>% 
  count(start_date, right_track, wt = weight) %>% 
  group_by(start_date) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  ungroup()


recessions.df = read.table(textConnection(
  "Peak, Trough
2020-02-01, 2020-04-01"), sep=',',
colClasses=c('Date', 'Date'), header=TRUE)

font_add_google("Roboto", "roboto")

right_direction %>% 
  ggplot() +
  geom_rect(data=recessions.df, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.5) +
  geom_line(aes(start_date, percent, color = right_track),
            show.legend = F,
            lwd = 1) +
  scale_y_continuous(labels = function(y){glue("{y}%")}) +
  scale_color_manual(values = c("blue","red","grey")) +
  scale_x_date(date_breaks = "2 months") +
  labs(title = "Percent of people who say the country is on the <span style = 'color: blue;'>right</span> versus <span style = 'color: red;'>wrong</span> track",
       subtitle = "Grey shading indicates the Covid recession",
       caption = "Plot: @jakepscott2020 | Data: Democracy Fund",
       y = NULL,
       x = NULL) +
  theme_minimal(base_family = "roboto", 
                base_size = 12) +
  theme(plot.title = element_markdown(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey20"),
        plot.caption = element_text(size = rel(0.9), face="italic", 
                                    color = "grey40"),
        strip.text = element_text(face="bold",size = rel(1)),
        legend.title = element_text(size = rel(.8)),
        plot.title.position = "plot", 
        axis.title.y = element_blank(),
        legend.position = "none")
    
ggsave(here("figures/right-track.png"), dpi = 600, bg = "white",
       height = 6, width = 6*1.62, units = "in")
