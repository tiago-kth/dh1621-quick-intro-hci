library(tidyverse)
library(extrafont)
extrafont::loadfonts()

data_raw <- read.csv('survey-group-project.csv')

colnames(data_raw) <- c(
  'timestamp',
  'type_student',
  'main_info_sources',
  'wi_fi',
  'printing',
  'paperwork',
  'library',
  'student_life',
  'academics',
  'accommodation',
  'sports',
  'union',
  'getting_around',
  'sweden',
  'other'
)




# main information sources ------------------------------------------------

#max(data_raw$main_info_sources %>% str_count(';'))
#sum(data_raw$main_info_sources %>% str_count(';') + 1)
# 4 columns

main_info <- data_raw %>%
  select(main_info_sources) %>%
  separate(
    main_info_sources, 
    into = c('info1', 'info2', 'info3', 'info4'), 
    sep = ';', 
    remove = TRUE) %>%
  gather(key = 'info_source_no', value = 'info_source') %>%
  filter(!is.na(info_source)) %>%
  select(info_source) %>%
  count(info_source) %>%
  mutate(info_source_new = ifelse(n == 1, 'Other', info_source)) %>%
  group_by(info_source_new) %>%
  summarise(n = sum(n))

ggplot(main_info) +
  geom_col(aes(
    x = n, 
    y = reorder(info_source_new, n),
    fill = n == max(main_info$n)
  ), width = .5) +
  geom_text(aes(
    x = n + .5, 
    y = reorder(info_source_new, n),
    color = n == max(main_info$n),
    label = n,
    family = 'Fira Code',
    hjust = 'left'
  )) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c('TRUE' = 'coral', 'FALSE' = 'gray')) +
  scale_color_manual(values = c('TRUE' = 'coral', 'FALSE' = 'gray')) +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Code'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'none'
        )

ggsave(plot = last_plot(), width = 6, height = 3, filename = 'plot-main-sources.png')
           


# aspects -----------------------------------------------------------------

data_aspects <- data_raw %>%
  select(-timestamp, -type_student, -main_info_sources, -other) %>%
  gather(key = 'aspect', value = 'value') %>%
  count(aspect, value) %>%
  mutate(value = factor(value, 
  levels = rev(c(
  "Helpful and enough" = "H/E", 
  "Helpful, but not enough" = "H/NE",
  "Not helpful" = "NH",
  "I didn't even know that the website had this information!" = "IDK",
  "I am not interested in this information" = "NI"
  )),
  labels = rev(c(
    "Helpful and enough", 
    "Helpful, but not enough",
    "Not helpful",
    "I didn't even know that the website had this information!",
    "I am not interested in this information"
  ))))

order <- data_aspects %>%
  spread(key = value, value = n) %>%
  mutate(helpful = `Helpful, but not enough` + `Helpful and enough`) %>%
  select(aspect, helpful) %>%
  arrange(helpful)

data_aspects2 <- data_aspects %>% left_join(order)

ggplot(data_aspects2) + 
  geom_col(aes(y = reorder(aspect, helpful), x = n, fill = value), width = .7) +
  scale_fill_manual(
    values = c(
      "Helpful and enough" = "steelblue",
      "Helpful, but not enough" = "dodgerblue",
      "Not helpful" = "coral",
      "I didn't even know that the website had this information!" = "gold",
      "I am not interested in this information" = "gray"
    )
  ) +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Code'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 8),
        legend.direction = 'vertical'
  )
