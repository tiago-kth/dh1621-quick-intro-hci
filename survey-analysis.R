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
      "I am not interested in this information"))
    ),
    aspect = factor(aspect,
                    levels = c(
                      "Academics" = "academics",
                      "Accommodation" = "accommodation",
                      "Getting around Campus" = "getting_around",
                      "KTH Library" = "library",
                      "Student Life, Canvas, Ladok etc." = "student_life",
                      "Paperwork" = "paperwork",
                      "Using KTH Wi-Fi" = "wi_fi",
                      "THS" = "union",
                      "Sports and extracurricular activities" = "sports",
                      "Using printers on Campus" = "printing",
                      "Information about Stockholm and Sweden" = "sweden"
                    ),
                    labels = c(
                      levels = c(
                        "Academics",
                        "Accommodation",
                        "Getting around Campus",
                        "KTH Library",
                        "Student Life, Canvas, Ladok etc.",
                        "Paperwork",
                        "Using KTH Wi-Fi",
                        "THS",
                        "Sports and extracurricular activities",
                        "Using printers on Campus",
                        "Information about Stockholm and Sweden"
                      )
                    )))

order <- data_aspects %>%
  spread(key = value, value = n) %>%
  #mutate(helpful = `Helpful, but not enough` + `Helpful and enough`) %>%
  mutate(helpful = `Helpful and enough`) %>%
  select(aspect, helpful) %>%
  arrange(helpful)

data_aspects2 <- data_aspects %>% left_join(order)

ggplot(data_aspects2) + 
  geom_col(aes(y = reorder(aspect, helpful), x = n/30, fill = value), width = .7) +
  scale_fill_manual(
    values = c(
      "Helpful and enough" = "steelblue",
      "Helpful, but not enough" = "dodgerblue",
      "Not helpful" = "coral",
      "I didn't even know that the website had this information!" = "gold",
      "I am not interested in this information" = "gray"
    )
  ) +
  scale_x_continuous(labels=scales::percent) +
  labs(fill = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Code'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 8),
        legend.direction = 'vertical'
  )

ggsave(filename = "overview-aspects.png", plot = last_plot(), width = 9, height = 6)


plot <- function(crit, cor) {
  
  ggplot(data_aspects2 %>% filter(value == crit),
         aes(y = reorder(aspect, n), x = n/30)) + 
    geom_col(fill = cor, width = .7) +
    geom_text(aes(label = scales::percent(n/30, accuracy = 1)), 
              color = cor, hjust = 'left', nudge_x = .005, family = "Fira Code",
              size = 3) +
    scale_x_continuous(labels=scales::percent, limits = c(0,.7)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(text = element_text(family = 'Fira Code'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'bottom',
          legend.text = element_text(size = 8),
          legend.direction = 'vertical'
    )
  
}

plot("Helpful and enough", "steelblue")
ggsave(filename = "helpful-aspects.png", plot = last_plot(), width = 6, height = 3.5)

plot("Helpful, but not enough", "dodgerblue")
ggsave(filename = "not-enough-helpful-aspects.png", plot = last_plot(), width = 6, height = 3.5)

plot("Not helpful", "coral")
ggsave(filename = "not-helpful-aspects.png", plot = last_plot(), width = 6, height = 3.5)

plot("I didn't even know that the website had this information!", "gold")
ggsave(filename = "not-even-know-aspects.png", plot = last_plot(), width = 6, height = 3.5)



