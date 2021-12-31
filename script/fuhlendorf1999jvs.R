pacman::p_load(tidyverse)

load('./data/fuhlendorf1999jvs.Rdata')

fuhlendorf1999jvs %>%
  as_tibble() %>%
  mutate(across(c(CV, BA), ~as.numeric(.))) %>%
  ggplot(aes(x = BA, 
             y = CV, 
             color = Treatment, 
             fill = Treatment, 
             shape = Treatment)) + theme_bw(14) + 
    geom_smooth(method = 'lm', 
                formula = 'y ~ (x^3)', 
                se = F) +
    geom_point(size = 3, 
               color = 'lightgrey', 
               stroke = 1.25) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Annual basal area", 
       caption = "Fuhlendorf & Smeins 1999 JVS\nhttps://doi.org/10.2307/3237026")
