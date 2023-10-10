pacman::p_load(tidyverse, readxl)

examples = './data/CVexamples.xlsx' 



read_xlsx(examples, 'cain1999') %>%
  mutate(stage = factor(stage, levels = c("early", "middle", "late")) ) %>%
  ggplot(aes(x = mean_ppm, 
             y = CV)) + theme_bw(14) +
  geom_line(aes(color = species)) +
  geom_point(aes(fill = species, 
                 shape = stage), 
             size = 3, 
             color = 'lightgrey', 
             stroke = 1.5) +
  labs(x = "Mean (ppm)") + 
  scale_shape_manual(values = c(21, 24, 22)) +
  annotate("text", 
           x = 1.5, 
           y = 350, 
           label = ' "Soil nitrogen concentrations in our\nearly successional site were among the most variable\nfound in ecosystems studied to date"\nCain et al. 1999 Oecologia ')


read_xlsx(examples, 'turner2011') %>%
  mutate(year = as.factor(year)) %>% 
  filter(type == 'veg') %>%
  ggplot(aes(x = mean, 
             y = cv, 
             color = var)) + theme_bw(14) +
  geom_line(aes( )) +
  geom_point(aes(fill = var, 
                 shape = year), 
             size = 5, 
             color = 'lightgrey', 
             stroke = 1.5) +
  geom_text(aes(label = substr(year, 4,4)), 
            color = 'white') + 
  labs(x = "Mean (% cover)") + 
  scale_shape_manual(values = c(21, 24, 22, 23)) +
  annotate("text", 
           x = 25, 
           y = 450, 
           label = ' "Within-stand variability ... in postfire live vegetative cover \ndeclined with time since fire"\nTurner et al. 2011 Ecosystems ')
