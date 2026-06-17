pacman::p_load(tidyverse, readxl)
pacman::p_load_gh("devanmcg/wesanderson")

setwd('C:/Users/devan.mcgranahan/GitHubProjects/MeasuringHeterogeneity')

examples = './data/CVexamples.xlsx' 
excel_sheets(examples)

read_tsv('./data/Hector2010Ecology/TemporalCVs.txt', 
         show_col_types = FALSE) %>%
  filter(level == 'Species') %>%
  ggplot(aes(x = tempMean, 
             y = tempCV, 
             color = site)) + theme_bw(14) +
  geom_smooth(method = 'lm', 
              formula = 'y ~ x', 
              se = F) +
  geom_point(size = 2) +
  # facet_wrap(~level, scale = 'free_x') + 
  coord_cartesian(ylim = c(5,170), xlim = c(30,900)) + 
  labs(x = "Mean biomass", 
       y = 'Population CV') + 
  scale_color_manual(values= wes_palette("GrandBudapest1", n = 8, type = 'continuous'), 
                     guide = 'none') +
  annotate("text", 
           x = 650, 
           y = 150, 
           label = '"The effect of diversity on the population CVs\n was significantly positive"\nHector et al. 2010 Ecology ')

  
read_xlsx(examples, 'fuhlendorf1999') %>%
  separate(YearBA, into = c('year', 'BA'), sep=', ')%>%
  separate(YearCV, into = c('year', 'CV'), sep=', ') %>%
  mutate(across(c(CV, BA), ~as.numeric(.))) %>%
  rename(Treatment = grazing) %>%
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
  scale_color_manual("Grazing\nintensity", values = rev(wes_palette("GrandBudapest1", n =3))) +
  scale_fill_manual("Grazing\nintensity", values = rev(wes_palette("GrandBudapest1", n =3)) )+
  scale_shape_manual("Grazing\nintensity", values = c(21, 24, 22)) +
  labs(x = "Basal area") +
  annotate("text", 
           x = 1000, 
           y = 450, 
           label = ' "There is an inverse relationship between grazing intensity\nand variation or heterogeneity within treatments."\nFuhlendorf & Smeins 1999 J Veg Sci ')




read_xlsx(examples, 'cain1999') %>%
  mutate(stage = factor(stage, levels = c("early", "middle", "late")) ) %>%
  ggplot(aes(x = mean_ppm, 
             y = CV)) + theme_bw(14) +
  geom_line(aes(color = species)) +
  geom_point(aes(color = species, 
                 shape = stage), 
             size = 3, 
             #color = 'lightgrey', 
             stroke = 1.5) +
  labs(x = "Mean (ppm)") + 
 # scale_shape_manual(values = c(21, 24, 22)) +
  annotate("text", 
           x = 1.5, 
           y = 350, 
           label = ' "Soil nitrogen concentrations in our\nearly successional site were among the most variable\nfound in ecosystems studied to date"\nCain et al. 1999 Oecologia ')

# TUrner et al 2011 Ecosystems

read_xlsx(examples, 'turner2011') %>%
  mutate(year = year - 2000, 
         year = as.factor(year) ) %>% 
  filter(type == 'veg') %>%
  ggplot(aes(x = mean, 
             y = cv)) + theme_bw(14) +
  geom_line(aes(color = var)) +
  geom_point(aes(fill = var, 
                 shape = var), 
             size = 6, 
             color = 'lightgrey', 
             stroke = 1.5) +
  geom_text(aes(label = year), 
            color = 'white', 
            fontface = 'bold') + 
  labs(x = "Mean (% cover)", 
       caption = "Numerals denote years since fire") + 
  scale_color_manual("Functional\ngroup", values = wes_palette("GrandBudapest1", n =4)) +
  scale_fill_manual("Functional\ngroup", values = wes_palette("GrandBudapest1", n =4)) +
  scale_shape_manual("Functional\ngroup", values = c(21, 24, 22, 23)) +
  annotate("text", 
           x = 25, 
           y = 450, 
           label = ' "Within-stand variability ... in postfire live vegetative cover \ndeclined with time since fire"\nTurner et al. 2011 Ecosystems ')



