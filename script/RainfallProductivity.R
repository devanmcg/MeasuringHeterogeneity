pacman::p_load(tidyverse, magrittr, readxl)
pacman::p_load_gh("devanmcg/wesanderson")

setwd('C:/Users/devan.mcgranahan/GitHubProjects/MeasuringHeterogeneity')

examples = './data/CVexamples.xlsx' 
excel_sheets(examples) 

# Israel data from Golodets 2013

IsraelSumm <-
  read_xlsx(examples, 'Golodets2013') %>%
    separate(mm_ANPP, into = c('precip', 'ANPP'), sep=', ') %>%
    mutate(across(c(precip, ANPP), ~as.numeric(.))) %>%
    pivot_longer(names_to = 'var', 
                 values_to = 'value', 
                 -location) %>%
    group_by(location, var) %>%
    summarize(Mean = mean(value), 
              CV = (sd(value)/mean(value) ) * 100, 
              SEM = sd(value)/sqrt(n()), 
              .groups = 'drop')

A_gg <- 
  IsraelSumm %>%
  select(-CV) %>%
  pivot_wider(names_from = var, 
              values_from = c(Mean, SEM)) %>%
  ggplot(aes(x = Mean_precip, 
             y = Mean_ANPP)) + theme_bw(14) +
  geom_smooth(method = 'lm', 
              formula = 'y ~ x', 
              se = F, lwd = 1.25, 
              color =  wes_palette("Royal2")[5]) +
  geom_errorbarh(aes(xmin = Mean_precip - SEM_precip,
                     xmax = Mean_precip + SEM_precip), 
                 color =  wes_palette("Royal2")[5], 
                 height = 10, lwd = 1)+ 
  geom_errorbar(aes(ymin = Mean_ANPP - SEM_ANPP,
                    ymax = Mean_ANPP + SEM_ANPP), 
                color =  wes_palette("Royal2")[5], 
                width = 10, lwd = 1)+ 
  geom_point(size = 3, 
             pch = 24, 
             color =  wes_palette("Royal2")[5],
             fill = wes_palette("Royal2")[4],
             stroke = 1.5) +
  #coord_cartesian(xlim = c(100,800)) + 
  labs(y = expression("ANPP (g"~dm^-1~")"), 
       x = "Precipitation (mm)")

B_gg <- 
  IsraelSumm %>%
  filter(var == 'precip') %>%
  ggplot(aes(x = Mean, 
             y = CV)) + theme_bw(14) + 
  geom_smooth(method = 'lm', 
              formula = 'y ~ x', 
              se = F, lwd = 1.25, 
              color =  wes_palette("Darjeeling2")[2]) +
  geom_point(size = 3, 
             pch = 24, 
             color =  wes_palette("Darjeeling2")[2],
             fill = wes_palette("Darjeeling2")[4],
             stroke = 2) +
  #coord_cartesian(xlim = c(100,800)) + 
  labs(x = "Mean precipitation (mm)",
       y = "Precipitation CV") 

C_gg <- 
full_join(by = 'location', 
  IsraelSumm %>%
    filter(var == 'precip') %>%
    rename(MeanPrecip = Mean) %>%
    select(location, MeanPrecip) , 
  
  IsraelSumm %>%
    filter(var == 'ANPP') %>%
    rename(VarANPP = CV) %>%
    select(location, VarANPP) 
  ) %>%
  ggplot(aes(x = MeanPrecip, 
             y = VarANPP)) + theme_bw(14) + 
    geom_smooth(method = 'lm', 
                formula = 'y ~ (x^3)', 
                se = F, 
                color =  wes_palette("GrandBudapest1")[3]) +
    geom_point(size = 3, 
               pch = 24, 
               color =  wes_palette("GrandBudapest1")[3],
               fill = wes_palette("GrandBudapest1")[2],
               stroke = 1.5) +
  coord_cartesian(xlim = c(100,800)) + 
    labs(x = "Annual rainfall (mm)", 
         y = "ANPP CV") 

D_gg <- 
  IsraelSumm %>%
    filter(var == 'ANPP') %>%
    ggplot(aes(x = Mean, 
               y = CV)) + theme_bw(14) + 
    geom_smooth(method = 'lm', 
                formula = 'y ~ x', 
                se = F, lwd = 1.25, 
                color =  wes_palette("Royal2")[5]) +
    geom_point(size = 3, 
               pch = 24, 
               color =  wes_palette("Royal2")[5],
               fill = wes_palette("Royal2")[4],
               stroke = 2) +
    #coord_cartesian(xlim = c(100,800)) + 
    labs(x = expression("Mean ANPP (g"~dm^-1~")"), 
         y = "ANPP CV") 

plot_grid(ggdraw(A_gg) + draw_figure_label("A", "top.left", 18,  "bold"), 
          ggdraw(B_gg) + draw_figure_label("B", "top.left", 18,  "bold"), 
          ggdraw(C_gg) + draw_figure_label("C", "top.left", 18,  "bold"), 
          ggdraw(D_gg) + draw_figure_label("D", "top.left", 18,  "bold"),
          align = 'hv')

# Global data from Gherardi & Sala 2018

load('./data/GherardiSala/GherardiSala.Rdata')

GherardiSala %<>% rename(location = site, ANPP = npp, precip = ppt) %>%
  filter(complete.cases(.))

GSSumm <-
  GherardiSala  %>%
  mutate(across(c(precip, ANPP), ~as.numeric(.))) %>%
  pivot_longer(names_to = 'var', 
               values_to = 'value', 
               -location) %>%
  group_by(location, var) %>%
  summarize(Mean = mean(value), 
            CV = (sd(value)/mean(value) ) * 100, 
            SEM = sd(value)/sqrt(n()), 
            .groups = 'drop')

Ags_gg <- 
  GSSumm %>%
  select(-CV) %>%
  pivot_wider(names_from = var, 
              values_from = c(Mean, SEM)) %>%
  ggplot(aes(x = Mean_precip, 
             y = Mean_ANPP)) + theme_bw(14) +
  geom_smooth(method = 'lm', 
              formula = 'y ~ x', 
              se = F, lwd = 1.25, 
              color =  wes_palette("Royal2")[5]) +
  geom_errorbarh(aes(xmin = Mean_precip - SEM_precip,
                     xmax = Mean_precip + SEM_precip), 
                 color =  wes_palette("Royal2")[5], 
                 height = 10, lwd = 1)+ 
  geom_errorbar(aes(ymin = Mean_ANPP - SEM_ANPP,
                    ymax = Mean_ANPP + SEM_ANPP), 
                color =  wes_palette("Royal2")[5], 
                width = 10, lwd = 1)+ 
  geom_point(size = 3, 
             pch = 24, 
             color =  wes_palette("Royal2")[5],
             fill = wes_palette("Royal2")[4],
             stroke = 1.5) +
  #coord_cartesian(xlim = c(100,800)) + 
  labs(y = expression("ANPP  "), 
       x = "Precipitation (mm)")

Bgs_gg <- 
  GSSumm %>%
  filter(var == 'precip') %>%
  ggplot(aes(x = Mean, 
             y = CV)) + theme_bw(14) + 
  geom_smooth(method = 'lm', 
              formula = 'y ~ x', 
              se = F, lwd = 1.25, 
              color =  wes_palette("Darjeeling2")[2]) +
  geom_point(size = 3, 
             pch = 24, 
             color =  wes_palette("Darjeeling2")[2],
             fill = wes_palette("Darjeeling2")[4],
             stroke = 2) +
  #coord_cartesian(xlim = c(100,800)) + 
  labs(x = "Mean precipitation (mm)",
       y = "Precipitation CV") 

Cgs_gg <- 
  full_join(by = 'location', 
            GSSumm %>%
              filter(var == 'precip') %>%
              rename(MeanPrecip = Mean) %>%
              select(location, MeanPrecip) , 
            
            GSSumm %>%
              filter(var == 'ANPP') %>%
              rename(VarANPP = CV) %>%
              select(location, VarANPP) 
  ) %>%
  ggplot(aes(x = MeanPrecip, 
             y = VarANPP)) + theme_bw(14) + 
  geom_smooth(method = 'lm', 
              formula = 'y ~ (x^3)', 
              se = F, 
              color =  wes_palette("GrandBudapest1")[3]) +
  geom_point(size = 3, 
             pch = 24, 
             color =  wes_palette("GrandBudapest1")[3],
             fill = wes_palette("GrandBudapest1")[2],
             stroke = 1.5) +
  #coord_cartesian(xlim = c(200,1100)) + 
  labs(x = "Annual rainfall (mm)", 
       y = "ANPP CV") 

Dgs_gg <- 
  GSSumm %>%
  filter(var == 'ANPP') %>%
  ggplot(aes(x = Mean, 
             y = CV)) + theme_bw(14) + 
  geom_smooth(method = 'lm', 
              formula = 'y ~ x', 
              se = F, lwd = 1.25, 
              color =  wes_palette("Royal2")[5]) +
  geom_point(size = 3, 
             pch = 24, 
             color =  wes_palette("Royal2")[5],
             fill = wes_palette("Royal2")[4],
             stroke = 2) +
  #coord_cartesian(xlim = c(100,800)) + 
  labs(x = expression("Mean ANPP "), 
       y = "ANPP CV") 

plot_grid(ggdraw(Ags_gg) + draw_figure_label("A", "top.left", 18,  "bold"), 
          ggdraw(Bgs_gg) + draw_figure_label("B", "top.left", 18,  "bold"), 
          ggdraw(Cgs_gg) + draw_figure_label("C", "top.left", 18,  "bold"), 
          ggdraw(Dgs_gg) + draw_figure_label("D", "top.left", 18,  "bold"),
          align = 'hv')

# Preciptation only
  # Amboseli Park
  read_xlsx(examples, 'BronikowskiAltmann1996') %>%
    ggplot(aes(x = mm, 
               y = cv)) + theme_bw(14) + 
    geom_smooth(method = 'lm', 
                formula = 'y ~ (x^3)', 
                se = F, 
                color =  wes_palette("Darjeeling2")[2]) +
    geom_point(size = 3, 
               pch = 24, 
               color =  wes_palette("Darjeeling2")[2],
               fill = wes_palette("Darjeeling2")[4],
               stroke = 1.5) +
    labs(x = "Annual rainfall (mm)", 
         y = "CV") 
  # Oklahoma 
  read_csv('./data/OKmesonet/DailyPrecip.csv', 
            show_col_types = FALSE) %>%
    filter(RAIN >= 0) %>%
    group_by(STID, YEAR) %>%
    summarize(Rain = sum(RAIN), 
              .groups = 'drop_last') %>%
    summarize(Mean = mean(Rain), 
              CV = (sd(Rain)/mean(Rain) ) * 100, 
              .groups = 'drop')  %>%
    filter(Mean > 10) %>%
    ggplot(aes(x = (Mean * 2.54) * 10, 
               y = CV)) + theme_bw(14) + 
    geom_smooth(method = 'lm', 
                formula = 'y ~ x', 
                se = F, 
                color =  wes_palette("Darjeeling2")[2]) +
    geom_point(size = 3, 
               pch = 24, 
               color =  wes_palette("Darjeeling2")[2],
               fill = wes_palette("Darjeeling2")[4],
               stroke = 1.5) +
    labs(x = "Mean annual rainfall (mm)", 
         y = "CV") 
  
# Effect of sample size?
  ok_d <- 
  read_csv('./data/OKmesonet/DailyPrecip.csv', 
           show_col_types = FALSE) %>%
    filter(RAIN >= 0, 
           ! STID %in% c('FAI2', 'ALVA')) %>% 
    group_by(STID, YEAR) %>%
    summarize(Rain = sum(RAIN), 
              .groups = 'drop_last')
  
  
  
draws = c(5, 10, 15, 20, 25, 30 )
N = 1000
sim_res <- tibble() 
sim_st <- tibble() 
for(i in 1:length(draws)){ 
  draw_res <- tibble() 
  draw_st <- tibble() 
  for(n in 1:N) {
  yrs <- sample(ok_d$YEAR, size=draws[i], replace = F)
  ok_sum <-
   ok_d %>%
    filter(YEAR %in% yrs) %>%
      summarize(Mean = mean(Rain), 
                CV = (sd(Rain)/mean(Rain) ) * 100, 
                .groups = 'drop') 
    lm(CV ~ Mean, ok_sum)$coefficients %>%
      as_tibble() %>%
      slice(2) %>%
      bind_rows(draw_res) -> draw_res
    ok_sum %>%
      mutate(sim = n) %>%
      bind_rows(draw_st) -> draw_st
  }
  draw_res %>%
    summarise(ciL= quantile(value, probs = 0.025),
                            median=median(value), 
              ciU = quantile(value, probs = 0.975))  %>%
    mutate(draws = draws[i]) %>%
    bind_rows(., sim_res) -> sim_res
  draw_st %>%
    pivot_longer(names_to = 'var', 
                 values_to = 'value', 
                 cols=c(Mean, CV)) %>%
    group_by(STID, var) %>%
    summarise(ciL= quantile(value, probs = 0.025, na.rm = TRUE),
              median=median(value, na.rm = TRUE), 
              ciU = quantile(value, probs = 0.975, na.rm = TRUE))  %>%
    mutate(draws = draws[i]) %>%
    bind_rows(., sim_st) -> sim_st
    
}

sim_res %>%
  ggplot() + theme_bw(14) + 
  geom_errorbar(aes(x = draws, 
                    ymin = ciL, 
                    ymax = ciU)) + 
  geom_point(aes(x = draws, 
                 y = median))

sim_st %>%
 # filter(STID %in% unique(sim_st$STID)[1:5]) %>%
  ggplot(aes(x = draws, 
             y = median)) + theme_bw(14) + 
  # geom_errorbar(aes(ymin = ciL, 
  #                   ymax = ciU), 
  #               width = 1, 
  #               lwd = 0.5, 
  #               color = 'grey80') + 
  geom_smooth(aes(group = STID), 
                  method = 'lm', 
              formula = 'y ~ poly(x, 2)', 
              se = F, 
              lwd = 0.9, color = 'lightblue') + 
  #geom_point(pch = 21, size = 4, fill = 'white', color = 'grey80') +
  geom_smooth(data = . %>%
                group_by(var, draws) %>%
                summarize(median = mean(median)), 
                method = 'lm', 
              formula = 'y ~ poly(x, 2)', 
              se = T, lwd = 1,
              color = 'black', fill = 'darkblue') + 
  facet_wrap(~ var, scales = "free_y") +
  labs(x = "Sample size", 
       y= "95% CI") +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30), 
                   labels = c(5, 10, 15, 20, 25, 30)) +
  theme(panel.grid.minor.x = element_blank())

sim_grps <- 
  sim_st %>%
  select(STID, var, draws, median) %>%
  filter(! is.na(median)) %>%
  group_by(STID, var,  .add = TRUE)

sim_tau <-  
  bind_cols(
    group_keys(sim_grps), 
    sim_grps %>%
      group_split( ) %>%
      map(~ EnvStats::kendallTrendTest(median ~ draws, data = .))  %>%
      map_dfr(~ as.data.frame(t(as.matrix(c(round(.$p.value, 3),
                                            .$estimate[1:2], 
                                            .$interval$limits))))) 
  ) %>%
  rename( P = z) %>%
  select(STID, var, P, tau, P, slope)

sim_tau %>%
  filter(STID != 'WEB3') %>%
  mutate(sig = ifelse(P < 0.05, 1, 0)) %>%
  group_by(var) %>%
  summarize(sig = sum(sig)/n(), 
            MedianSlope = median(slope), 
            ciL = quantile(slope, 0.025), 
            ciU = quantile(slope, 0.975))
    
  