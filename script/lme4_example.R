pacman::p_load(tidyverse)
# lme4::lmer
var_dat <- tibble() 
for(b in 1:length(unique(SimData$BiomassScenario))) {
for(s in 1:length(unique(SimData$sim))) {
    B = unique(SimData$BiomassScenario)[b]
    S = unique(SimData$sim)[s]
    # subset
    d <- SimData %>% 
      filter(BiomassScenario == B, 
             sim == S) 
    # fit model
    lme4::lmer(StandingCrop ~ 
                 (1|patch) , 
               d ,
               REML = TRUE) %>% 
      lme4::VarCorr() %>% 
      as_tibble() %>%
      select(grp, sdcor) %>%
      mutate(BS = B, 
             sim = S) %>%
      bind_rows(var_dat) -> var_dat
  }}