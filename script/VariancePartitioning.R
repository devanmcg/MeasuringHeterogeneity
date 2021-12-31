pacman::p_load(tidyverse, rptR)

# 
# Simple Simscape analysis
#
  load('./Rdata/SimData1.Rdata')
  
  VarDatrptR_1 <-  tibble() 
  for(b in 1:length(unique(SimData1$BiomassScenario))) {
    for(s in 1:length(unique(SimData1$sim))) {
      B = unique(SimData1$BiomassScenario)[b]
      S = unique(SimData1$sim)[s]
      d <- SimData1 %>% 
        filter(BiomassScenario == B, 
               sim == S) 
      # Fit random-effect model
      rpt <- rptGaussian(StandingCrop ~ 
                           (1|patch) , 
                         grname=c("patch"), 
                         data=d, 
                         nboot=0,
                         npermut = 0,
                         #ncores = parallel::detectCores(),
                         ratio = FALSE) 
      # Wrangle output
      summary(rpt)$R %>%
        t() %>%
        as.data.frame() %>%
        rename(variance = V1) %>%
        rownames_to_column("term")  %>%
        mutate(BS = B, 
               sim = S) %>%
        bind_rows( VarDatrptR_1) -> VarDatrptR_1
    }  }
  # save(VarDatrptR_1, file = './data/VarDatrptR_1.Rdata')
  



