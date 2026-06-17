pacman::p_load(tidyverse)

folder <- './data/GherardiSala'

files <- list.files(folder, pattern = "*.csv") 

GherardiSala <- tibble() 

for(i in 1:length(files)) {  
  paste0(folder, '/', files[i]) %>% 
  read_csv() %>%
  bind_rows(., GherardiSala) -> GherardiSala 
}

# save(GherardiSala, file = paste0(folder, '/', 'GherardiSala.Rdata') )
