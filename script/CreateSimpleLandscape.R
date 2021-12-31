pacman::p_load(tidyverse, sf, wesanderson)
# ensure simulated dataset is loaded 
  load('./data/sim_data.Rdata')
# summary statistics of simulated dataset
  sim_sums <- 
    sim_data %>%
    group_by(crop) %>%
    summarize(count = n(),
              CropMean = round(mean(PerProd, na.rm = T), 0), 
              CropSD = round(sd(PerProd, na.rm = T), 1),
              .groups = 'drop')
#
# Landscape simulations
# 
# Create the simulation landscape and points
# Create landscape with 3 patches
  simscape1 <- 
    bind_rows(
      tibble(patch = 'B', 
             x = c(0,0,1200,1200), 
             y = c(0,400,400,0)) , 
      tibble(patch = 'C', 
             x = c(0,0,1200,1200), 
             y = c(400,800,800,400)), 
      tibble(patch = 'T', 
             x = c(0,0,1200,1200), 
             y = c(800,1200,1200,800))  
    ) %>%
    st_as_sf(coords = c('x', 'y')) %>%
    group_by(patch) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  # save(simscape1, file = './data/simscape1.Rdata')

# create gridded landscape (10 m cells)    
# vector polygons; raster would be better ¯\_(ツ)_/¯
  SimCells1 <- 
    simscape1 %>%
    group_by(patch) %>%
    st_make_grid(10) %>%
    st_sf() %>%
    st_join(simscape1)%>%
    arrange(patch) %>%
    rownames_to_column('point') 

# Assign points to biomass class
  SimCells1_sf <-
    SimCells1 %>% 
    group_by(patch, point) %>% 
    group_modify(~ {
      tibble(hom = sample(2:4, 1))
        }) %>%
    ungroup() %>%
    mutate(hom = recode(as.character(hom), 
                        '2' = "L",
                        '3' = "M", 
                        '4' = "H")) %>%
    mutate(het = recode(as.character(patch), 
                        'B' = "L",
                        'C' = "M", 
                        'T' = "H")) %>%
    pivot_longer(cols = c('hom', 'het'), 
                 names_to = "sim", 
                 values_to = "class") %>%
    full_join(., SimCells1,  by = c('patch', 'point')) %>%
    st_as_sf()  %>%
    mutate(patch = factor(patch, levels = c('T','C','B')),
           patch = recode(patch, 
                          'B' = 'Bottom', 
                          'C' = "Center", 
                          'T' = "Top"), 
           sim = recode(sim, 
                        'hom' = 'Homogeneous', 
                        'het' = "Heterogeneous"))
# Assign biomass to cells based on class moments
# k = multipliers for standing crop 
  K = c(0.5, 0.75, 1.0, 1.25, 1.5)
  SimData1 <- tibble() 
  for(k in 1:length(K)) {
    for(c in 1:length(unique(sim_sums$crop)[2:4])) {
      C = as.character(unique(sim_sums$crop)[2:4][c]) 
      Csum <- filter(sim_sums, crop == C)
      Cpts <- filter(SimCells1_sf, class == C) %>% as_tibble() 
      Cpts %>%
        mutate(StandingCrop = rnorm(nrow(Cpts), 
                                    mean = Csum$CropMean*K[k], 
                                    sd = mean(slice(sim_sums, 2:4)$CropSD)), 
               BiomassScenario = K[k] ) %>% 
        rbind(., SimData1) -> SimData1
    }
  }
# save(SimData1, file = './data/SimData1.Rdata')


# create sample points (~10 per patch)
  SimPoints1 <-  
    simscape1 %>%
    st_as_sf() %>%
    group_by(patch) %>%
    st_buffer(-100) %>%
    st_sample(size = c(10,10,10), 
              type = "random", 
              exact = T, 
              by_polygon = TRUE) %>%
    st_sf() %>%
    st_buffer(18) %>%
    st_join(simscape1)%>%
    arrange(patch) %>%
    rownames_to_column('point')
  # save(SimPoints1, file = './data/SimPoints1.Rdata')
  
  
    