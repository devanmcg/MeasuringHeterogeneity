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
  
  df <-     bind_rows(
    tibble(patch = 'B', 
           x = c(0,0,1200,1200), 
           y = c(0,400,400,0)) , 
    tibble(patch = 'C', 
           x = c(0,0,1200,1200), 
           y = c(400,800,800,400)), 
    tibble(patch = 'T', 
           x = c(0,0,1200,1200), 
           y = c(800,1200,1200,800))  
  )
  library(raster)
  dfr <- raster::rasterFromXYZ(df, res = 10)  #Convert first two columns as lon-lat and third as value                
  plot(dfr)
  
  spg <- df
  sp::coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  sp::gridded(spg) <- TRUE
  # coerce to raster
  rasterDF <- raster::raster(spg)

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
  # A different construction for each timestep
  SimCellsTemp <-
    bind_rows(
    SimCells1 %>%
    group_by(patch, point) %>% 
    group_modify(~ {
      tibble(hom = sample(2:4, 1))
        }) %>%
    ungroup() %>%
    mutate(timestep = 1, 
           hom = recode(as.character(hom), 
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
                        'het' = "Heterogeneous")), 
    SimCells1 %>% 
      group_by(patch, point) %>% 
      group_modify(~ {
        tibble(hom = sample(2:4, 1))
      }) %>%
      ungroup() %>%
      mutate(timestep = 2, 
              hom = recode(as.character(hom), 
                          '2' = "L",
                          '3' = "M", 
                          '4' = "H")) %>%
      mutate(het = recode(as.character(patch), 
                          'C' = "L",
                          'T' = "M", 
                          'B' = "H")) %>%
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
                          'het' = "Heterogeneous")), 
    SimCells1 %>% 
      group_by(patch, point) %>% 
      group_modify(~ {
        tibble(hom = sample(2:4, 1))
      }) %>%
      ungroup() %>%
      mutate( timestep = 3, 
              hom = recode(as.character(hom), 
                          '2' = "L",
                          '3' = "M", 
                          '4' = "H")) %>%
      mutate(het = recode(as.character(patch), 
                          'T' = "L",
                          'B' = "M", 
                          'C' = "H")) %>%
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
    )
  
# Assign biomass to cells based on class moments
# k = multipliers for standing crop 
{
  K = c(0.5, 0.75, 1.0, 1.25, 1.5)
  begin = Sys.time()
  if("cl" %in% ls()) rm(cl)
  if(! "cores" %in% ls()) cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  SimData2 <-
  foreach(k=1:length(K), 
          .combine=bind_rows,
          .inorder = FALSE) %:%
  foreach(t=1:length(unique(SimCellsTemp$timestep)), 
          .combine=bind_rows,
          .inorder = FALSE) %:%
  foreach(c=1:length(unique(sim_sums$crop)[2:4]), 
          .combine=bind_rows, 
          .inorder = FALSE,
          .errorhandling = 'stop', 
          .packages=c('tidyverse', 'sf')) %dopar% {
        C = as.character(unique(sim_sums$crop)[2:4][c]) 
        Csum <- filter(sim_sums, crop == C)
        Cpts <- filter(SimCellsTemp, class == C, timestep == t) %>% as_tibble() 
        Cpts %>%
          mutate(StandingCrop = rnorm(nrow(Cpts), 
                                      mean = Csum$CropMean*K[k], 
                                      sd = mean(slice(sim_sums, 2:4)$CropSD)), 
                 BiomassScenario = K[k] ) 
          }
        stopCluster(cl)
        Sys.time() - begin 
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
  
  
    