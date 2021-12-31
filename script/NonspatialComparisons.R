# load('./data/sim_data.Rdata') # object created in CreateDataset.R 

sim_sums <- 
  sim_data %>%
  group_by(crop) %>%
  summarize(count = n(),
            CropMean = round(mean(PerProd, na.rm = T), 0), 
            CropSD = round(sd(PerProd, na.rm = T), 1),
            .groups = 'drop')

# Set simulation parameters 
  reps = 1:100
  N = 25
# create empty tibble
  sim_crop <- tibble() 
# Simulate group-level data with group-level SD
  for(r in 1:length(reps)) {
    sim_sums %>%
      group_by(crop) %>%
      group_modify(~ {
        rnorm(n=N, mean = .x$CropMean, sd = .x$CropSD) %>%
          tibble::enframe('sim') 
      }) %>%
      ungroup() %>%
      mutate(rep = reps[r], 
             SD = 'Unique', 
             Mean = 'Unique') %>%
      bind_rows(sim_crop) -> sim_crop
  } 
# Simulate group-level data with common SD
  SDs = tibble(Lowest = min(sim_sums$CropSD), 
               Highest = max(sim_sums$CropSD)) %>%
    pivot_longer(cols = c(Lowest, Highest), 
                 names_to ='level', 
                 values_to = 'sd')
  for(i in 1:length(unique(SDs$level))) {
  for(r in 1:length(reps)) {
    sim_sums %>%
      group_by(crop) %>%
      group_modify(~ {
        rnorm(n=N, mean = .x$CropMean, sd = slice(SDs, i)$sd) %>%
          tibble::enframe('sim') 
      }) %>%
      ungroup() %>%
      mutate(rep = reps[r], 
             SD = slice(SDs, i)$level, 
             Mean = 'Unique') %>%
      bind_rows(sim_crop) -> sim_crop
    } 
  }
# Simulate group-level data with common means
  Mus = tibble(Lowest = min(sim_sums$CropMean), 
               Highest = max(sim_sums$CropMean)) %>%
    pivot_longer(cols = c(Lowest, Highest), 
                 names_to ='level', 
                 values_to = 'mean')
  for(i in 1:length(unique(Mus$level))) {
  for(r in 1:length(reps)) {
    sim_sums %>%
      group_by(crop) %>%
      group_modify(~ {
        rnorm(n=N, mean = slice(Mus, i)$mean, sd = .x$CropSD) %>%
          tibble::enframe('sim') 
      }) %>%
      ungroup() %>%
      mutate(rep = reps[r], 
             SD = "Unique",
             Mean = slice(Mus, i)$level) %>%
      bind_rows(sim_crop) -> sim_crop
    } 
  }
# save(sim_crop, file = './data/sim_crop.Rdata')