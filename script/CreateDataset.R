pacman::p_load(tidyverse, sf)
# define an Albers Equal Area CRS
  AEA = '+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
#
# Get fire perimeters on US National Grasslands
#
  # US National Grasslands 
    load('../CommonFiles/NationalGrasslands.Rdata') # https://data.fs.usda.gov/geodata
    ng <- NationalGrasslands %>%
            st_transform(AEA)
  # USFS fire perimeters 
    load('../CommonFiles/FirePerimetersUSFS.Rdata') # https://data.fs.usda.gov/geodata
    ng_fires <- 
      FirePerimetersUSFS %>%
      st_transform(AEA) %>%
      filter(between(FIREYEAR, 1985, 2010)) %>%
      mutate(valid = st_is_valid(.)) %>%
      filter(valid != FALSE) %>%
      select(-valid) %>%
      st_intersection(st_buffer(ng, 0) %>% select(GRASSLANDN)) %>%
      filter(TOTALACRES >= 247.12) %>% # > 100 ha
      select(GRASSLANDN, FIRENAME, DISCOVERYD, STATCAUSE)  %>%
      rename(unit = 'GRASSLANDN', 
             fire = 'FIRENAME', 
             date = 'DISCOVERYD', 
             cause = 'STATCAUSE') %>%
      st_transform(4326) %>%
      unite('key', c(unit, fire), sep ='|') %>%
      mutate(season = ifelse(as.numeric(format(date, '%m')) <= 5, 'early', 'late'), 
             start = case_when(
               season == "early" ~ format(date, '%Y'),
               season == 'late' ~ format(date + lubridate::years(1), '%Y') ),
             start = as.numeric(start), 
             end = start + 10 )
#
# Fetch RAP data
#
 {
  begin = Sys.time()
  # Get data for 1 yr before and 9 yrs after each fire
    range = -1:9 
  # setup for parallel processing
    if("cl" %in% ls()) rm(cl)
    if(! "cores" %in% ls()) cores = parallel::detectCores()
    cl <- makeSOCKcluster(cores) 
    registerDoSNOW(cl)
    clusterCall(cl, function(x) .libPaths(x), .libPaths())
  #
  # M A I N   L O O P
  #
    PolyVeg <- 
      foreach(y = 1:length(unique(ng_fires$start)), 
              .combine=bind_rows, 
              .inorder = TRUE,
              .errorhandling = 'remove') %:%
      foreach(i = 1:length(range), 
              .combine=bind_rows, 
              .inorder = TRUE,
              .errorhandling = 'remove', 
              .packages=c('tidyverse', 'sf')) %dopar% {
      # Subset feature of interest (aoi)
        s = unique(ng_fires$start)[y] 
        aoi <- ng_fires %>% 
                filter(start == s) 
      # build specific URL for RAP data type + year 
        year =  s + range[i]
        index_url = 'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-biomass/v2/vegetation-biomass-v2-'
        rap_url = paste0('/vsicurl/', index_url, year, '.tif')
      # Extract RAP data from URL by aoi
        terra::extract(terra::rast(rap_url), 
                       terra::vect(aoi), 
                       fun = 'mean', 
                       list = FALSE,
                       na.rm = TRUE, 
                       df = TRUE) %>%
          as_tibble() %>%
          rename( AnnProd = veg_1, 
                  PerProd = veg_2) %>%
          mutate(key = aoi$key, 
                 year = range[i] + 1) %>%
          select(key, year, AnnProd, PerProd)
            }
  stopCluster(cl)
  Sys.time() - begin 
    }
# Wrangle perennial productivity data 
  prod_d <-
    PolyVeg %>%
    filter(between(PerProd,
                   quantile(PerProd, 0.0275),
                   quantile(PerProd, 0.975) ) ) %>%
    arrange(PerProd) %>%
    slice(3:747) 
#
# Create novel dataset
#
  # Find moments for novel dataset
    MASS::fitdistr(prod_d$PerProd, 'normal')
  # Create simulated dataset from raw data moments
  # assign to quantile classes
    set.seed(5678)
    sim_data <-
      tibble(PerProd = rnorm(1000, 866.9, 257.1)) %>%
      filter(between(PerProd,
                     quantile(PerProd, 0.0275),
                     quantile(PerProd, 0.975) ) ) %>%
      mutate( PerProd = round(PerProd, 0), 
              crop = case_when(
                between(PerProd,
                        quantile(PerProd, 0.0),
                        quantile(PerProd, 0.19999)) ~ "VL",
                between(PerProd,
                        quantile(PerProd, 0.20),
                        quantile(PerProd, 0.39999)) ~ "L",
                between(PerProd,
                        quantile(PerProd, 0.40),
                        quantile(PerProd, 0.59999)) ~ "M",
                between(PerProd,
                        quantile(PerProd, 0.60),
                        quantile(PerProd, 0.79999)) ~ "H",
                between(PerProd,
                        quantile(PerProd, 0.8),
                        quantile(PerProd, 1)) ~ "VH")) %>%
      mutate(crop = factor(crop, levels = c("VL", "L", "M","H", 'VH'))) 
  # save(sim_data, file = './data/sim_data.Rdata')  