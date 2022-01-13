SimCellsTemp %>%
  ggplot() + theme_bw(14) + 
    geom_sf(aes(fill = class), 
            color = NA) +
    facet_grid(sim ~ timestep)
