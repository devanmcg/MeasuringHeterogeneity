 pacman::p_load(sp, magrittr, ggplot2, viridis)


# Final product

#grid.arrange(arrangeGrob(veg.gg,creature.gg, ncol=1, nrow=2),
#             arrangeGrob(leg.gg, ncol=1, nrow=1), 
#             heights=c(5,1), widths=c(3,3))

source("C:/Users/devan.mcgranahan/Google Drive/R code/ggplot themes/theme_map.r")

# Make the map 


load(file="C:/Users/devan.mcgranahan/Google Drive/GIS data/africa.md.Rdata")
load(file="C:/Users/devan.mcgranahan/Google Drive/GIS data/TNCafrica.Rdata")

# Plot map with ggplot
ggplot() + theme_map() + 
  coord_map(projection = "mercator",
            xlim=c(-28,51.42), ylim=c(-40,20)) + 
  geom_polygon(data=range.biomes, aes(x=long, y=lat, 
                                      group=group, 
                                      fill=Biomes), 
                         color=NA, size=0.25, alpha=0.3) +
  geom_polygon(data=africa.md, aes(x=long, y=lat, group=group), 
               color="grey60", fill=NA, size=0.25) + 
  scale_fill_viridis(discrete = TRUE, direction=-1) +
  theme(legend.position = c(0.25, 0.33), 
        legend.title = element_blank(), 
        legend.text=element_text(size=14), 
        legend.key.height = unit(1.25, "cm")) +
  guides(fill=guide_legend(ncol=1)) +
  geom_point(data=map.dat, aes(x=longitude, y=latitude),
             pch=24, color="black", stroke=1.5, 
             bg="white", size=1.5, show.legend = NA)




# Creatures only:
creature.gg <- africa.gg + ggtitle("Wildlife studies by taxa") +
  theme(plot.title = element_text(hjust=0.5, size=18)) +
    geom_emoji(data=map.dat[map.dat$taxon=="invert",], 
             aes(x=longitude, y=latitude), emoji="1f41c") +
   geom_emoji(data=map.dat[map.dat$taxon=="bird",], 
             aes(x=longitude, y=latitude), emoji="1f426") + 
  geom_emoji(data=map.dat[map.dat$taxon=="mammal",], 
             aes(x=longitude, y=latitude), emoji="1f42d") +
  geom_emoji(data=map.dat[map.dat$taxon=="herp",], 
             aes(x=longitude, y=latitude), emoji="1f98e")




range.biomes <- TNCafrica[!TNCafrica$WWF_MHTNAM %in% 
                            c("Tropical and Subtropical Moist Broadleaf Forests", 
                              "Tropical and Subtropical Dry Broadleaf Forests", 
                              "Inland Water", "Mangroves"), ]
range.biomes <- droplevels(range.biomes)
range.biomes$Biomes <- plyr::revalue(as.factor(range.biomes$WWF_MHTNUM), 
                      c("7"="Tropical & Subtropical Grassland,\n Savanna, Shrubland", 
                        "8"="Temperate Grassland,\nSavanna, Shrubland", 
                        "9"="Flooded Grassland, Savanna",
                        "10"="Montane Grassland, Savanna",
                        "12"='Mediterranean Forest,\nWoodland, Scrub',
                        "13"="Desert, Xeric Shrubland"))
save(range.biomes, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/r objects/range.biomes.Rdata")
  

# Geo data
pacman::p_load(sp, magrittr) 
map.raw <-  with(raw.d,
                 cbind.data.frame(short_key, coordinates, taxon) )
# map.raw$coordinates <- trimws(map.raw$coordinates)
map.dat <- map.raw %>%
            mutate(coordinates = 
                     strsplit(as.character(coordinates), ";")) %>% 
              tidyr::unnest(coordinates) %>%
                tidyr::separate(., coordinates, 
                                 into=c("latitude","longitude"),
                                 sep=", ")
# Convert coordinates
map.dat$latitude <- map.dat$latitude %>%
                      sub('°', 'd', .) %>%
                        sub('º', 'd', .) %>% 
                        trimws() %>%
                        char2dms %>%
                          as.numeric
map.dat$longitude <- map.dat$longitude %>%
                      sub('°', 'd', .) %>%
                        sub('º', 'd', .) %>% 
                          trimws() %>%
                            char2dms %>%
                              as.numeric

# assign emojis

map.dat$emoj <- plyr::revalue(map.dat$taxon,
                              c("bird"="1f426", 
                                "invert"="1f41c",
                                "mammal"="1f42d",
                                "herp"="1f98e"))
                              
# save(map.dat, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/r objects/map.dat.Rdata")
