pacman::p_load(googlesheets, dplyr)

# Connect and import from Google Sheets
  abd.url <- "https://docs.google.com/spreadsheets/d/1p4B_Ni060a7Y2l_xsG9A-8sWfq8wIMGMpazSxlU8CLA/edit?usp=sharing"
  abd_gs <- gs_url(abd.url)
  raw.d <- as.data.frame(abd_gs %>% gs_read_csv(ws="cleaned up for poster"))
  
  save(raw.d, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/r objects/raw.d.Rdata")



  
