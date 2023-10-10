load(file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/r objects/raw.d.Rdata")

pacman::p_load(dplyr)

# Data prep

#
# Experimental approach 
#
EA.raw <- with(raw.d, data.frame(short_key, taxon, 
                                 level,management,
                                 `Study type`,
                                 scale=`Scale studied? (S/T/S+T/N)`))
EA.raw$Taxa <- with(EA.raw, plyr::revalue(taxon, 
                                        c("bird"="Birds", 
                                          "herp"="Herptofauna",
                                          "invert"="Invertebrates", 
                                          "mammal"="Small mammals") ) )

EA.d <- EA.raw %>% 
  tidyr::separate(Study.type, c("study.type","repped"), sep=",") %>%
  tidyr::separate(duration, c("duration","unit"), sep=" ") 

EA.d <- EA.d[complete.cases(EA.d), ]
EA.d$study.type <- with(EA.d, ifelse(study.type=="E",
                                     "Experimental","Observational"))
EA.d$repped <-   factor(EA.d$repped, levels=c("eg","r","pr","nr"))
EA.d$repped <- with(EA.d, plyr::revalue(repped, 
                         c("r"="Replicated", 
                           "pr"="Pseudo-replicated",
                           "eg"="Environmental\ngradient", 
                           "nr"="Not replicated") ) )

save(EA.d, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/GSSA poster/r_objects/EA.d.Rdata")


ggplot(EA.d, aes(x=repped, fill=study.type)) +
  geom_bar(stat = "count") + theme_bw(14) +
  facet_wrap(~Taxa) +
  labs(x=" ",
       y="Number of studies") +
  scale_fill_brewer(palette="Set1",
                    name="Study type", direction=1) +
  theme(legend.position = "top",
        plot.margin = unit(c(0.75,0.25,0.75,0.25), "cm"),
        axis.text.x = element_text(color="black"),
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor.x = element_line(colour=NA))+
  guides(fill=guide_legend(nrow=1)) +
  coord_flip()


# Study duration
duration.dat <- raw.d$duration 
duration.dat <- plyr::revalue(duration.dat, 
                                c("14 YR"="10 YR+",
                                  "10 YR"="10 YR+",
                                  "01 MO"="1-2 MO",
                                  "02 MO"="1-2 MO",
                                  "02 SE"="2-3 SE",
                                  "03 SE"="2-3 SE"))
duration.dat <- factor(duration.dat, 
                         levels=c("01 IN","1-2 MO", 
                                  "01 SE","2-3 SE", 
                                  "01 YR","02 YR","03 YR",
                                  "04 YR", "10 YR+"))
duration.sum <- plyr::count(duration.dat)
colnames(duration.sum)[1] <- "duration"
duration.sum$duration <- plyr::revalue(duration.sum$duration, 
                          c("01 IN"="1 instance",
                            "1-2 MO"="1-2 months", 
                            "01 SE"="1 season",
                            "2-3 SE"="2-3 seasons", 
                            "01 YR"="1 year",
                            "02 YR"='2 years',
                            "03 YR"="3 years",
                            "04 YR"="4 years", 
                            "10 YR+"="10 or more\nyears"))
duration.sum <- duration.sum[complete.cases(duration.sum), ]

# save(duration.sum, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/GSSA poster/r_objects/duration.sum.Rdata")

ggplot(duration.sum, aes(x=duration, y=freq)) + theme_bw(14) +
  geom_bar(stat="identity", fill="#377eb8") +
  labs(x="Study duration",
       y="Number of studies") +
  # scale_fill_brewer(palette="Set1", name="Study type", direction=-1) +
  theme(plot.margin = unit(c(0.75,2,0.75,0.3), "cm"),
        axis.text.x = element_text(color="black", angle=33, hjust=1),
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor.x = element_line(colour=NA))

# Approaches to heterogeneity 
het.raw <- with(raw.d, data.frame(short_key, taxon, het=heterogeneity))
het.raw <- het.raw[complete.cases(het.raw), ]
het.rare <- het.raw %>%
  mutate(het = strsplit(as.character(het), ";")) %>% 
  tidyr::unnest(het)

het.rare$HetPersp <- plyr::revalue(trimws(het.rare$het), 
                                       c("N"="Not considered",
                                         "I"="Implied", 
                                         "M"="Mentioned",
                                         "R"="Recommended", 
                                         "S e"="Studied explicitly",
                                         "S i"='Studied implicitly'))
# save(het.rare, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/GSSA poster/r_objects/het.rare.Rdata")

ggplot(het.rare, aes(x=reorder(HetPersp, HetPersp, 
                               function(x)-length(x) ), 
                     fill=taxon)) + theme_bw(14) +
  geom_bar(stat="count") +
  labs(x="Inclusion of heterogeneity",
       y="Number of studies") +
  scale_fill_brewer(palette="Set1", name="Taxon", direction=-1) +
  theme(legend.position = c(0.8,0.8),
    plot.margin = unit(c(0.75,2,0.75,0.3), "cm"),
        axis.text.x = element_text(color="black", angle=33, hjust=1),
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor.x = element_line(colour=NA))

# Response variable word cloud 
resp.raw <- with(raw.d, data.frame(short_key, 
                                   responses=`response variables`))
resp.rare <- resp.raw %>%
  mutate(responses = strsplit(as.character(responses), ";")) %>% 
  tidyr::unnest(responses)
# Pull out response comments
resp.rare$comment <- trimws(stringr::str_extract(resp.rare$responses,
                                        "\\s*\\([^\\)]+\\)"))

resp.rare$responses <- resp.rare$responses %>%
  gsub("\\s*\\([^\\)]+\\)","",.) %>% 
  trimws() %>%
  tolower()
resp.freq <- plyr::count(resp.rare, vars="responses")
# save(resp.freq, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/GSSA poster/r_objects/resp.freq.Rdata")

pacman::p_load(RColorBrewer)
wordcloud::wordcloud(resp.freq$responses, 
                     resp.freq$freq, scale=c(5,1),
          min.freq=1, max.words=Inf, random.order=FALSE,
          random.color=FALSE, rot.per=0,
          colors=brewer.pal(8,"Dark2"))

# Analyses word cloud
an.raw <- with(raw.d, data.frame(short_key, analyses))
an.raw$analyses <- an.raw$analyses %>% 
  gsub("\\s*\\([^\\)]+\\)","", .) 
an.rare <- an.raw %>%
  mutate(analyses = strsplit(as.character(analyses), ";")) %>% 
  tidyr::unnest(analyses) 

an.rare$analyses <- an.rare$analyses %>%
                      trimws() %>%
                        tolower() 
an.rare <- an.rare[complete.cases(an.rare), ]
an.freq <- plyr::count(an.rare, vars="analyses")

# save(an.freq, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/GSSA poster/r_objects/an.freq.Rdata")

wordcloud::wordcloud(an.freq$analyses, 
                     an.freq$freq, scale=c(5,1),
                     min.freq=1, max.words=Inf, random.order=FALSE,
                     random.color=FALSE, rot.per=0,
                     colors=brewer.pal(8,"Dark2"))

# Strength of community analysis
strength.raw <- with(raw.d, data.frame(short_key,
                                       taxon,
                                       strength=`Strength of community analysis`))
strength.rare <- strength.raw[complete.cases(strength.raw), ]
strength.rare$Taxa <- with(strength.rare, plyr::revalue(taxon, 
                                          c("bird"="Birds", 
                                            "herp"="Herptofauna",
                                            "invert"="Invertebrates", 
                                            "mammal"="Small mammals")))
strength.rare$strength <- tolower(strength.rare$strength) 

# wfm(strength.raw, char2space = "~~")
strength.freq <- plyr::count(strength.rare, vars=c("Taxa", "strength"))
#colnames(strength.freq) <- c("category", "frequency")
strength.freq$strength <- as.factor(strength.freq$strength)
strength.freq$strength <- with(strength.freq, 
                               factor(strength,
                                levels(strength)[c(2, 5,6,1,3,4)]))

# save(strength.freq, file="C:/Users/devan.mcgranahan/Google Drive/Research/Projects/African biodiversity/GSSA poster/r_objects/strength.freq.Rdata")

ggplot(strength.freq, aes(x=strength, y=freq, fill=Taxa)) + theme_bw(16) +
  geom_bar(stat="identity") + 
  labs(x="Strength of community analysis",
       y="Number of studies") +
  scale_fill_brewer(palette="Set1", name="Taxa", direction=1) +
  theme(legend.position = c(0.8,0.8), 
        plot.margin = unit(c(0,0.5,0,0.5), "cm"),
        axis.text.x = element_text(color="black", angle=45, hjust=1),
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor.x = element_line(colour=NA))


head(EA.raw$Study.type)
unique(EA.d$unit)
# Heterogeneity 
het.raw <- with(raw.d, data.frame(short_key, taxon, 
                                  regime, heterogeneity))