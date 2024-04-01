rm(list=ls())

## Libraries
library(tidyverse)
library(reshape2)
library(devtools)
library(MASS)
library(dplyr)
# Plotting
library(ggplot2)
library(RColorBrewer)
library(ggbiplot)
library(plot3D)
library(plotly)
library(ggsankey)

setwd("/Users/ful083/Work/papers_being_written/Lenfest_EBFM/Hub_index/")

dfmap <- read.table("Hub_Mapping.csv", header=TRUE, sep = ",")
dfmap <- read.table("Hub_Mapping_v2.csv", header=TRUE, sep = ",")
dflong <- dfmap%>% make_long(FullModel, Simplif_1, Simplif_2, Simplest)

dflong$node <- factor(dflong$node, levels =c("Transient Killers","Killers Whales","Resident Killers","Toothed whales","Sperm and Beaked Whales","Porpoises","Belugas","Gray Whales","Humpbacks","Fin Whales","Sei whales","Right Whales","Minke Whales","Bowhead Whales","Baleen whales","Sea Otters","Walrus Bd Seals","Steller Sea Lion juv","Steller Sea Lion adults","Steller Sea Lion","N. Fur Seal juv","N. Fur Seal adults","N. Fur Seal","Resident seals","Wintering seals","Seals","Shearwater","Murres","Kittiwakes","Auklets","Albatross Jaeger","Mobile Seabirds","Seabirds","Pisc. birds","Puffins","Fulmars","Storm Petrels","Cormorants","Gulls","Inshore Seabirds","Sleeper shark","W. Pollock juv","W. Pollock adults","W. Pollock ","Arrowtooth juv","Arrowtooth adults","Arrowtooth","Kamchatka fl. juv","Kamchatka fl. adult","Kamchatka fl. ","Gr. Turbot juv","Gr. Turbot adults","Gr. Turbot ","YF. Sole juv","YF. Sole adults","YF. Sole ","FH. Sole juv","FH. Sole adults","FH. Sole ","N. Rock sole juv","N. Rock sole adults","AK Plaice","Dover Sole","Rex Sole","Misc. Flatfish","Flatfish","Small Flatfish","P. Halibut juv","P. Halibut adults","P. Halibut ","Large Flatfish","Alaska skate","Other skates","Skates","P. cod juv","P. cod adults","P. cod ","Sablefish juv","Sablefish adults","Sablefish ","Eelpouts","Sharpchin Rock","Northern Rock","Dusty Rock","Demersal fish","Shortraker Rock","Rougheye Rock","Rockfish","POP","Shortspine Thorns","Other Sebastes","Atka mackerel juv","Atka mackerel adult","Atka mackerel ","Greenlings","Lg. Sculpins","Other sculpins","Sculpins","Misc. fish shallow","O.Dem.Fish","Grenadiers","Misc. fish deep","Deepwater fish","Capelin","Sandlance","Eulachon","Forage fish","Oth. managed forage","Oth. pelagic smelt","Herring juv","Herring adults","Herring ","Shallow Pelagics","Salmon returning","Salmon outgoing","Salmon","Bathylagidae","Myctophid","Mesopelagics","Deep Pelagics","Octopi","Squids","Cephalopods","Bairdi","King Crab","Opilio","Pandalidae","NP shrimp","Hermit crabs","Misc. crabs","Misc. Crustacean","Crustaceans","Sea stars","Brittle stars","Urchins dollars cucumbers","Snails","Benthic Amphipods","Epifauna","Anemones","Corals","Hydroids","Coral-like","Urochordata","Sea Pens","Sponges","Bivalves","Sea Pens & Sponges","Benth.P.Feeders","Polychaetes","Misc. worms","Worms","Infauna","Scyphozoid","Gelatinous filter feeders","Jellyfish","Fish Larve","Chaetognaths","Euphasids","Mysids","Pelagic Amphipods","Large Zooplankton","Pteropods","Copepods","HerbivZooplankton","Pelagic microbes","Benthic microbes","Microbes","Macroalgae","Large Phytoplankton","Sm Phytoplankton","Phytoplankton","Outside Production","Discards","Offal","Pelagic Detritus","Benthic Detritus","Detritus","Outside Detritus"))
dflong$next_node <- factor(dflong$next_node, levels =c("Killers Whales","Toothed whales","Porpoises","Gray Whales","Baleen whales","Sea Otters","Walrus Bd Seals","Steller Sea Lion","N. Fur Seal","Resident seals","Wintering seals","Seals","Mobile Seabirds","Inshore Seabirds","Seabirds","Pisc. birds","Sleeper shark","W. Pollock ","Arrowtooth ","Kamchatka fl. ","Gr. Turbot ","YF. Sole ","FH. Sole ","N. Rock sole ","P. Halibut ","Small Flatfish","Large Flatfish","Flatfish","Skates","P. cod ","Sablefish ","Eelpouts","Rockfish","Shortspine Thorns","Other Sebastes","Demersal fish","Atka mackerel ","Greenlings","Sculpins","Misc. fish shallow","O.Dem.Fish","Herring ","Forage fish","Shallow Pelagics","Salmon","Bathylagidae","Mesopelagics","Deep Pelagics","Octopi","Squids","Cephalopods","Bairdi","King Crab","Opilio","NP shrimp","Hermit crabs","Misc. crabs","Misc. Crustacean","Crustaceans","Sea stars","Brittle stars","Urchins dollars cucumbers","Snails","Benthic Amphipods","Epifauna","Coral-like","Urochordata","Sea Pens & Sponges","Bivalves","Benth.P.Feeders","Polychaetes","Misc. worms","Worms","Infauna","Scyphozoid","Gelatinous filter feeders","Jellyfish","Fish Larve","Euphasids","Mysids","Pelagic Amphipods","Large Zooplankton","Pteropods","Copepods","HerbivZooplankton","Pelagic microbes","Benthic microbes","Microbes","Macroalgae","Large Phytoplankton","Sm Phytoplankton","Phytoplankton","Outside Production","Discards","Offal","Detritus","Outside Detritus"))

# Plot the sankey mapping from one EBS model to next
png("sankey_EBS.png", width = 2400, height = 3200)
ggplot(dflong, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +           
  geom_sankey(flow.alpha = 0.5, node.color = "black", show.legend = FALSE) +
  geom_sankey_label(size = 10, color = "black", fill = "white") +
  scale_fill_viridis_d(option = "A", alpha = 0.95) +
  theme_sankey(base_size = 46) +
  theme(axis.title = element_blank())
dev.off()

