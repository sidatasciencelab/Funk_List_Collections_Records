library(dplyr)
# library(magrittr)
library(tidyverse)
library(rotl)
library(ggtree)
library(ggtreeExtra)
# library(phytools)

setwd('') # set your wd here

# read in family list
# df <- read.csv(file = "funk_list_deceased.csv", header = T, check.names = F)
# subset <- df %>% select(family, kingdom)

# funk_list_names <- read.csv("assets/funk_list_names.csv", header = T)
# # make a lastname_firstname column
# funk_list_names$lastname_firstname <- paste0(funk_list_names$last_name, "_", funk_list_names$first_name)

bionomia_df_all <- read.csv("bionomia_df.csv", header = T, check.names = F)

##### get all the unique family #####
# count family per person
family_df <- bionomia_df_all %>% dplyr::group_by(family, kingdom) %>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() 
# # remove "idae" non-family names
# family_df <- dplyr::filter(family_df, !grepl("idae",family))

# set factor levels to this sort order
# family$name_readable <- factor(family$name_readable, levels = family_levels)

resolved_names <- rotl::tnrs_match_names(unique(family_df$family))
# two didn't have a match
resolved_names <- na.omit(resolved_names) 

in_tree <- rotl::is_in_tree(rotl::ott_id(resolved_names))
family_tree <- tol_induced_subtree(ott_id(resolved_names)[in_tree])

# how many families total?
length(unique(family_df$family))
# [1] 1482

# how many in tree?
length(resolved_names$search_string[in_tree])
# [1] 1132

# join count data to rotl df since tip names are ott_id
full_df <- dplyr::left_join(resolved_names, family_df, c("unique_name" = "family")) %>% na.omit()

# # prepare data for labeling kingdoms
# df_in_tree <- full_df %>% subset(., unique_name %in% resolved_names$unique_name[in_tree])
# 
# Animalia <- df_in_tree %>% filter(kingdom == "Animalia")
# Plantae <- df_in_tree %>% filter(kingdom == "Plantae")
# Fungi <- df_in_tree %>% filter(kingdom == "Fungi")
# Chromista <- df_in_tree %>% filter(kingdom == "Chromista")
# Bacteria <- df_in_tree %>% filter(kingdom == "Bacteria")
# 
# Animalia_mrca <- ape::getMRCA(family_tree, tip = Animalia$unique_name) # error, come back later


# fix tip labels for plotting data
# taxon_map <- structure(full_df$search_string, names = full_df$unique_name)
otl_tips <- strip_ott_ids(family_tree$tip.label, remove_underscores = TRUE)
taxon_map <- structure(full_df$unique_name, names = full_df$unique_name) # can use this map to change the names further if need to
family_tree$tip.label <- taxon_map[ otl_tips ]


ggtree(family_tree, layout = "fan", open.angle = 1, size = 0.15) +
  ggtreeExtra::geom_fruit(data = full_df, geom = geom_bar,
             mapping=aes(y = unique_name, x = log10(count+1), fill = kingdom), # add 1 to count before log so that 0 still shows as a bar
             pwidth=0.38, 
             orientation="y", 
             stat="identity",
  ) + 
  scale_fill_manual(name = "Kingdom", 
                    values = c(Animalia = "#FB4D3D", Fungi = "#345995", Plantae = "#03CEA4", Bacteria = "#E5D4ED", Chromista = "#EAC435")) +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> family_tree_plot 

family_tree_plot 


pdf(file = "family_tree.pdf", height = 4, width = 4.3)
family_tree_plot
dev.off()
