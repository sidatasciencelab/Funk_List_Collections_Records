library(tidyverse)
library(fs)
library(ggpubr)
library(stringr)
library(viridis)
library(RColorBrewer)
library(tibble)
library(data.table)

# setwd('') <- # set your working directory here

#### read in funk list ####
funk_list_original <- read.csv("../Funk_List_Names_Unit_Department.csv", header = T)

# correct department of Awl, Aime M. from Zoology to Vertebrate Zoology
funk_list_original$DEPARTMENT[funk_list_original$DEPARTMENT == "Zoology"] <- "Vertebrate Zoology"
# change Division of Fishes to Vertebrate Zoology
funk_list_original$DEPARTMENT[funk_list_original$DEPARTMENT == "Division of Fishes"] <- "Vertebrate Zoology"

# read in regex names
funk_list_names <- read.csv("../Funk_List_Names_RegEx.csv", header = T)

# make a lastname_firstname column
funk_list_names$lastname_firstname <- paste0(funk_list_names$last_name, "_", funk_list_names$first_name)

# make a full_name column and remove the extra space this creates if someone doesn't have a middle name
funk_list_names$full_name <- paste(funk_list_names$first_name, funk_list_names$middle_name, funk_list_names$last_name) %>% gsub("  ", " ", .)

# replace accents with unaccented letters
funk_list_names$lastname_firstname <- gsub("é", "e", funk_list_names$lastname_firstname)
funk_list_names$lastname_firstname <- gsub("è", "e", funk_list_names$lastname_firstname)

####  load Funk list bionomia data #### 
file_list <- list.files('../Specimen_attributions_from_Bionomia', full.names=TRUE)
bionomia_names <- vector(mode='list', length=length(file_list))

for (i in 1:length(file_list)) {
  bionomia_names[[i]] <- read.csv(file_list[i], header=TRUE)
}
names(bionomia_names) <- basename(fs::path_ext_remove(file_list))
  

bionomia_df <- distinct(rbindlist(bionomia_names, use.names=T, fill=T, idcol=T)) 
names(bionomia_df)[names(bionomia_df) == '.id'] <- 'name'

write.csv(file = "output/dataframes/bionomia_df.csv", bionomia_df, row.names = F)

#### count records per person ####
funk_list_counts <- bionomia_df %>% dplyr::group_by(name) %>% dplyr::summarize(count = dplyr::n())

#### merge funk list counts with funk list original ####
names <- merge(x = funk_list_counts, by.x = "name", y = funk_list_names, by.y = "lastname_firstname",
                          all = TRUE)

funk_list <- merge(x = names, y = funk_list_original,
                              all = TRUE)

# correct "SI Archives " to "SI Archives" and change two others for viz
funk_list$SI.UNIT <- gsub("SI Archives ", "SI Archives", funk_list$SI.UNIT)
funk_list$SI.UNIT <- gsub("SI Archives", "Archives", funk_list$SI.UNIT)
funk_list$SI.UNIT <- gsub("SI", "Castle", funk_list$SI.UNIT)

# correct "BAE" to "Anthropology" for viz and remove dept from others
funk_list$DEPARTMENT <- gsub("Arts and Industries ", "Arts and Industries", funk_list$DEPARTMENT)
funk_list$DEPARTMENT <- gsub("BAE", "Anthropology", funk_list$DEPARTMENT)
funk_list$DEPARTMENT <- gsub("Department of ", "", funk_list$DEPARTMENT)
funk_list$DEPARTMENT <- gsub("Smithsonian ", "", funk_list$DEPARTMENT)
funk_list$DEPARTMENT <- gsub("US National Marine Fisheries Service, Systematics Laboratory", "US National Marine Fisheries Service", funk_list$DEPARTMENT)



# add more readable names to the new combined df
# split Name into two columns and merge the two columns with the names the other way round
splits <- str_split_fixed(funk_list$name, "_", 2)
funk_list$name_readable <- paste(splits[,2], splits[,1], sep = ' ')

# remove those with no counts
funk_list_counts <- funk_list %>% drop_na(count) 

# count collections per SI unit and sort for plotting purposes
SI.UNIT_counts <- funk_list_counts %>% dplyr::group_by(SI.UNIT) %>% dplyr::summarize(sum_count = sum(count, na.rm = T), ) %>% arrange((sum_count))
unit_sorted <- SI.UNIT_counts$SI.UNIT

# make units a factor
funk_list_counts$SI.UNIT <- factor(funk_list_counts$SI.UNIT, levels = unit_sorted)

# count collections per SI unit and sort for plotting purposes
SI.UNIT_counts <- funk_list_counts %>% dplyr::group_by(SI.UNIT) %>% dplyr::summarize(sum_count = sum(count, na.rm = T), ) %>% arrange((sum_count)) 
unit_sorted <- SI.UNIT_counts$SI.UNIT

# make units a factor 
funk_list_counts$SI.UNIT <- factor(funk_list_counts$SI.UNIT, levels = unit_sorted) 


##### plot counts #####  
unit_colors <- rev(brewer.pal(n = length(unit_sorted[unit_sorted %in% funk_list_counts$SI.UNIT]), name = 'Spectral'))
# reorder the colors
first <- unit_colors[1]
unit_colors <- unit_colors[!unit_colors %in% first]
unit_colors <- c(unit_colors, first)
unit_colors <- c(unit_colors, unit_colors)

  
count_plot <-   ggdotchart(funk_list_counts, x = "full_name", y = "count",
                                        # color = "SI.UNIT",                             # Color by SI unit?
                                        sorting = "desc",                            # Sort value in descending order
                                        add = "segments",                                # Add segments from y = 0 to dots
                                        rotate = TRUE,                                   # Rotate vertically
                                        group = "SI.UNIT",                             # Order by groups
                                        dot.size = 2.5,                                    
                                        label = round(funk_list_counts$count),           # Add counts values as dot labels
                                        font.label = list(color = "white", size = 2,
                                                          vjust = 0.5),                # Adjust label parameters
                                        ggtheme = theme_pubr()) +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x=element_text(size=8,face="bold"),
        axis.title.y=element_text(size=8,face="bold"),
        axis.ticks = element_line(size = 0.2),
        axis.line = element_line(size = 0.2),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.position = c(0.8, 0.7),
        legend.title.align = 0.5) +
  labs(y = "Number of Specimens Attributed", x = "Funk List Individual",
       color = "SI Unit") #+
count_plot


###### create rectangle bounds ######
# the units in order from least to most collections, so the order of boxes from bottom to top
unit_sorted

xmin <- vector(mode = "numeric", length = length(unit_sorted))
xmax <- vector(mode = "numeric", length = length(unit_sorted))
width <- vector(mode = "numeric", length = length(unit_sorted))
color <- vector(mode = "character", length = length(unit_sorted))
x_text <- vector(mode = "numeric", length = length(unit_sorted))

for (i in 1:length(unit_sorted)) {
  width[i] <- length((funk_list_counts %>% filter(SI.UNIT == unit_sorted[i]))$count)
  xmax[i] <- cumsum(width)[i] + 0.5
  xmin[i] <- cumsum(width)[i] - width[i] + 0.5
  color[i] <- unit_colors[match(unit_sorted[i], unit_sorted)]
  x_text[i] <- ((xmax[i]-xmin[i])/2+xmin[i])
}

# make a df of dimensions
unit_rects_xy <- data.frame(SI.UNIT = unit_sorted, xmin = xmin, xmax = xmax, width = width, color = color, x_text = x_text, y_text = rep((max(funk_list_counts$count))*1.2))

add_rects <- function(yx){
  annotate(geom = "rect", 
           ymin = -1000,
           ymax = (max(funk_list_counts$count))*1.25,
           xmin = yx$xmin, 
           xmax = yx$xmax,
           alpha = 0.15,
           fill = yx$color) 
}

add_rect_labels <- function(yx){
  annotate(geom = "text",
           label = yx$SI.UNIT,
           y = yx$y_text, 
           x = yx$x_text, 
           color = "black",
           angle = 0,
           size = 2,
           hjust = 1)  
}

###### plot counts with rectangles and labels ######
count_plot_rects <- count_plot + 
  add_rects(unit_rects_xy) + 
  add_rect_labels(unit_rects_xy) 


pdf(file = 'output/plots/Figure01_A.pdf', height = 5.5, width = 3.25)
count_plot_rects
dev.off()

##### NMNH only plot #####

# add one point per person per department
funk_list_counts_NMNH <- funk_list_counts %>% dplyr::filter(SI.UNIT == "NMNH") 

# count collections per Department in NMNH and sort for plotting purposes
DEPARTMENT_counts_NMNH <- funk_list_counts_NMNH %>% group_by(DEPARTMENT) %>% dplyr::summarize(sum_count = sum(count, na.rm = T), ) %>% arrange((sum_count)) 
dept_sorted_NMNH <- DEPARTMENT_counts_NMNH$DEPARTMENT

# make departments a factor 
funk_list_counts_NMNH$DEPARTMENT <- factor(funk_list_counts_NMNH$DEPARTMENT, levels = dept_sorted_NMNH) 

dept_colors_NMNH <- (brewer.pal(n = length(dept_sorted_NMNH[dept_sorted_NMNH %in% funk_list_counts_NMNH$DEPARTMENT]), name = 'Spectral'))
# dept_colors_NMNH <- c(dept_colors_NMNH, dept_colors_NMNH)

count_plot_NMNH <-   ggdotchart(funk_list_counts_NMNH, x = "full_name", y = "count",
                                             # color = "DEPARTMENT",                             # Color by SI unit?
                                             sorting = "desc",                            # Sort value in descending order
                                             add = "segments",                                # Add segments from y = 0 to dots
                                             rotate = TRUE,                                   # Rotate vertically
                                             group = "DEPARTMENT",                             # Order by groups
                                             dot.size = 2.75,                                    
                                             label = round(funk_list_counts_NMNH$count),           # Add counts values as dot labels
                                             font.label = list(color = "white", size = 2.25,
                                                               vjust = 0.5),                # Adjust label parameters
                                             ggtheme = theme_pubr()) +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x=element_text(size=7,face="bold"),
        axis.title.y=element_text(size=7,face="bold"),
        axis.ticks = element_line(size = 0.2),
        axis.line = element_line(size = 0.2),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.position = c(0.8, 0.7),
        legend.title.align = 0.5) +
  labs(y = "Number of Specimens Attributed", x = "Funk List Individual",
       color = "Department")

count_plot_NMNH

###### create rectangle bounds ######
# the depts in order from least to most collections, so the order of boxes from bottom to top
dept_sorted_NMNH

xmin <- vector(mode = "numeric", length = length(dept_sorted_NMNH))
xmax <- vector(mode = "numeric", length = length(dept_sorted_NMNH))
width <- vector(mode = "numeric", length = length(dept_sorted_NMNH))
color <- vector(mode = "character", length = length(dept_sorted_NMNH))
x_text <- vector(mode = "numeric", length = length(dept_sorted_NMNH))

for (i in 1:length(dept_sorted_NMNH)) {
  width[i] <- length((funk_list_counts_NMNH %>% filter(DEPARTMENT == dept_sorted_NMNH[i]))$count)
  xmax[i] <- cumsum(width)[i] + 0.5
  xmin[i] <- cumsum(width)[i] - width[i] + 0.5
  color[i] <- dept_colors_NMNH[match(dept_sorted_NMNH[i], dept_sorted_NMNH)]
  x_text[i] <- ((xmax[i]-xmin[i])/2+xmin[i])
}

# make a df of dimensions
dept_rects_xy_NMNH <- data.frame(DEPARTMENT = dept_sorted_NMNH, xmin = xmin, xmax = xmax, width = width, color = color, x_text = x_text, y_text = rep((max(funk_list_counts_NMNH$count))*1.2))


add_rects <- function(yx){
  annotate(geom = "rect", 
           ymin = -1000,
           ymax = (max(funk_list_counts_NMNH$count))*1.25,
           xmin = yx$xmin, 
           xmax = yx$xmax,
           alpha = 0.15,
           fill = yx$color) 
}

add_rect_labels <- function(yx){
  annotate(geom = "text",
           label = yx$DEPARTMENT,
           y = yx$y_text, 
           x = yx$x_text, 
           color = "black",
           angle = 0,
           size = 2,
           hjust = 1)  
}

###### plot counts with rectangles and labels ######
count_plot_rects_NMNH <- count_plot_NMNH + 
  add_rects(dept_rects_xy_NMNH) + 
  add_rect_labels(dept_rects_xy_NMNH)



pdf(file = 'output/plots/Figure01_B.pdf', height = 5.5, width = 3.25)
count_plot_rects_NMNH
dev.off()



#### Department in NMNH #####
# substitute "Fish" "Birds" and "MAMM" for VZ
bionomia_df$collectionCode <- gsub("Fish", "VZ", bionomia_df$collectionCode)
bionomia_df$collectionCode <- gsub("Birds", "VZ", bionomia_df$collectionCode)
bionomia_df$collectionCode <- gsub("MAMM", "VZ", bionomia_df$collectionCode)
bionomia_df$collectionCode <- gsub("Invertebrate Zoology", "IZ", bionomia_df$collectionCode)
bionomia_df$collectionCode <- gsub("HERB", "US", bionomia_df$collectionCode)

# subset into NMNH and other institutions
bionomia_df_NMNH <- bionomia_df %>% filter(institutionCode == "US" | institutionCode == "USNM")
bionomia_df_other <- bionomia_df %>% filter(institutionCode != "US") %>% filter(institutionCode != "USNM")

# make a column using NMNH Dept code and "other" for all else
bionomia_df_NMNH$collectionCode_edit <- bionomia_df_NMNH$collectionCode


bionomia_df_other$collectionCode_edit <- rep("Other")

# bind these back together
bionomia_df <- rbind(bionomia_df_NMNH, bionomia_df_other)

# count collectionCode per person
collectionCode_df <- bionomia_df %>% group_by(name, collectionCode_edit) %>% dplyr::summarize(count_collectionCode = n()) %>% distinct()

##### plot collectionCode per person #####
# merge funk list counts with funk list original
names_collectionCode <- merge(x = collectionCode_df, by.x = "name", y = funk_list_names, by.y = "lastname_firstname",
                       all = TRUE)
funk_list_collectionCode <- merge(x = names_collectionCode, y = funk_list_original,
                           all = TRUE)

# correct "SI Archives " to "SI Archives" and change two others for viz
funk_list_collectionCode$SI.UNIT <- gsub("SI Archives ", "SI Archives", funk_list_collectionCode$SI.UNIT)
funk_list_collectionCode$SI.UNIT <- gsub("SI Archives", "Archives", funk_list_collectionCode$SI.UNIT)
funk_list_collectionCode$SI.UNIT <- gsub("SI", "Castle", funk_list_collectionCode$SI.UNIT)

# correct "BAE" to "Anthropology" for viz and remove dept from others
funk_list_collectionCode$DEPARTMENT <- gsub("Arts and Industries ", "Arts and Industries", funk_list_collectionCode$DEPARTMENT)
funk_list_collectionCode$DEPARTMENT <- gsub("BAE", "Anthropology", funk_list_collectionCode$DEPARTMENT)
funk_list_collectionCode$DEPARTMENT <- gsub("Department of ", "", funk_list_collectionCode$DEPARTMENT)
funk_list_collectionCode$DEPARTMENT <- gsub("Smithsonian ", "", funk_list_collectionCode$DEPARTMENT)

# add more readable names to the new combined df
# split Name into two columns and merge the two columns with the names the other way round
splits <- str_split_fixed(funk_list_collectionCode$name, "_", 2)
funk_list_collectionCode$name_readable <- paste(splits[,2], splits[,1], sep = ' ')

# at this point NA in counts_collectionCode means there were no collections found for that person, so can omit them
funk_list_counts_collectionCode <- funk_list_collectionCode %>% drop_na(count_collectionCode)

# replace blanks in collectionCode column with NA then NA with "Unknown"
funk_list_counts_collectionCode$collectionCode_edit <- as.character(funk_list_counts_collectionCode$collectionCode_edit)
funk_list_counts_collectionCode$collectionCode_edit[funk_list_counts_collectionCode$collectionCode_edit==""] <- NA
funk_list_counts_collectionCode$collectionCode_edit[is.na(funk_list_counts_collectionCode$collectionCode_edit)] <- "Unknown"

# count collections per Department in NMNH and sort for plotting purposes
collectionCode_counts <- funk_list_counts_collectionCode %>% dplyr::group_by(collectionCode_edit) %>% dplyr::summarize(sum_count = sum(count_collectionCode, na.rm = T), ) %>% arrange((sum_count)) 
collectionCode_sorted <- collectionCode_counts$collectionCode_edit
# set factor levels to this sort order
funk_list_counts_collectionCode$collectionCode_edit <- factor(funk_list_counts_collectionCode$collectionCode_edit, levels = collectionCode_sorted) # levels 

# sort names by total collectioons
names_collectionCode_counts <- funk_list_counts_collectionCode %>% dplyr::group_by(full_name) %>% dplyr::summarize(count_sum = sum(count_collectionCode)) %>% arrange(count_sum) %>% distinct()
names_collectionCode_sorted <- unique(names_collectionCode_counts$full_name)
# set factor levels to this sort order
funk_list_counts_collectionCode$full_name <- factor(funk_list_counts_collectionCode$full_name, levels = names_collectionCode_sorted)

# set dept colors
ent_color <- "#1c75bd"
iz_color <- "#00a79d"
vz_color <- "#f15a28"
bot_color <- "#00a651"
paleo_color <- "#be1e2c"

# count number of different collectionCodes each person collected to identify broad collectors
collectionCode_depts <- bionomia_df_NMNH %>% group_by(name) %>% dplyr::summarize(depts_count = length(unique(collectionCode_edit))) 

# pull out folks who collected from 5 for asterisks
collectionCode_4 <- collectionCode_depts %>% filter(depts_count > 3)
# add a name_readable column
tmp <- left_join(collectionCode_4, funk_list_names, by = c("name" = "lastname_firstname"))
collectionCode_4$name_readable <- paste(tmp$first_name, tmp$last_name)

funk_list_counts_collectionCode_4 <- funk_list_counts_collectionCode %>% filter(name %in% collectionCode_4$name)
# make df for xy coords
asterisk_4 <- funk_list_counts_collectionCode_4 %>% dplyr::group_by(full_name) %>% dplyr::summarize(sum_count = sum(count_collectionCode)) 

# add asterisks to this plot 
ggplot(funk_list_counts_collectionCode, aes(fill=collectionCode_edit, y=count_collectionCode, x=full_name)) + 
  geom_bar(position="stack", stat="identity", alpha = 0.75) + 
  coord_flip() + 
  # scale_y_log10() + # for use with position = "dodge"
  theme_classic() +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 4),
        axis.title.x=element_text(size=6,face="bold"),
        axis.title.y=element_text(size=6,face="bold"),
        axis.ticks = element_line(size = 0.2),
        axis.line = element_line(size = 0.2),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.position = c(0.8,0.35),
        legend.title.align = 0.5) +
  labs(x = "Funk List Individual", y = "Number of Specimens Attributed", fill = "NMNH Department") +
  scale_fill_manual(values = c(IZ = iz_color, US = bot_color,  Paleobiology = paleo_color, VZ = vz_color, ENT = ent_color, Other = "gray28"),
                    labels = c("ENT" = "Entomology",
                               "Paleobiology" = "Paleobiology",
                               "IZ" = "Invertebrate Zoology",
                               "VZ" = "Vertebrate Zoology",
                               "US" = "Botany", 
                               "Other" = "Other Institutions")) + 
  annotate(geom = "text",
           label = "*",
           y = asterisk_4$sum_count, 
           x = asterisk_4$full_name, 
           color = "black",
           angle = 0,
           size = 2,
           hjust = -0.1,
           vjust = 0.75)    -> collectionCode_4_bars

collectionCode_4_bars 

# function to make legend smaller
addSmallLegend <- function(myPlot, pointSize = 2.5, textSize = 5, spaceLegend = 1) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize, face = "bold"), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}


pdf(file = 'output/plots/Figure02_test.pdf', height = 3, width = 3.25)
addSmallLegend(collectionCode_4_bars)
dev.off()




#### Botany families ####
#### change from recordedBy to combined after update #####
# filter df for botany only
bionomia_df_botany <- bionomia_df %>% filter(collectionCode == "US") 
bionomia_df_botany$dataset <- rep("Extant")

##### get all the unique family #####
all_family <- unique(bionomia_df_botany$family) 
# count family per person
family_df <- bionomia_df_botany %>% dplyr::group_by(name, family) %>% dplyr::summarize(count_family = n()) %>% arrange(desc(count_family))

#### merge funk list counts with funk list original ####  
names_family <- merge(x = family_df, by.x = "name", y = funk_list_names, by.y = "lastname_firstname",
                              all = TRUE)
funk_list_family <- merge(x = names_family, y = funk_list_original,
                                  all = TRUE)

# add more readable names to the new combined df
# split Name into two columns and merge the two columns with the names the other way round
splits <- str_split_fixed(funk_list_family$name, "_", 2)
funk_list_family$name_readable <- paste(splits[,2], splits[,1], sep = ' ')

# at this point NA in counts_family means there were no collections found for that person, so can omit them
funk_list_counts_family <- funk_list_family %>% drop_na(count_family)

# replace blanks in family column with NA then NA with "Unknown"
funk_list_counts_family$family <- as.character(funk_list_counts_family$family)
funk_list_counts_family$family[funk_list_counts_family$family==""] <- NA
funk_list_counts_family$family[is.na(funk_list_counts_family$family)] <- "Unknown"

# # # count collections per Department in NMNH and sort for plotting purposes
# # family_counts <- funk_list_counts_family %>% group_by(family) %>% dplyr::summarize(sum_count = sum(count_family, na.rm = T), ) %>% arrange(desc(sum_count)) 
# # family_sorted <- family_counts$family
# # set factor levels to this sort order
# funk_list_counts_family$family <- factor(funk_list_counts_family$family, levels = family_sorted) # levels 

# # sort names by total number of families
# names_family_counts <- funk_list_counts_family %>% group_by(name_readable) %>% summarize(count_sum = sum(count_family)) %>% arrange(count_sum)
# names_family_sorted <- names_family_counts$name_readable
# # set factor levels to this sort order
# funk_list_counts_family$name_readable <- factor(funk_list_counts_family$name_readable, levels = names_family_sorted)

# count number of different familys each person collected to identify broad collectors
family_botany <- funk_list_counts_family %>% ungroup() %>% group_by(full_name) %>% dplyr::summarize(families_count = length(unique(family))) %>% arrange((families_count)) %>% unique()
# choose levels by combined families
family_botany_levels <- (family_botany %>% dplyr::group_by(full_name) %>% dplyr::summarize(sum_family = sum(families_count)) %>% arrange(sum_family))$full_name

# set factor levels to this sort order
family_botany$full_name <- factor(family_botany$full_name, levels = family_botany_levels)


# plot
ggdotchart(family_botany, x = "full_name", y = "families_count",
                sorting = "desc",                            # Sort value in descending order
                add = "segments",                                # Add segments from y = 0 to dots
                rotate = TRUE,                                   # Rotate vertically
                dot.size = 2,                                    
                label = round(family_botany$families_count),           # Add counts values as dot labels
                font.label = list(color = "white", size = 3.2,
                                  vjust = 0.5),                # Adjust label parameters
                ggtheme = theme_pubr()) +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 6),
        axis.title.x=element_text(size=6,face="bold"),
        axis.title.y=element_text(size=7,face="bold"),
        axis.ticks = element_line(size = 0.2),
        axis.line = element_line(size = 0.2),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.position = c(0.8,0.5),
        legend.title.align = 0.5) +
  labs(x = "Funk List Individual", y = "Number of Families", fill = "Department") -> family_bars
  
  
  

pdf(file = 'output/plots/Figure03_B.pdf', height = 4, width = 2.2)
family_bars
dev.off()















