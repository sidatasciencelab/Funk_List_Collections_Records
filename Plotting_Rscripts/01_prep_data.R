library(tidyverse)
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
funk_list_names$NAME <- gsub("√©", "e", funk_list_names$NAME)
funk_list_names$NAME <- gsub("√®", "e", funk_list_names$NAME)

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

# save a copy of this df
write.csv(file = "output/dataframes/funk_list_counts.csv", funk_list_counts, row.names = F)



