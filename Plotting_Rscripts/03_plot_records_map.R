  library(ggplot2)
  library(gdata)
  library(googleLanguageR)
  library(dplyr)
  library(maps)
  library(countrycode)
  
  setwd('') # set your wd here
  google_auth_json <- "../../AWHI-collections-data/data/countries/strategic-arc-361820-458f7ece628b.json" # path to your google api json here
  
  # load data
  # create function to read tsv with special characters
  read.funky.table <- function(file, ...) {
    tab <- readLines(file)
    tab <- do.call(rbind, strsplit(tab, "\t"))
    colnames(tab) <- tab[1,]
    tab <- tab[-1,]
    tab <- as.data.frame(tab, ...)
    return(tab)    
  }
  
  # apply function to load data (ignore warning about end of line)
  funk <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/funk_countries.tsv")
  mexia <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/mexia_countries_edited.tsv")
  chase <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/chase_countries_edited.tsv")
  rudd <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/rudd_countries_edited.tsv")
  
  # 
  # robinson <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/robinson_countries2.tsv")
  # sfblake <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/sfblake_countries_edited.tsv")
  
  # add columns 
  # combine 
  countries_data <- gdata::combine(funk, mexia, chase, rudd)
  
  # add a column for continent from country
  countries_data$region <- countrycode::countrycode(sourcevar = countries_data[, "country"],
                                                    origin = "country.name",
                                                    destination = "region")
  
  # some don't have a region, apparently because they are in a different language. remove those, then translate them, try again and put them back
  region_NA <- countries_data %>% dplyr::filter(is.na(region))
  countries_data_na.omit <- na.omit(countries_data)
  countries_data_na.omit$country_english <- countries_data_na.omit$country
  
  googleLanguageR::gl_auth(google_auth_json)
  region_NA_translated <- gl_translate(region_NA$country, target = "en", format = "text", source = "fr")
  region_NA$country_english <- region_NA_translated$translatedText
  
  # bind this back to countries_data 
  countries_data_clean <- rbind(countries_data_na.omit, region_NA)
  
  # manually clean and remove some unclear locations
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Capital")
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Indeterminate")
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Unknown")
  countries_data_clean$country_english <- gsub("green cap", "Cape Verde", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Somalis", "Somalia", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Uruguai", "Uruguay", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Southern Africa", "South Africa", countries_data_clean$country_english)
  
  
  # try getting region again
  countries_data_clean$region <- countrycode::countrycode(sourcevar = countries_data_clean[, "country_english"],
                                                          origin = "country.name",
                                                          destination = "region")
  
  
  # add some custom match values for remaining NA matches
  # options (including two custom):
  # "Latin America & Caribbean"  "Europe & Central Asia"      "Sub-Saharan Africa"         "East Asia & Pacific"       
  # "Middle East & North Africa" "North America"              "South Asia"                 "Antarctica"    
  
  # for now splitting "Macaronesia" beween europe (azores and madeira) and sub saharan africa (cape verde)
  
  # unk_region <- countries_data_clean %>% filter(is.na(region)) # run these once just to get the regions
  # v <- unique(unk_region$country_english)
  
  
  # this function requires a named vector for custom matches. 
  # the vector is the region result and the names of the vector elements are the country names
  custom <- c("Latin America & Caribbean",                  "Latin America & Caribbean",       
              "East Asia & Pacific",                        "Latin America & Caribbean",                                      
              "Sub-Saharan Africa",                         "Latin America & Caribbean",                               
              "Europe & Central Asia",                      "Latin America & Caribbean",                                   
              "Latin America & Caribbean",                  "Europe & Central Asia",                           
              "Latin America & Caribbean",                   "Latin America & Caribbean",                             
              "Antarctica",                                 "Sub-Saharan Africa",
              "Sub-Saharan Africa",                         "East Asia & Pacific",
              "Europe & Central Asia",                      "Europe & Central Asia",
              "Latin America & Caribbean",                   "East Asia & Pacific",  
              "Latin America & Caribbean")  
  
  names(custom) <- c("Caribbean",                                  "Virgin Islands of the United States",       
                     "Micronesia",                                 "Saba",                                      
                     "Western Sahara",                             "west indies",                               
                     "Europe & Central Asia",                      "Bonaire",                                   
                     "Virgin Islands",                            "Madeira Islands",                           
                     "Porto Rico",                                 "Santo Domingo",                             
                     "antarctica",                                 "Cape Verde",
                     "Democratic Republic of the Congo or Angola", "British Indian Ocean Territory",
                     "French",                                     "Azores",
                     "Freuch Guiana",                              "U.S. Administered Islands (Micronesia)",
                     "Granada")  
  
  # try getting region again, this time with custom matches
  countries_data_clean$region <- countrycode::countrycode(sourcevar = countries_data_clean[, "country_english"],
                                                          origin = "country.name",
                                                          destination = "region",
                                                          custom_match = custom)
  
  # all good for now! 
  
  
  # summarize counts per region
  countries_data_clean$count <- as.numeric(as.character(countries_data_clean$count))
  countries_data_clean$action <- as.factor(countries_data_clean$action)
  countries_data_clean$region <- as.factor(countries_data_clean$region)
  countries_data_summary <- countries_data_clean %>% group_by(action, source, region, .drop = FALSE) %>% dplyr::summarize(sum_count = sum(count)) %>% ungroup()
  countries_data_summary$action <- as.factor(countries_data_summary$action)
  countries_data_summary$action <- factor(countries_data_summary$action, levels = c("recorded", "identified"))
  
  # set factor/plotting order to birth year order
  # chase 1869 
  # mexia 1870
  # rudd 1910
  # funk 1947
  countries_data_summary$source <- factor(countries_data_summary$source, levels = c("chase", "mexia", "rudd", "funk"))
  
  
  
  # add lat long for each region (manually based on where i want the plot on the map)
  countries_data_summary <- countries_data_summary %>% mutate(lat = dplyr::case_when(region == "Latin America & Caribbean" ~ -11.7832, 
                                                                                     region == "Europe & Central Asia" ~ 54.4507,
                                                                                     region == "Sub-Saharan Africa" ~ 4.284266,
                                                                                     region == "East Asia & Pacific" ~ 58.7946, 
                                                                                     region == "Middle East & North Africa" ~ 19.7917, 
                                                                                     region == "North America" ~ 45.5260,
                                                                                     region == "South Asia" ~ 31.0376,
                                                                                     region == "Antarctica" ~ -77.8628))
  countries_data_summary <- countries_data_summary %>% mutate(long = dplyr::case_when(region == "Latin America & Caribbean" ~ -57.9915, 
                                                                                      region == "Europe & Central Asia" ~ 48.8319,
                                                                                      region == "Sub-Saharan Africa" ~ 26.838434,
                                                                                      region == "East Asia & Pacific" ~ 117.5348, 
                                                                                      region == "Middle East & North Africa" ~ 7.5926, 
                                                                                      region == "North America" ~ -102.2551,
                                                                                      region == "South Asia" ~ 95.4563,
                                                                                      region == "Antarctica" ~ 100.0000))
  
  
  
  #### map ####
  # set custom colors for bar plots 
  
  chase.recorded <- "#0E8C3A" # green
  chase.identified <- "#20E966"
  
  mexia.recorded <- "#00688B" # blue
  mexia.identified <- "#00BEFE"
  
  rudd.recorded <- "#CD1076" # pink
  rudd.identified <- "#F796CA"
  
  funk.recorded <- "#b34700" # orange 
  funk.identified <- "#ff6500"
  
  ##### create a bar plot for each of the 9 regions #####
  # "Latin America & Caribbean"  "Europe & Central Asia"      "Sub-Saharan Africa"         "East Asia & Pacific"       
  # "Middle East & North Africa" "North America"              "South Asia"                 "Antarctica"    "Macaronesia"
  
  # change the name of east asia & pacific for plotting
  countries_data_summary$region <- gsub("East Asia & Pacific", "East Asia, Pacific, & Oceania", countries_data_summary$region)
  
  
  # create bar plots in loop
  barplots <- vector(mode = "list", length = length(unique(countries_data_summary$region)))
  
  for (i in 1:length(unique(countries_data_summary$region))) {
    barplots[[i]] <-  ggplot(
      (countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i])), 
      aes(x = source, 
          y = sum_count, 
          fill = interaction(source, action)) ) +
      geom_bar(position = "stack", stat = 'identity') +
      theme_bw() + 
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 3, color = "white"),
            rect = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.y = element_line(size = 0.1, color = "white"),
            axis.ticks.length=unit(.05, "cm"),         
            axis.ticks.x = element_line(size = 0.5, color = "white"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = c(funk.identified = funk.identified,
                                   funk.recorded = funk.recorded,
                                   chase.identified = chase.identified,
                                   chase.recorded = chase.recorded,
                                   rudd.identified = rudd.identified,
                                   rudd.recorded = rudd.recorded, 
                                   mexia.identified = mexia.identified, 
                                   mexia.recorded = mexia.recorded)) 
  }
  names(barplots) <- unique(countries_data_summary$region)
  
  
  # make a df of xy locations and titles of bar plots
  xmin <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  xmax <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  ymin <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  ymax <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  # barplot_title <- vector(mode = "character", length = length(unique(countries_data_summary$region)))
  
  for (i in 1:length(unique(countries_data_summary$region))) {
    xmin[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(long))$long) - 20
    xmax[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(long))$long) + 20
    ymin[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(lat))$lat) - 8
    ymax[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(lat))$lat) + 8
  }
  
  # make a df of dimensions from above loop
  xy_barplots <- data.frame(region = unique(countries_data_summary$region), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  
  
  # functions to add bar plots and titles
  add_barplots <- function(xy, barplot){
    annotation_custom(ggplotGrob(barplot), 
                      ymin = xy$ymin,
                      ymax = xy$ymax,
                      xmin = xy$xmin, 
                      xmax = xy$xmax) 
  }
  
  add_barplot_titles <- function(xy){
    annotate(geom = "text",
             label = xy$region,
             y = xy$ymax - 0.25,
             x = xy$xmin + 5, 
             color = "white", 
             size = 1.5, 
             hjust = 0)
  }
  
  # # make a fake plot just to steal the legend from!
  # bp_fake_for_legend <- ggplot((countries_data_summary %>% filter(region == "North America")), aes(x = source, y = sum_count, fill = interaction(source, action))) +
  #   geom_bar(position = "stack", stat = 'identity') +
  #   theme_bw() +
  #   theme(legend.position = "bottom",
  #         axis.title.x = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         axis.text.y = element_blank(),
  #         rect = element_blank(),
  #         axis.line.x = element_blank(),
  #         axis.ticks.y = element_blank(),
  #         axis.ticks.x = element_line(size = 0.1, color = "white"),
  #         # text = element_blank(),
  #         panel.grid.major.x = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.grid.major.y = element_blank(),
  #         panel.grid.minor.y = element_blank()) +
  #   # scale_x_discrete(breaks=NULL) +
  #   labs(x = NULL, y = NULL) +
  #   scale_fill_manual("Subject", values = c(funk.identified = funk.identified,
  #                                           funk.recorded = funk.recorded,
  #                                           chase.identified = chase.identified,
  #                                           chase.recorded = chase.recorded,
  #                                           rudd.identified = rudd.identified,
  #                                           rudd.recorded = rudd.recorded,
  #                                           mexia.identified = mexia.identified,
  #                                           mexia.recorded = mexia.recorded), 
  #                     labels = c(funk.identified = "\n\nVicki Funk",
  #                                funk.recorded = "",
  #                                chase.identified = "\n\nMary Chase",
  #                                chase.recorded = "\n\n",
  #                                rudd.identified = "\n\nMarian Pettibone",
  #                                rudd.recorded = "\n\n",
  #                                mexia.identified = "\n\nMary Rathbun",
  #                                mexia.recorded = "\n\n")) 
  
  # map_legend <- cowplot::get_legend(bp_fake_for_legend)
  # grid::grid.draw(map_legend)
  
  if (require("maps")) {
    #Get world map info
    world_map <- map_data("world") 
    #Create a base plot
    map <- ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                   fill = "gray18", 
                   color="gray18", 
                   size = 0.2) + 
      theme_void() 
  
  }
  
  # # make legend 
  # grid::grid.newpage(grid::grid.draw(map_legend))
  
  # add barplots to map
  p <- map
  for (i in 1:length(barplots)) {
    r <- unique(barplots[[i]][["data"]][["region"]])
    p <- p + add_barplots((xy_barplots %>% filter(region == r)), barplots[[i]])
  } 
  p
  
  # add barplot titles to map
  map_with_bars <- p + 
    add_barplot_titles(xy_barplots) 
  
  
  map_with_bars
  
  pdf(file = "output/figures/Figure04_v1.pdf", height = 5, width = 9)
  map_with_bars + coord_fixed()
  dev.off()
  
  #### botany version 2 ####
  
  # load data
  # create function to read tsv with special characters
  read.funky.table <- function(file, ...) {
    tab <- readLines(file)
    tab <- do.call(rbind, strsplit(tab, "\t"))
    colnames(tab) <- tab[1,]
    tab <- tab[-1,]
    tab <- as.data.frame(tab, ...)
    return(tab)    
  }
  
  # apply function to load data (ignore warning about end of line)
  funk <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/funk_countries.tsv")
  calderon <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/calderon_countries_edited.tsv")
  chase <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/chase_countries_edited.tsv")
  rudd <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/rudd_countries_edited.tsv")
  
  # 
  # robinson <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/robinson_countries2.tsv")
  # sfblake <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/sfblake_countries_edited.tsv")
  
  # add columns 
  # combine 
  countries_data <- gdata::combine(funk, calderon, chase, rudd)
  
  # add a column for continent from country
  countries_data$region <- countrycode::countrycode(sourcevar = countries_data[, "country"],
                                                    origin = "country.name",
                                                    destination = "region")
  
  # some don't have a region, apparently because they are in a different language. remove those, then translate them, try again and put them back
  region_NA <- countries_data %>% dplyr::filter(is.na(region))
  countries_data_na.omit <- na.omit(countries_data)
  countries_data_na.omit$country_english <- countries_data_na.omit$country
  
  googleLanguageR::gl_auth(google_auth_json)
  region_NA_translated <- gl_translate(region_NA$country, target = "en", format = "text", source = "fr")
  region_NA$country_english <- region_NA_translated$translatedText
  
  # bind this back to countries_data 
  countries_data_clean <- rbind(countries_data_na.omit, region_NA)
  
  # manually clean and remove some unclear locations
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Capital")
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Indeterminate")
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Unknown")
  countries_data_clean$country_english <- gsub("green cap", "Cape Verde", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Somalis", "Somalia", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Uruguai", "Uruguay", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Southern Africa", "South Africa", countries_data_clean$country_english)
  
  
  # try getting region again
  countries_data_clean$region <- countrycode::countrycode(sourcevar = countries_data_clean[, "country_english"],
                                                          origin = "country.name",
                                                          destination = "region")
  
  
  # add some custom match values for remaining NA matches
  # options (including two custom):
  # "Latin America & Caribbean"  "Europe & Central Asia"      "Sub-Saharan Africa"         "East Asia & Pacific"       
  # "Middle East & North Africa" "North America"              "South Asia"                 "Antarctica"    
  
  # for now splitting "Macaronesia" beween europe (azores and madeira) and sub saharan africa (cape verde)
  
  # unk_region <- countries_data_clean %>% filter(is.na(region)) # run these once just to get the regions
  # v <- unique(unk_region$country_english)
  
  
  # this function requires a named vector for custom matches. 
  # the vector is the region result and the names of the vector elements are the country names
  custom <- c("Latin America & Caribbean",                  "Latin America & Caribbean",       
              "East Asia & Pacific",                        "Latin America & Caribbean",                                      
              "Sub-Saharan Africa",                         "Latin America & Caribbean",                               
              "Europe & Central Asia",                      "Latin America & Caribbean",                                   
              "Latin America & Caribbean",                  "Europe & Central Asia",                           
              "Latin America & Caribbean",                   "Latin America & Caribbean",                             
              "Antarctica",                                 "Sub-Saharan Africa",
              "Sub-Saharan Africa",                         "East Asia & Pacific",
              "Europe & Central Asia",                      "Europe & Central Asia",
              "Latin America & Caribbean",                   "East Asia & Pacific",  
              "Latin America & Caribbean")  
  
  names(custom) <- c("Caribbean",                                  "Virgin Islands of the United States",       
                     "Micronesia",                                 "Saba",                                      
                     "Western Sahara",                             "west indies",                               
                     "Europe & Central Asia",                      "Bonaire",                                   
                     "Virgin Islands",                            "Madeira Islands",                           
                     "Porto Rico",                                 "Santo Domingo",                             
                     "antarctica",                                 "Cape Verde",
                     "Democratic Republic of the Congo or Angola", "British Indian Ocean Territory",
                     "French",                                     "Azores",
                     "Freuch Guiana",                              "U.S. Administered Islands (Micronesia)",
                     "Granada")  
  
  # try getting region again, this time with custom matches
  countries_data_clean$region <- countrycode::countrycode(sourcevar = countries_data_clean[, "country_english"],
                                                          origin = "country.name",
                                                          destination = "region",
                                                          custom_match = custom)
  
  # all good for now! 
  
  
  # summarize counts per region
  countries_data_clean$count <- as.numeric(as.character(countries_data_clean$count))
  countries_data_clean$action <- as.factor(countries_data_clean$action)
  countries_data_clean$region <- as.factor(countries_data_clean$region)
  countries_data_summary <- countries_data_clean %>% group_by(action, source, region, .drop = FALSE) %>% dplyr::summarize(sum_count = sum(count)) %>% ungroup()
  countries_data_summary$action <- as.factor(countries_data_summary$action)
  countries_data_summary$action <- factor(countries_data_summary$action, levels = c("recorded", "identified"))
  
  # set factor/plotting order to birth year order
  # chase 1869 
  # mexia 1870
  # rudd 1910
  # calderon 1929
  # funk 1947
  countries_data_summary$source <- factor(countries_data_summary$source, levels = c("chase", "rudd", "calderon", "funk"))
  
  
  
  # add lat long for each region (manually based on where i want the plot on the map)
  countries_data_summary <- countries_data_summary %>% mutate(lat = dplyr::case_when(region == "Latin America & Caribbean" ~ -11.7832, 
                                                                                     region == "Europe & Central Asia" ~ 54.4507,
                                                                                     region == "Sub-Saharan Africa" ~ 4.284266,
                                                                                     region == "East Asia & Pacific" ~ 58.7946, 
                                                                                     region == "Middle East & North Africa" ~ 19.7917, 
                                                                                     region == "North America" ~ 45.5260,
                                                                                     region == "South Asia" ~ 31.0376,
                                                                                     region == "Antarctica" ~ -77.8628))
  countries_data_summary <- countries_data_summary %>% mutate(long = dplyr::case_when(region == "Latin America & Caribbean" ~ -57.9915, 
                                                                                      region == "Europe & Central Asia" ~ 48.8319,
                                                                                      region == "Sub-Saharan Africa" ~ 26.838434,
                                                                                      region == "East Asia & Pacific" ~ 117.5348, 
                                                                                      region == "Middle East & North Africa" ~ 7.5926, 
                                                                                      region == "North America" ~ -102.2551,
                                                                                      region == "South Asia" ~ 95.4563,
                                                                                      region == "Antarctica" ~ 100.0000))
  
  
  
  #### map ####
  # set custom colors for bar plots 
  
  chase.recorded <- "#0E8C3A" # green
  chase.identified <- "#20E966"
  
  rudd.recorded <- "#00688B" # blue
  rudd.identified <- "#00BEFE"
  
  calderon.recorded <- "#CD1076" # pink
  calderon.identified <- "#F796CA"
  
  funk.recorded <- "#b34700" # orange 
  funk.identified <- "#ff6500"
  
  ##### create a bar plot for each of the 9 regions #####
  # "Latin America & Caribbean"  "Europe & Central Asia"      "Sub-Saharan Africa"         "East Asia & Pacific"       
  # "Middle East & North Africa" "North America"              "South Asia"                 "Antarctica"    "Macaronesia"
  
  # change the name of east asia & pacific for plotting
  countries_data_summary$region <- gsub("East Asia & Pacific", "East Asia, Pacific, & Oceania", countries_data_summary$region)
  
  
  # create bar plots in loop
  barplots <- vector(mode = "list", length = length(unique(countries_data_summary$region)))
  
  for (i in 1:length(unique(countries_data_summary$region))) {
    barplots[[i]] <-  ggplot(
      (countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i])), 
      aes(x = source, 
          y = sum_count, 
          fill = interaction(source, action)) ) +
      geom_bar(position = "stack", stat = 'identity') +
      theme_bw() + 
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 3, color = "white"),
            rect = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.y = element_line(size = 0.1, color = "white"),
            axis.ticks.length=unit(.05, "cm"),         
            axis.ticks.x = element_line(size = 0.5, color = "white"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = c(funk.identified = funk.identified,
                                   funk.recorded = funk.recorded,
                                   chase.identified = chase.identified,
                                   chase.recorded = chase.recorded,
                                   rudd.identified = rudd.identified,
                                   rudd.recorded = rudd.recorded, 
                                   calderon.identified = calderon.identified, 
                                   calderon.recorded = calderon.recorded)) 
  }
  names(barplots) <- unique(countries_data_summary$region)
  
  
  # make a df of xy locations and titles of bar plots
  xmin <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  xmax <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  ymin <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  ymax <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  # barplot_title <- vector(mode = "character", length = length(unique(countries_data_summary$region)))
  
  for (i in 1:length(unique(countries_data_summary$region))) {
    xmin[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(long))$long) - 20
    xmax[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(long))$long) + 20
    ymin[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(lat))$lat) - 8
    ymax[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(lat))$lat) + 8
  }
  
  # make a df of dimensions from above loop
  xy_barplots <- data.frame(region = unique(countries_data_summary$region), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  
  
  # functions to add bar plots and titles
  add_barplots <- function(xy, barplot){
    annotation_custom(ggplotGrob(barplot), 
                      ymin = xy$ymin,
                      ymax = xy$ymax,
                      xmin = xy$xmin, 
                      xmax = xy$xmax) 
  }
  
  add_barplot_titles <- function(xy){
    annotate(geom = "text",
             label = xy$region,
             y = xy$ymax - 0.25,
             x = xy$xmin + 5, 
             color = "white", 
             size = 1.5, 
             hjust = 0)
  }
  
  # # make a fake plot just to steal the legend from!
  # bp_fake_for_legend <- ggplot((countries_data_summary %>% filter(region == "North America")), aes(x = source, y = sum_count, fill = interaction(source, action))) +
  #   geom_bar(position = "stack", stat = 'identity') +
  #   theme_bw() +
  #   theme(legend.position = "bottom",
  #         axis.title.x = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         axis.text.y = element_blank(),
  #         rect = element_blank(),
  #         axis.line.x = element_blank(),
  #         axis.ticks.y = element_blank(),
  #         axis.ticks.x = element_line(size = 0.1, color = "white"),
  #         # text = element_blank(),
  #         panel.grid.major.x = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.grid.major.y = element_blank(),
  #         panel.grid.minor.y = element_blank()) +
  #   # scale_x_discrete(breaks=NULL) +
  #   labs(x = NULL, y = NULL) +
  #   scale_fill_manual("Subject", values = c(funk.identified = funk.identified,
  #                                           funk.recorded = funk.recorded,
  #                                           chase.identified = chase.identified,
  #                                           chase.recorded = chase.recorded,
  #                                           rudd.identified = rudd.identified,
  #                                           rudd.recorded = rudd.recorded,
  #                                           calderon.identified = calderon.identified,
  #                                           calderon.recorded = calderon.recorded), 
  #                     labels = c(funk.identified = "\n\nVicki Funk",
  #                                funk.recorded = "",
  #                                chase.identified = "\n\nMary Chase",
  #                                chase.recorded = "\n\n",
  #                                rudd.identified = "\n\nMarian Pettibone",
  #                                rudd.recorded = "\n\n",
  #                                calderon.identified = "\n\nMary Rathbun",
  #                                calderon.recorded = "\n\n")) 
  
  # map_legend <- cowplot::get_legend(bp_fake_for_legend)
  # grid::grid.draw(map_legend)
  
  if (require("maps")) {
    #Get world map info
    world_map <- map_data("world") 
    #Create a base plot
    map <- ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                   fill = "gray18", 
                   color="gray18", 
                   size = 0.2) + 
      theme_void() 
    
  }
  
  # # make legend 
  # grid::grid.newpage(grid::grid.draw(map_legend))
  
  # add barplots to map
  p <- map
  for (i in 1:length(barplots)) {
    r <- unique(barplots[[i]][["data"]][["region"]])
    p <- p + add_barplots((xy_barplots %>% filter(region == r)), barplots[[i]])
  } 
  p
  
  # add barplot titles to map
  map_with_bars <- p + 
    add_barplot_titles(xy_barplots) 
  
  
  map_with_bars
  
  pdf(file = "output/figures/Figure04_v2.pdf", height = 5, width = 9)
  map_with_bars + coord_fixed()
  dev.off()
  
  # pdf(file = "map_with_bars_legend.pdf", height = 5, width = 9)
  # grid::grid.newpage(grid::grid.draw(map_legend))
  # dev.off()
  
  
  
  
  
  
  
  
  
  #### invertebrate zoology ####
  
  setwd('') # set your wd here
  
  # load data
  # create function to read tsv with special characters
  read.funky.table <- function(file, ...) {
    tab <- readLines(file)
    tab <- do.call(rbind, strsplit(tab, "\t"))
    colnames(tab) <- tab[1,]
    tab <- tab[-1,]
    tab <- as.data.frame(tab, ...)
    return(tab)    
  }
  
  
  # apply function to load data (ignore warning about end of line)
  mclaughlin <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/mclaughlin_countries_edited.tsv")
  rice <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/rice_countries_edited.tsv")
  rathbun <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/rathbun_countries_edited.tsv")
  pettibone <- read.funky.table("../Attributions_groupedby_country_from_Bionomia/pettibone_countries_edited.tsv")
  
  
  # add columns 
  # combine 
  countries_data <- gdata::combine(mclaughlin, rice, rathbun, pettibone)
  
  # add a column for continent from country
  countries_data$region <- countrycode::countrycode(sourcevar = countries_data[, "country"],
                                                    origin = "country.name",
                                                    destination = "region")
  
  # some don't have a region, apparently because they are in a different language. remove those, then translate them, try again and put them back
  region_NA <- countries_data %>% dplyr::filter(is.na(region))
  countries_data_na.omit <- na.omit(countries_data)
  countries_data_na.omit$country_english <- countries_data_na.omit$country
  
  googleLanguageR::gl_auth(google_auth_json)
  region_NA_translated <- gl_translate(region_NA$country, target = "en", format = "text", source = "fr")
  region_NA$country_english <- region_NA_translated$translatedText
  
  # bind this back to countries_data 
  countries_data_clean <- rbind(countries_data_na.omit, region_NA)
  
  # manually clean and remove some unclear locations
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Capital")
  countries_data_clean <- countries_data_clean %>% dplyr::filter(country_english != "Indeterminate")
  countries_data_clean$country_english <- gsub("green cap", "Cape Verde", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Somalis", "Somalia", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Uruguai", "Uruguay", countries_data_clean$country_english)
  countries_data_clean$country_english <- gsub("Southern Africa", "South Africa", countries_data_clean$country_english)
  
  
  # try getting region again
  countries_data_clean$region <- countrycode::countrycode(sourcevar = countries_data_clean[, "country_english"],
                                                          origin = "country.name",
                                                          destination = "region")
  
  
  # add some custom match values for remaining NA matches
  # options (including two custom):
  # "Latin America & Caribbean"  "Europe & Central Asia"      "Sub-Saharan Africa"         "East Asia & Pacific"       
  # "Middle East & North Africa" "North America"              "South Asia"                 "Antarctica"    
  
  # for now splitting "Macaronesia" beween europe (azores and madeira) and sub saharan africa (cape verde)
  
  # unk_region <- countries_data_clean %>% filter(is.na(region)) # run these once just to get the regions
  # v <- unique(unk_region$country_english)
  
  
  # this function requires a named vector for custom matches. 
  # the vector is the region result and the names of the vector elements are the country names
  custom <- c("Latin America & Caribbean",                  "Latin America & Caribbean",       
              "East Asia & Pacific",                        "Latin America & Caribbean",                                      
              "Sub-Saharan Africa",                         "Latin America & Caribbean",                               
              "Europe & Central Asia",                      "Latin America & Caribbean",                                   
              "Latin America & Caribbean",                  "Europe & Central Asia",                           
              "Latin America & Caribbean",                  "Latin America & Caribbean",                             
              "Antarctica",                                 "Sub-Saharan Africa",
              "Sub-Saharan Africa",                         "East Asia & Pacific",
              "Europe & Central Asia",                      "Europe & Central Asia",
              "Sub-Saharan Africa",                          "East Asia & Pacific")  
  
  names(custom) <- c("Caribbean",                                  "Virgin Islands of the United States",       
                     "Micronesia",                                 "Saba",                                      
                     "Western Sahara",                             "west indies",                               
                     "Europe & Central Asia",                      "Bonaire",                                   
                     "Virgin Islands",                            "Madeira Islands",                           
                     "Porto Rico",                                 "Santo Domingo",                             
                     "antarctica",                                 "Cape Verde",
                     "Democratic Republic of the Congo or Angola", "British Indian Ocean Territory",
                     "French",                                     "Azores",
                     "St Helena",                                "Wallis and Futuna Islands")  
  
  # try getting region again, this time with custom matches
  countries_data_clean$region <- countrycode::countrycode(sourcevar = countries_data_clean[, "country_english"],
                                                          origin = "country.name",
                                                          destination = "region",
                                                          custom_match = custom)
  
  # all good for now! 
  
  
  # summarize counts per region
  countries_data_clean$count <- as.numeric(as.character(countries_data_clean$count))
  countries_data_clean$action <- as.factor(countries_data_clean$action)
  countries_data_clean$region <- as.factor(countries_data_clean$region)
  countries_data_summary <- countries_data_clean %>% group_by(action, source, region, .drop = FALSE) %>% dplyr::summarize(sum_count = sum(count)) %>% ungroup()
  countries_data_summary$action <- as.factor(countries_data_summary$action)
  countries_data_summary$action <- factor(countries_data_summary$action, levels = c("recorded", "identified"))
  
  # set factor/plotting order to birth year order
  # rathbun 1860-1943
  # pettibone 1908-2003
  # rice 1926-2021
  # mclaughlin 1932-2011 
  
  countries_data_summary$source <- factor(countries_data_summary$source, levels = c("rathbun", "pettibone", "rice", "mclaughlin"))
  
  
  # add lat long for each region (manually based on where i want the plot on the map)
  countries_data_summary <- countries_data_summary %>% mutate(lat = dplyr::case_when(region == "Latin America & Caribbean" ~ -11.7832, 
                                                                                     region == "Europe & Central Asia" ~ 54.4507,
                                                                                     region == "Sub-Saharan Africa" ~ 4.284266,
                                                                                     region == "East Asia & Pacific" ~ 58.7946, 
                                                                                     region == "Middle East & North Africa" ~ 19.7917, 
                                                                                     region == "North America" ~ 45.5260,
                                                                                     region == "South Asia" ~ 31.0376,
                                                                                     region == "Antarctica" ~ -77.8628))
  countries_data_summary <- countries_data_summary %>% mutate(long = dplyr::case_when(region == "Latin America & Caribbean" ~ -57.9915, 
                                                                                      region == "Europe & Central Asia" ~ 48.8319,
                                                                                      region == "Sub-Saharan Africa" ~ 26.838434,
                                                                                      region == "East Asia & Pacific" ~ 117.5348, 
                                                                                      region == "Middle East & North Africa" ~ 7.5926, 
                                                                                      region == "North America" ~ -102.2551,
                                                                                      region == "South Asia" ~ 95.4563,
                                                                                      region == "Antarctica" ~ 100.0000))
  
  
  
  #### map ####
  # set custom colors for bar plots 
  
  rathbun.recorded <- "#0E8C3A" # green
  rathbun.identified <- "#20E966"
  
  pettibone.recorded <- "#00688B" # blue
  pettibone.identified <- "#00BEFE"
  
  rice.recorded <- "#CD1076" # pink
  rice.identified <- "#F796CA"
  
  mclaughlin.recorded <- "#b34700" # orange
  mclaughlin.identified <- "#ff6500"
  
  ##### create a bar plot for each of the 9 regions #####
  # "Latin America & Caribbean"  "Europe & Central Asia"      "Sub-Saharan Africa"         "East Asia & Pacific"       
  # "Middle East & North Africa" "North America"              "South Asia"                 "Antarctica"    "Macaronesia"
  
  # change the name of east asia & pacific for plotting
  countries_data_summary$region <- gsub("East Asia & Pacific", "East Asia, Pacific, & Oceania", countries_data_summary$region)
  
  
  # create bar plots in loop
  barplots <- vector(mode = "list", length = length(unique(countries_data_summary$region)))
  
  for (i in 1:length(unique(countries_data_summary$region))) {
    barplots[[i]] <-  ggplot(
      (countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i])), 
      aes(x = source, 
          y = sum_count, 
          fill = interaction(source, action)) ) +
      geom_bar(position = "stack", stat = 'identity') +
      theme_bw() + 
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 3, color = "white"),
            rect = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.y = element_line(size = 0.1, color = "white"),
            axis.ticks.length=unit(.05, "cm"),         
            axis.ticks.x = element_line(size = 0.5, color = "white"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(values = c(mclaughlin.identified = mclaughlin.identified,
                                   mclaughlin.recorded = mclaughlin.recorded,
                                   rathbun.identified = rathbun.identified,
                                   rathbun.recorded = rathbun.recorded,
                                   pettibone.identified = pettibone.identified,
                                   pettibone.recorded = pettibone.recorded, 
                                   rice.identified = rice.identified, 
                                   rice.recorded = rice.recorded)) 
  }
  names(barplots) <- unique(countries_data_summary$region)
  
  
  # make a df of xy locations and titles of bar plots
  xmin <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  xmax <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  ymin <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  ymax <- vector(mode = "numeric", length = length(unique(countries_data_summary$region)))
  # barplot_title <- vector(mode = "character", length = length(unique(countries_data_summary$region)))
  
  for (i in 1:length(unique(countries_data_summary$region))) {
    xmin[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(long))$long) - 20
    xmax[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(long))$long) + 20
    ymin[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(lat))$lat) - 8
    ymax[i] <- unique((countries_data_summary %>% filter(region == (unique(countries_data_summary$region))[i]) %>% select(lat))$lat) + 8
  }
  
  # make a df of dimensions from above loop
  xy_barplots <- data.frame(region = unique(countries_data_summary$region), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  
  
  # functions to add bar plots and titles
  add_barplots <- function(xy, barplot){
    annotation_custom(ggplotGrob(barplot), 
                      ymin = xy$ymin,
                      ymax = xy$ymax,
                      xmin = xy$xmin, 
                      xmax = xy$xmax) 
  }
  
  add_barplot_titles <- function(xy){
    annotate(geom = "text",
             label = xy$region,
             y = xy$ymax - 0.25,
             x = xy$xmin + 5, 
             color = "white", 
             size = 1.5, 
             hjust = 0)
  }
  
  # # make a fake plot just to steal the legend from!
  # bp_fake_for_legend <- ggplot((countries_data_summary %>% filter(region == "North America")), aes(x = source, y = sum_count, fill = interaction(source, action))) +
  #   geom_bar(position = "stack", stat = 'identity') +
  #   theme_bw() +
  #   theme(legend.position = "bottom",
  #         axis.title.x = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         axis.text.y = element_blank(),
  #         rect = element_blank(),
  #         axis.line.x = element_blank(),
  #         axis.ticks.y = element_blank(),
  #         axis.ticks.x = element_line(size = 0.1, color = "white"),
  #         # text = element_blank(),
  #         panel.grid.major.x = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.grid.major.y = element_blank(),
  #         panel.grid.minor.y = element_blank()) +
  #   # scale_x_discrete(breaks=NULL) +
  #   labs(x = NULL, y = NULL) +
  #   scale_fill_manual("Subject", values = c(mclaughlin.identified = mclaughlin.identified,
  #                                           mclaughlin.recorded = mclaughlin.recorded,
  #                                           rathbun.identified = rathbun.identified,
  #                                           rathbun.recorded = rathbun.recorded,
  #                                           pettibone.identified = pettibone.identified,
  #                                           pettibone.recorded = pettibone.recorded,
  #                                           rice.identified = rice.identified,
  #                                           rice.recorded = rice.recorded), 
  #                     labels = c(mclaughlin.identified = "\n\nVicki Funk",
  #                                mclaughlin.recorded = "",
  #                                rathbun.identified = "\n\nMary Chase",
  #                                rathbun.recorded = "\n\n",
  #                                pettibone.identified = "\n\nMarian Pettibone",
  #                                pettibone.recorded = "\n\n",
  #                                rice.identified = "\n\nMary Rathbun",
  #                                rice.recorded = "\n\n")) 
  
  # map_legend <- cowplot::get_legend(bp_fake_for_legend)
  # grid::grid.draw(map_legend)
  
  if (require("maps")) {
    #Get world map info
    world_map <- map_data("world") 
    #Create a base plot
    map <- ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                   fill = "gray18", 
                   color="gray18", 
                   size = 0.2) + 
      theme_void() 
    
  }
  
  # # make legend 
  # grid::grid.newpage(grid::grid.draw(map_legend))
  
  # add barplots to map
  p <- map
  for (i in 1:length(barplots)) {
    r <- unique(barplots[[i]][["data"]][["region"]])
    p <- p + add_barplots((xy_barplots %>% filter(region == r)), barplots[[i]])
  } 
  p
  
  # add barplot titles to map
  map_with_bars <- p + 
    add_barplot_titles(xy_barplots) 
  
  
  map_with_bars
  
  pdf(file = "output/figures/Figure05.pdf", height = 5, width = 9)
  map_with_bars + coord_fixed()
  dev.off()
  
  # pdf(file = "map_with_bars_legend.pdf", height = 5, width = 9)
  # grid::grid.newpage(grid::grid.draw(map_legend))
  # dev.off()
  
  
  
  
  
