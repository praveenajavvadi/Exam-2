getwd()

#clear environment
rm(list = ls(all= TRUE))

#importing data
library(WDI)
female_flfp = WDI(country = "all",
                 indicator = c("SL.TLF.CACT.FE.ZS"),
                 start = 2010, end = 2015,
                 extra = FALSE, cache = NULL)
#rename variable
female_flfp <-
  female_flfp%>%
  rename(flfp = "SL.TLF.CACT.FE.ZS")

#collapsing data
collapsed_flfp <-
  female_flfp%>%
  group_by(iso2c, country)%>%
  summarize(mean_flfp = mean(flfp, na.rm = TRUE))

#show which countries have <15% participation rates




#loading packages
library(rio)
library(tidyverse)
library(googlesheets4)
library(labelled)
library(data.table)
library(varhandle)
library(ggrepel)
library(geosphere)
library(rgeos)
library(viridis)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)
library(rnaturalearthhires)
library(remotes)
library(raster)
library(sp)
library(sf)
library(Imap)
library(rnaturalearthhires)
library(ggsflabel)

#load the world map
world_borders <- st_read("/Users/praveenajavvadi/College/world border shape files")

#make map compatible with GIS formats <------
borders <- st_transform(world_borders, "+proj=longlat +ellps=WGS84 +datum=WGS84")
rm(world_borders)

#leftjoin so that collapsed_flfp can have the longitude and latitude
collapsed_flfp <-
  collapsed_flfp%>%
  rename(ISO2 = "iso2c")
collapsed_flfp = left_join(collapsed_flfp, borders, by=c("ISO2"))
View(collapsed_flfp)

#only keep necessary variables
collapsed_flfp <- collapsed_flfp%>%
  distinct(country, mean_flfp, LON, LAT, ISO2)

names(collapsed_flfp)

collapsed_flfp <- na.omit(collapsed_flfp, 
                          select = c("LON", "LAT",
                          "ISO2", "country", 
                                     "mean_flfp"))
#set as SF
collapsed_flfp_sf <- st_as_sf(collapsed_flfp, 
                               coords = c("LON", "LAT"),
                               crs = 4326,
                               agr = "constant")

#getting world map
world <- ne_countries(scale = "large", returnclass = "sf")


##now make world map using ggplot and sf
library(viridis)

world_basic =ggplot()+
  geom_sf(data = world)+
  geom_sf(data = collapsed_flfp_sf)+
  scale_color_viridis()
world_basic
print(world_basic)

#asia
#show same cluster of states from previous question

#shiny app main components
#load shiny library
#1. specify theUser Interface (UI); 
#2. fill out a server function; and 
#3. then execute the Shiny app.


#pulling pdf from denly
library(pdftools)
library()
text=pdf_text(pdf="https://pdf.usaid.gov/pdf_docs/PA00TNMJ.pdf")
#creating dataframe
armeniatext <- data.frame(text,stringsAsFactors = FALSE)

#tokenizing the data
armeniatext = armeniatext%>%
  unnest_tokens(word, text)

data("stop_words")
armeniatext <- armeniatext%>%
  anti_join(stop_words)

#top 5 most used words
armeniatext %>%
  count(word, sort = TRUE)%>%
  filter(n > 100)
#law, corruption, rule, armenia, european






#load billboard hot 100 page
#pulling website and reading it as html <------
library(rvest)
library(dplyr)
hot100page <- "https://www.billboard.com/charts/hot-100"
hot100 <- read_html(hot100page)

#obtaining nodes from teh webpage
body_nodes <- hot100 %>%
  html_node("body") %>%
  html_children()
body_nodes

#gets more specific
body_nodes%>%
  html_children()

#pulling specific data from webpage <-----

rank <- hot100 %>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-element__rank__number')]")%>%
  rvest::html_text()

artist <- hot100%>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-element__information__artist')]") %>%
  rvest::html_text()

title <- hot100%>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-element__information__song')]") %>%
  rvest::html_text()

last_week <- hot100%>%
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//span[contains(@class,
                     'chart-sorter__button')]") %>%
  rvest::html_text()

#create data frame using the data <-------
chart_df <- data.frame(rank,artist,title)

knitr::kable(
  chart_df %>% head(10))



