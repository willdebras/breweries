setwd("C:/Users/Will Bonnell/Desktop/Start Up/R shit")

#Scraping and visualizing Canadian breweries 

#Install necessary packages if they aren't installed, then load them all
p_needed <- c("TSP", "ggmap", "plyr", "dplyr", "tidytext", "stringr", "lubridate", "jsonlite", "httr", "xml2", "rvest", "devtools", "ggmap", "maps", "networkD3", "pageviews", "aRxiv", "twitteR", "streamR", "rtweet")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

lapply(p_needed, require, character.only = TRUE)


#Scrape the top rated Canadian breweries from Untappd

url_p <- read_html("https://untappd.com/brewery/top_rated?country_id=32")
headlines <- html_nodes(url_p, css = ".num , .ibu , a")
breweries_raw <- html_text(headlines)
head(breweries_raw)
length(breweries_raw)

#Replace line breaks with normal spaces and trim this white space

breweries_clean <- breweries_raw %>% str_replace_all("\\n", "") %>% str_trim()
length(breweries_clean)


#Subset the data we want and turn it into a dataframe
#Using the matrix command we can specify we want four columns out of this character vector

brew1 <- breweries_clean[23:221]
brew_df <- as.data.frame(matrix(brew1, ncol=4, byrow=TRUE)) %>%
  select(V1, V2, V3) %>%
  `colnames<-`(c("brewery", "review", "rating")) %>%
  mutate(brewery = as.character(brewery)) %>%
  mutate_geocode(brewery, output = "latlon")



write.csv(brew_df, "brew_ca_df.csv")


brew_df <- read.csv("brew_ca_df.csv")


brew_df2 <- brew_df %>%
  mutate(latlon = paste(lat, lon, sep = "+")) %>%
  filter(!is.na(lat))

distances <- gmapsdistance(origin = brew_df2$latlon,
                           destination = brew_df2$latlon,
                           combinations = "all",
                           mode = "driving")$Distance[, -1]


distances1 <- distances %>%
  `colnames<-`(brew_df2$brewery) %>%
  `rownames<-`(brew_df2$brewery)

distances2 <- distances1[-13,-13]

write.csv(distances2, "distances_canada.csv")


tsp <- TSP(as.dist(distances2))

tour <- solve_TSP(tsp)

tour_order <- as.integer(tour)

brew_df3 <- brew_df2 %>%
  filter(!X==14)

brew_df4 <- brew_df3[tour_order,]

route <- lapply(seq(nrow(brew_df4) - 1), function(n) {
  route(brew_df4$latlon[n], brew_df4$latlon[n+1], structure = "route") %>%
    mutate(section = n)
})

route <- route %>% bind_rows()

map <- get_map(location = c(lon = -91, lat = 42), zoom = 3, maptype = "roadmap")

g <- ggmap(map, extent = "device") +
  geom_path(data = route, aes(x = lon, y = lat),  colour = "blue", size = 1, alpha = 0.6) +
  geom_point(data = brew_df4, aes(x = lon, y = lat), size = 2.5, alpha = 0.9) +
  labs(x = "", y = "")

g
