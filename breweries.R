setwd("C:/Users/Will Bonnell/Desktop/Start Up/R Shit")


#Scraping and visualizing Chicago breweries 

#Install/load Necessary packages
p_needed <- c("htmltools", "leaflet", "tm", "ggmap", "plyr", "dplyr", "tidytext", "stringr", "lubridate", "jsonlite", "httr", "xml2", "rvest", "devtools", "ggmap", "maps", "networkD3", "pageviews", "aRxiv", "twitteR", "streamR", "rtweet", "DT", "htmlwidgets")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

lapply(p_needed, require, character.only = TRUE)



url_p <- read_html("http://thehopreview.com/blog/chicago-brewery-list")
headlines <- html_nodes(url_p, css = "p")
breweries_raw <- html_text(headlines)
head(breweries_raw)
length(breweries_raw)

breweries_clean <- breweries_raw %>% str_replace_all("\\n", "") %>% str_trim()
length(breweries_clean)

brew <- as.data.frame(breweries_clean)
#pattern <- "\s+(?=\d){1,3}"
#pattern <- "[:space:][0-9]"

brew1 <- brew[3:209,] %>%
  str_split("\\s+(?=\\d)", simplify = TRUE, n = 2) %>%
  `colnames<-`(c("name", "address"))

brew2 <- as.data.frame(brew1)
addresses <- str_split_fixed(brew2$address, "-|\\s+(?=\\[)", n = 2) %>%
  cbind(brew2) %>%
  select(name, `1`, `2`, address) %>%
  `colnames<-`(c("name", "address", "type", "original")) %>%
  mutate(address = as.character(address)) %>%
  mutate_geocode(address, output = "latlon") #%>%
  #`colnames<-`(c("Address", "Area", "X", "Longitude", "Latitude"))


addresses$address <- removePunctuation(addresses$address)
addresses$address <- as.character(addresses$address)

#Some geocodes might fail. 
#We can run this loop to try again and sub the lon lat columns
for(i in 1:nrow(addresses))
{
  result <- geocode(addresses$address[i], output = "latlon", source = "google")
  if (is.na(addresses$lon[i])) addresses$lon[i] <- as.numeric(result[1])
  if (is.na(addresses$lat[i])) addresses$lat[i] <- as.numeric(result[2])
}

write.csv(addresses_full, "breweriesv2.csv")





addresses_full <- select(addresses_full, names, address, type, original, lon, lat)

View(addresses_full)
addresses_full$type <- gsub("\\w+(?=\\[)", "", addresses_full$type, perl=T)


addresses_full <- read.csv("breweriesv2.csv")

addresses_full_2 <- mutate(addresses_full, location = "Suburb") 
addresses_full_2$location <- ifelse(as.numeric(rownames(addresses_full)) < 70, c("Chicago Proper"), c("'Burbs"))
addresses_full_2$location <- as.factor(addresses_full_2$location)


addresses_full_2$type <- as.character(addresses_full_2$type)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
addresses_full_2$type <- trim(addresses_full_2$type)
addresses_full_2$type <- as.factor(addresses_full_2$type)

brew <- mutate(addresses_full_2, type=fct_recode(type, 
                                                 NULL = "",
                                                 NULL = "Logan Square",
                                                 NULL = "Near West Side",
                                                 NULL = "Albany Park",
                                                 NULL = "Belmont Cragin",
                                                 NULL = "Englewood",
                                                 NULL = "Loop",
                                                 NULL = "Old Irving Park",
                                                 NULL = "Roseland",
                                                 NULL = "River North",
                                                 NULL = "Ravenswood",
                                                 NULL = "Pullman",
                                                 NULL = "Irving Park",
                                                 NULL = "West Loop",
                                                 "Kitchen and Tap" = "[K]",
                                                 "Kitchen and Tap" = "Old [K]",
                                                 "Tours and Tap" = "[K, T]",
                                                 "Tours and Tap" = "[K, T]",
                                                 "Taproom" = "[TR]",
                                                 "Taproom" = "Back of [TR]",
                                                 "Taproom" = "Near [TR]",
                                                 "Tours" = "[T,[Bottle Shop]",
                                                 "Tours" = "[T]",
                                                 "Tours" = "[T, Bottle Shop]",
                                                 "Tours and Tap" = "[TR, T]",
                                                 "Tours and Tap" = "Near [TR, T]",
                                                 "Bottle Shop" = "[Limited Bottle Shop]",
                                                 "Bottle Shop" = "[Bottle Shop]"))


brew[189,] <- mutate(brew, type = "Tours and Tap")
brew[195,] <- mutate(brew, type = "Tours and Tap")
brew[59:60,] <- mutate(brew, type = "Tours and Tap")

write.csv(brew, "breweriesv3.csv")
brew <- read.csv("breweriesv3.csv")


icons <- iconList(
  `Tours and Tap` = makeIcon("table.png", 32),
  `Taproom` = makeIcon("beer.png", 32),
  `Bottle Shop` = makeIcon("bottle.png", 32),
  `Kitchen and Tap` = makeIcon("sausage.png", 32), 
  `Tours` = makeIcon("keg.png", 32)
)



html_legend <- "<img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAAAA3NCSVQICAjb4U/gAAAACXBIWXMAAAejAAAHowEwL7LFAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAAPlQTFRF//////8AcDAAaS0AaTIF8cQX78UZ8MIb8cQYaTAGqYNZay8GazEGajEFajAFai8FhlUr78QY8MQZ8MMYajAG8MQZaTEGdD0T8MQZcz0SazAGcjoQajAG2q8ZajAGajAGaTAGtokVt4oV8MQZ8MMZsYUVtYwXtIoXpnwWpHsX8MQZajAGajAGajAGf2FDgGJEgWNFjm1LkW9Mk3FNlnNPmXZQm3dRoHtUoXtUoXxUonxUpH5Vp4FXqYJYq4VbtY9oupVvwZ14zquK1LGR2LAa2rIa3bue37Ua4LYa47ka5rsa5rwa68AZ68yy7c2z8MQZ8NC389O79NW98ZhDgwAAAC10Uk5TAAEQETg4PkNJUFBWWF5gZ4GSmKewt7i9v8HJycvN09Tc39/h4+Tm5/Dz/P3+bafS9QAAAQpJREFUOE/ty0dDwkAQhuGhQ+i9Ewy9LJrYsGAFqbrK//8xzszmuMDVg9/pyc4bAAi1pJTtMAB4skMph1kPMtzGx1YIaPnVdr5ZllDB3WK9XuyCyNJyM9+u8hyku9bgrJdBBer9TqdfDyAzPWtgddMcQE4IUWD5UcLPLKBy8HeCGrrBilAQYTZQNXWPjek5jvJVSBUfMk4ax+ieGAn+SILXZAnTC0n+SYwSGLivoglRVyIKTVcmgFGsqpWNlKtqyii7KhowO7H/4GTw/k07ErzuaTCdPh64/6hgMrnV3p8+9ypwHOdeWzx/qcC27TttMHtRwQXuWhu8fdDgCnfzoC14cE67PBIcPqn9AtdmuO4Coo9rAAAAAElFTkSuQmCC'
style='width:30px;height:30px;'>  Bottle Shop<br/> 

<img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAIhAAACIQBDVcC+gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAMbSURBVFiF7ZZPaBRXHMc/vzcz+8e1iYv5MxNLT1apeFLQg0Fa8W5PPZTSiyeh9Oy1pbQ3EXrooRBBULGlvSi9FESDQinEg/ivl4AN2Uk1NqRs1Elm3s/DbHY3u7PZ3UTjoX5h4L33/c37fd6bmd88UVU6aXZ2do8x5nPgU2BFVX/3PO/08PBwteNNfUo6AczMzIy5rvsQeKfFuisi11V1N3DTcZxLIyMj068coFKpXBaRT3qY44m1djyXy1XiOD4B7DTGXBsdHb3bN8Dc3NwhVf0COAbs6mWCmuaBElCs9a2IfOX7/tc9A4Rh+AFwv4+kXSUiH/q+f2O9GNMUfOpVJgdQ1Y+7xdQBVPX4awDY0S3GzYLpR4X5CzfcF9NupinOY8a+7xlgQ/L+u/W+YMeyPE1fznXV06q9panb2Hildbzw9NfJTsl7VeYOlGa/ualmW4J4ONGjAUmqBwrO4NSzkZPFpLh7j4kXn+b//eWBV506spnkrQB3gL3ACycKi5AcbA6UZPFgKTwDEAOjtWvTqgMsLCx8Vi6XJ1T1HiQ/9wi9vpztA2EYnm0aeex53rmhoaFwdSCzFC/dOvGHeHoYAV0WAGykiCuIAxgwOYuNDChorGgCJp/GmrxFEyFh55/V97491DL9CnA8CILJjqtxSnbYlJI08TNDVBFslIJ6ZUMuiEHAWMtyxSFetGnigpDfpZhC2pel54PAP7Vpy0AO8IAvgUno8BWYvB2qt3O2njylsyC17TOKXW54NlJM3tb7kqtuD4LAD4LAF5F3gUc1a399/iwARRsVstWU9ug1zWZf1Vlt+r7/BGj7L2yo+m1CbevZaoA2vQX43wEErQOb/h2vK2OcMAxPA6jqXhFZPfTUj/WvFUDUcYHvAETWFJCLWwKQoXkRmXBd98ctAVBdiYIgaKudzXrjX0HmDsRV928jug9AreAONDxRSBYbi3KKgvEafrMH8teGAFg2v1lSAACTX2vbqJFEnPTK8lCudAPIfASuxj8ANsvrQ89jkokNAeTHr06r8BHwE+kJph9FwHmBo4PjVxe6Bb8Ea1cRnDxOOXkAAAAASUVORK5CYII='  
style='width:30px;height:30px;'>  Taproom<br/>

<img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAA3QAAAN0BcFOiBwAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAPySURBVFiFtVdbaBxlFP7O+Wd2ZyebmNisTSMhJm1MYy/bJG4waaEPUkjSh1KK4JMXBCH0rUUE6ZsXfBAVK6gPSipegiVtBbXihYIhtDQYEQMBLU0gdtuNqWzMZScznf/4EEgTnd1NmtkD8zLnfBfO/88//yERQRhBRAkmmtQi9ur3BtFNT+vafDgjFPXlmAUQebW5GZ2VlQCA/hs3cDad/qcQiMNSFxE3SjT+l+vCFQET4ff5eS+n9cViwNAeAn4AIBazr4g0AAHwTiFMaOIG84m4Uu659nZZ6ukRr6dHziSTYjF7DLxUUgMAthDgf51KyfjBg/LEtm3y84ED4vf2ykBrqxhEOQCxIGxYe+CRStP0uhMJnBwfx8TiInbG4wCAozU1MJkVgENBwLAMPPxAJKIBYCSbxQuNjbB4mdoggs2sAdhBwLAMVJQpRQBgMcPRek3S0ZoBOKU0ELON5SOlOR7HlkhkTdLVWgHIBQHDOojsMl7u+WBbG+LGXVotAk+k5B0ot5VSAHBsdBS3XXclsWo5SteBGHNvR2WlAoCvUimYRCu5nO+veCmJASJqArDjaE0NAKwRB4CxuTkQoAX4MwgfxhIc22HbTlNZWWDyXCYDS6nLIvJ36AaIiCzmp56srbWC8gLgi3Tazfn+5/k4NtuB5wVoerauLjA5ks1i2nVNAOdDN0BE9QbRW2+0tBgPxWKBNRcyGcSUGhWRdD6ee9qERERRpT5OVVSovvr6vHUD6bST8/3PCnFtuANEZCjm9xjoOrNvX4Ty1H2ZyWDKcQwAZwvxbagDRHRfVKnzNvP+b1KpvK3P+T76xsYWtci7IjIVigEiaogyf19vWXXfdnRE6vOIA8Br1645M647B+DlYrzrMkBED1rMw11VVdWD7e1mhZEfdiWbxevXrxta5KSIzIdiwGL+rjuRSAy0tRn/PelWx5Tj4PDIyJIW+UREPl0Pd1EDRNTAQMvpXbuokPii7+Pw1auuo/UvAPrWI17UABEZAJ6JG4ZXa1mRtOPAUgr3m+b/asfn53/9Y2FhwRU5IiLeeg0EfoZEVE5Epyzmm4ro1ILvm9Oui8Fbt9B46RKSQ0OYzN39u749MXElZZqPLWm9X0Rm1isOABQ0mhHR0wD6X9y+HScaGtA6NOR1VVWpD5NJ/nFmBr4IjmzdipHZWcSITj86PDzti7yyEeGVyHPNNqPMv3UnEp7f2ys/dXaKItINtn3ngz17pD+ZlEPV1S4BGsDjm7rSF7jr71RES+/v3i0f7d0rTHTHILoYYZ4pV2rSZH4TQNumZ4oiA8fxKLNnEvkAjoc1Ra1+AvfA6rCYLwjR7SXff+6e1rhI/AtxXQhoUyapwAAAAABJRU5ErkJggg=='
style='width:30px;height:30px;'>  Kitchen and Tap<br/>

<img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAA3QAAAN0BcFOiBwAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAARLSURBVFiF7VZLbFRlFP7Of1/TO52H4xSw0/RBQaYzBsRgKglNFBJQ66ILSjQkyEIWJBZiiNCurAshLlgZYzS6aBNZ4IINhsAGGhaNpFJJgFRraWtSdBxneplX587MvccFnXGcTmWKaDc9yb+49zvnO9/5z/8iZsZqmljV7GsCViJA1/WLiqKmHw4lTUQnixgRnVQUJV3EdV2/+MQFCCHa9vf26kPDw3ooFNIABMrgQCgU0oaGh/X9vb26EKLtiQuwgVg8FkMkEoFhGAwgXgbHDcPgSCSCeCwGG4jVygtmrmlomnrB63bylvZGVhSZAfQVMQB9iiLzlvZG9rqdrGnqhVp5a54B08xFu/eEMXF1AH6fy66cAb/PZU9cHUD3njBMMxetlZeqHURE5BZAvyZJB8GsAUCO2aVqSp3f58Lcr3GSiAwZMAGgAGgWszfwjI//iCeRM/MLKlFykcw0LeucDZxh5kStAvpb6uoGB9rbNU38u51q2jbOTE2ZswsLHzDzx0tyAbgO4G1mvlf86ZTlsUaHY/smp27XkqTd6bQ/Cgazgghfzv6ivrFhff7T6Wn1x1RKAoCf0xlxP5sdTxcKO8qK3AhgSAawC0AzgJKAjGWdmMpkvp6XRWDdU+5HCpjXZRynjAoAczrhe9l03NNlGLaK3+cTiGcyc8x8oiKsGcAuuRohM48Q0elkZuFso9/rkKq0QVVkdLQGwAxMzM7hh8nZEhY1HrZflgSSmYUsM59m5pFquaoKWLSvcvlC/u7M/YMEOCrBgmV1+tz1AIAbd6cgS9J3SwoBspZlnQMwtFySooDDRPRyZQ4Ao5Zl7WXmXGUgEXH5As4XCi9V8VEBdAF4n4gqi20tCQjV17/lUZS/LbiCbeNOKiXlbDtLRM8y82/LVVHNiGiDTPSTKoQjXF9vKRVtfJDPizupFGRBZJ3t6FD3NjQsIbGZ0TU6yjcM4xSA91YiQACndng82vWdOxVBpFTiV6JRdI+NWbJE9NnR27eP9LW2am26XnKwmHErkcDNRELYwOWVJAcAG7h8M5F498PJSWxzuyERlbDpTAafzMyYEtEXAOAB0O+U5XGFKEWAJYhyqhAJXZIuAThU7QwHwPs6t/K+zq0MgJfxOaRL0iVViIQgyhFgKUQppyyPA+gH4KkWdA3A4KMukVoEVPgPArj22JfRf2VrAv7pJFxiRBQAcFSSaPvi919EsvjWsvgWgM+ZeXYZiscXQEROQTTh97rkpz0uh6YqKF5Uz29ugZkvvB57kNwdNZLHiWg9M6dq4S21gIiCRNQDwA8gSEQ9ROQv831TkoQj3NbkaFrnw7ZNzcjm8iAibNvcguc2NiHc1uSQJaECeKeM17/IGwTgX+QNlljLtsl5AEbFOFaGvyqIkoIoLQkR2/1C+IjHWTdy4JUXDxx+raunwev6RhClBVEKwJWyuGNVeM8X8aovov/TVn0XrAn4E2OWVKvXXuBOAAAAAElFTkSuQmCC'
style='width:30px;height:30px;'>  Tours and Tap<br/>

<img src='data:image/png;base64,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'
style='width:30px;height:30px;'>  Tours<br/>
"

m <- leaflet(data = brew) %>% setView(lng = -87.7, lat = 41.8, zoom = 10) %>% addTiles %>%
  addMarkers(~lon, ~lat, icon = ~icons[type], label = ~htmlEscape(names)) %>%
  addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(minZoom = 6, maxZoom = 20)) %>%
  addControl(html = html_legend, position = "bottomright") %>%
  addMeasure(
    position = "topright",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")

library(htmlwidgets)
saveWidget(m, file="brewery2.html", selfcontained=TRUE)

brewv4 <- na.omit(brew)