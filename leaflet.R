install.packages("leaflet")
library(leaflet)
install.packages("esquisse")
library(esquisse)

esquisse::esquisser()



# leaflet() %>% 
#   addProviderTiles("Esri") %>%
#   addMarkers(lng = data_subset$longitude, lat = data_subset$latitude) # or addPopups()
# 
# names(providers)
# 
# df2 <- 
#   tibble(
#     latt = dfRemain$latitude,
#     lon = dfRemain$longitude
#   )
# 
# leaflet() %>% 
#   addProviderTiles("CartoDB") %>% 
#   addMarkers(lng = df2$lon, lat = df2$latt)


leaflet() %>% 
  addProviderTiles("CartoDB") %>%
  #addTiles()
  #setting default map view
  setView(lng = -73, lat = 40, zoom = 13)
names(providers)

leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  addPopups()
  setView(lng = -73.98575, lat = 40.74856, zoom = 6)

leaflet(options = 
          leafletOptions(minZoom = 14, dragging = FALSE))  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = -73.98575, lat = 40.74856, zoom = 14)


leaflet(options = leafletOptions(
  # Set minZoom and dragging 
  minZoom = 12, dragging = TRUE))  %>% 
  addProviderTiles("CartoDB")  %>% 
  
  # Set default zoom level 
  setView(lng = dc_hq$lon[2], lat = dc_hq$lat[2], zoom = 14) %>% 
  
  # Set max bounds of map 
  setMaxBounds(lng1 = dc_hq$lon[2] + .05, 
               lat1 = dc_hq$lat[2] + .05, 
               lng2 = dc_hq$lon[2] - .05, 
               lat2 = dc_hq$lat[2] - .05) 

df <- 
  tibble(
    hq = c("").
    lat = c(0,0,0),
    lng = c(0,0,0)
  )

leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  addMarkers(lng = -73.98575, lat = 40.74856)

# Plot DataCamp's NYC HQ with zoom of 12    
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(lng = -73.98575, lat = 40.74856)  %>% 
  setView(lng = -73.98575, lat = 40.74856, zoom = 12)    

# Plot both DataCamp's NYC and Belgium locations
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat)

# Store leaflet hq map in an object called map
map <- leaflet() %>%
  addProviderTiles("CartoDB") %>%
  # Use dc_hq to add the hq column as popups
  addMarkers(lng = dc_hq$lon, lat = dc_hq$lat,
             popup = dc_hq$hq)

# Center the view of map on the Belgium HQ with a zoom of 5 
map_zoom <- map %>%
  setView(lat = 50.881363, lng = 4.717863,
          zoom = 5)

# Print map_zoom
map_zoom

