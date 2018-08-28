rm(list=ls())
library(leaflet)

hopkinsIcon <- makeIcon(
  iconUrl = "https://brand.jhu.edu/assets/uploads/sites/5/2014/06/university_logo_small_vertical_blue.png",
  iconWidth = 31*215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

hopkinsLatLong <- data.frame(
  lat = c(39.2973166, 39.3288851, 39.2906617, 39.2970681, 39.2824806),
  lng = c(-76.5929798, -76.6206598, -76.5469683, -76.6150537, -76.6016766))

hopkinsLatLong %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = hopkinsIcon)
  
# adding multiple pop ups
hopkinsSites = c(
  "<a href='http://www.jhsph.edu/'>East Baltimore Campus</a>",
  "<a href='https://apply.jhu.edu/visit/homewood/'>Homewood Campus</a>",
  "<a href='http://www.hopkinsmedicine.org/johns_hopkins_bayview/'>Bayview Medical Center</a>",
  "<a href='http://www.peabody.jhu.edu/'>Peabody Institute</a>",
  "<a href='http://carey.jhu.edu/'>Carey Business School</a>"
)

hopkinsLatLong %>% 
  leaflet() %>%
  addTiles() %>% 
  addMarkers(icon = hopkinsIcon, popup = hopkinsSites)

