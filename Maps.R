setwd('specify directory')

# packages useful
library(leaflet);library(sp);library(maptools)
library(tidyr);library(rgdal)
library(RgoogleMaps);library(rjson)
library(ggmap)


# Option 1 : gécoder des adresses
Clt <- read.table("fichier",header=TRUE,sep=";",as.is=TRUE,quote="\"")

Clt$Numero <- as.character(Clt$Numero)
Clt$Numero[which(is.na(Clt$Numero))] <- ""
Clt$Adresse <- paste(Clt$Numero,Clt$Voie," Ville")

API_KEY <- "AIzaSyAW6nuM41xCFmW4n9bSSh3CinegoiP8Npw"
Adress2XY <- function(gcStr){ #directement issu de la fonction getGeoCode du package RgoogleMaps
  gcStr <- enc2utf8(gsub(" ", "%20", gcStr))
  connectStr <- paste("https://maps.googleapis.com/maps/api/geocode/json?address=", gcStr,"&key=",API_KEY,sep = "")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse = ""))
  close(con)
  data.json <- unlist(data.json)
  lat <- data.json["results.geometry.location.lat"]
  lng <- data.json["results.geometry.location.lng"]
  gcodes <- as.numeric(c(lat, lng))
  names(gcodes) <- c("lat", "lon")
  return(gcodes)
}

is_na <- which(is.na(Clt$lat))
Adresse <- Clt$Adresse[is_na]
Z <- t(sapply(Adresse,Adress2XY))

Clt$lat[is_na] <- Z[,1]
Clt$lon[is_na] <- Z[,2]

# Option 2 : A partir de fichier .shp

# Lambert 93 <=> EPSG:2154
proj.Lamb93 <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# fonctionne avec Lines/Points/Poly
Asset <- readShapeLines("Fichier shp sans extension",proj4string=CRS(proj.Lamb93))

PROJECTION <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
Data_Asset <- sp::spTransform(Asset,PROJECTION)

Data_Asset$id <- rownames(Data_Asset@data)
shp.df <- as.data.frame(Data_Asset)
data_fort <- fortify(Data_Asset, region = "id")
data_merged <- merge(data_fort, shp.df, by="id")


#### Option A - A partir de ggmap

pdf('Eau France/GE/St Die des Vosges/Results/Maps.pdf',width = 14,height = 9)
m <- qmap("Ville, Pays", zoom = 13, maptype="hybrid")
# ou si cela concerne plusieurs villes
m <- ggmap(get_map(location=c(lon=lon.s,lat=lat.s),maptype="hybrid",zoom=12))
m + geom_line(aes(x=long,y=lat,group=group),data=data_fort,colour="red") +
  geom_point(aes(x=LON, y=LAT), data=Clt, size=1,colour="blue",position=position_jitter(w = 0.0002, h = 0.0002)) +
  theme(legend.text=element_text(size=10),plot.title = element_text(lineheight=.8, face="bold")) +
  guides(colour = guide_legend(nrow=30,override.aes = list(size=5))) + ggtitle("Saclay")


m + geom_line(aes(x=long,y=lat,group=group),data=data_fort,colour="red") +
#   geom_point(aes(x=lon, y=lat), colour="blue", data=Clt , size=1,
#              position=position_jitter(w = 0.0002, h = 0.0002))  +
  geom_density2d(data = Clt , aes(x = lon, y = lat), size = 0.3) +
  stat_density2d(data = Clt,
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01,
                 bins = 44, geom = "polygon") + scale_fill_gradient(low = "purple", high = "blue") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_alpha(range = c(0.1, 0.5), guide = FALSE) +
  ggtitle("Saint Dié des Vosges - Density")
dev.off()


# pdf('Results/GIS/AMRvsNoAMR_maps_byzone.pdf',width = 14,height = 9)
#
# m + geom_point(aes(x=LON, y=LAT, color=ZONA), data=data.clients.georeference %>% filter(TELELECTURA=="S"), size=1,
#                position=position_jitter(w = 0.0002, h = 0.0002)) +
#   theme(legend.text=element_text(size=10),plot.title = element_text(lineheight=.8, face="bold")) +
#   guides(colour = guide_legend(nrow=30,override.aes = list(size=5))) + ggtitle("Con Telelectura")
#
# m + geom_point(aes(x=LON, y=LAT, color=ZONA), data=data.clients.georeference %>% filter(TELELECTURA=="N"), size=1,
#                position=position_jitter(w = 0.0002, h = 0.0002)) +
#   theme(legend.text=element_text(size=10),plot.title = element_text(lineheight=.8, face="bold")) +
#   guides(colour = guide_legend(nrow=30,override.aes = list(size=5))) + ggtitle("Sin Telelectura")
# dev.off()
#
# pdf('Results/GIS/AMR_vs_NoAMR_density_map.pdf')
#
# m +
#   geom_point(aes(x=LON, y=LAT, color="red"), data=data.clients.georeference %>% filter(TELELECTURA=="S"), size=1,
#              position=position_jitter(w = 0.0002, h = 0.0002))  +
#   geom_density2d(data = data.clients.georeference %>% filter(TELELECTURA=="S") , aes(x = LON, y = LAT), size = 0.3) +
#   stat_density2d(data = data.clients.georeference %>% filter(TELELECTURA=="S"),
#                  aes(x = LON, y = LAT, fill = ..level.., alpha = ..level..), size = 0.01,
#                  bins = 44, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
#   theme(plot.title = element_text(lineheight=.8, face="bold")) +
#   scale_alpha(range = c(0.1, 0.5), guide = FALSE) +
#   ggtitle("Con Telelectura")
# m +
#   geom_point(aes(x=LON, y=LAT, color="red"), data=data.clients.georeference %>% filter(TELELECTURA=="N"), size=1,
#              position=position_jitter(w = 0.0002, h = 0.0002))  +
#   geom_density2d(data = data.clients.georeference %>% filter(TELELECTURA=="N") , aes(x = LON, y = LAT), size = 0.3) +
#   stat_density2d(data = data.clients.georeference %>% filter(TELELECTURA=="N"),
#                  aes(x = LON, y = LAT, fill = ..level.., alpha = ..level..), size = 0.01,
#                  bins = 44, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
#   theme(plot.title = element_text(lineheight=.8, face="bold")) +
#   scale_alpha(range = c(0.1, 0.5), guide = FALSE) +
#   ggtitle("Sin Telelectura")
# dev.off()



localisation <- data.frame(site=c("Saint-Dié-des-Vosges","Saclay","La Laguna","Alicante"),lat=c(48.1697579,48.7367033,28.469868,38.3579029),lon=c(4.948819,2.1342479,-16.3433748,-0.5075437))

i <- 4

leaflet() %>%
  # addControl(html="Zoom",position=("topright")) %>%
  addProviderTiles("Esri.WorldImagery",
                   options = providerTileOptions(noWrap = TRUE)
  ) %>%
  addMarkers(lng =localisation$lon[i], lat = localisation$lat[i],layerId = localisation$site[i])
