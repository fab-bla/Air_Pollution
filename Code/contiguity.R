library("sf")
library("spdep")
geom_new <- st_read("../Data/China_Sourced/shp")

plot(st_geometry(geom_new))

NB<-poly2nb(geom_new, queen=TRUE)
listw_C<-nb2listw(NB, style="W", zero.policy = TRUE) #enable zero.policy in order to allow hainan to be an island (no neighbors)
