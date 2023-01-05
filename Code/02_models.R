# source
source("./Code/Auxilliary.R")

# packages
get.package(c("splm", "spatialreg", "Ecdat"))

# data
df <- readRDS("./Data/China_Sourced/rds/df_cut_years.rds")

# W matrix (inverse distance)
coords <- st_coordinates(st_centroid(df[df$year == 2011, ]$Geom))
k1 <- knn2nb(knearneigh(coords))
critical.threshold <- max(unlist(nbdists(k1, coords)))
nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
distances <- nbdists(nb.dist.band,coords)
invd1 <- lapply(distances, \(x) (1 / x))
Wlist <- nb2listw(nb.dist.band, glist = invd1, style = "B")

# W matrix KNN
coords <- st_coordinates(st_centroid(df[df$year == 2011, ]$Geom))
k.near <- spdep::knearneigh(coords, k = 3)
k3 <- spdep::knn2nb(k.near)
Wlist <- spdep::nb2listw(k3, style = "W")

# build formula
!colnames(df) %in% c("Region", "Geom", "year", "Number_of_Beds_in_Health_Care_Institutions",
                     "Number_of_Beds_in_Hospitals", "Number_of_Health_Care_Institutions", "Number_of_Medical_Personell",
                     "Sample_population_of_age_15_65", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
                     "Health_Care_Expenditures") -> excl_ind

fml <- paste("Health_Care_Expenditures ~", paste(colnames(df)[excl_ind], collapse = " + ")) |> as.formula()

# Is there a spatial Lag structure?
df2 <- df[, -2] # tried remopving geom as error was received
df2[, "Region"] <- as.factor(df2[, "Region"]) # tried classes for region

# first test
slmlag <- slmtest(fml, data = df2, listw = Wlist, test = "lml", model = "within")


