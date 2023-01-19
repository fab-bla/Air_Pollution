# source
source("./Code/Auxilliary.R")

# packages
get.package(c("splm", "spatialreg", "Ecdat", "spdep"))

# data
df <- readRDS("./Data/China_Sourced/rds/df_cut_yeats_alt_W.rds")
ind <- df[, "Region"] != "Hainan"
df <- df[ind, ]

# W matrix (inverse distance)
# coords <- st_coordinates(st_centroid(df[df$year == 2011, ]$Geom))
# k1 <- knn2nb(knearneigh(coords))
# critical.threshold <- max(unlist(nbdists(k1, coords)))
# nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
# distances <- nbdists(nb.dist.band,coords)
# invd1 <- lapply(distances, \(x) (1 / x))
# Wlist <- nb2listw(nb.dist.band, glist = invd1, style = "B")

# W matrix KNN
# coords <- st_coordinates(st_centroid(df[df$year == 2011, ]$Geom))
# k.near <- spdep::knearneigh(coords, k = 4)
# k3 <- spdep::knn2nb(k.near)
# Wlist <- spdep::nb2listw(k3, style = "W")

# W matrix queen contiguity
queen_nb <- readRDS("./Data/China_Sourced/rds/queen_nb.rds")
Wlist <- spdep::nb2listw(queen_nb, style = "W")

# model df without geom
df$Geom_alt <- NULL 

# build indices
!colnames(df) %in% c("Region", "Geom", "year", "Number_of_Beds_in_Health_Care_Institutions",
                           "Number_of_Beds_in_Hospitals", "Number_of_Health_Care_Institutions", "Number_of_Medical_Personell",
                           "Sample_population_of_age_15_65", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
                           "Health_Care_Expenditures") -> incl_ind
# NAs
sapply(df, \(cols) complete.cases(cols) |> all()) -> miss_ind
df[, "Population_affected_by_Naural_Desasters"] |> is.na() -> ind

# build formula
fml <- paste("Health_Care_Expenditures ~", paste(colnames(df)[incl_ind & miss_ind], collapse = " + ")) |> as.formula()

# first test
slmlag <- splm::slmtest(fml, data = df, listw = Wlist, test = "lml")


