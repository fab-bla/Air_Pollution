# source 
source("./Code/Auxilliary.R")

# import 
dat_WHO <- openxlsx::read.xlsx("./Data/WHO_Pollution_Data.xlsx", sheet = 2)
cities <- read.csv("./Data/chinese_cities.csv")
cities <- cities[, c("city", "admin_name")]
cities <- lapply(cities, tolower) |> as.data.frame()

#  packages
get.package(c("sf", "osmextract", "fuzzyjoin", "dplyr"))

# load chinese polygon coords
poly_china <- openstreetmap_fr_zones[which(openstreetmap_fr_zones$parent == "china"), ]

# subset chinese data from the WHO database
dat_China <- dat_WHO[dat_WHO[, "WHO.Country.Name"] == "China", ]
dat_China[, "City.or.Locality"] <- tolower(dat_China[, "City.or.Locality"])
colnames(dat_China)[4] <- "city"

# look at city or locality 
unique(dat_China[, "City.or.Locality"])
unique(cities[, "admin_name"])

# check 
tmp <- unique(dat_China[, "City.or.Locality"]) %in% cities[, "city"] # looks bad

# try stringdist join
stringdist_join(dat_China, cities, 
                by = "city", 
                mode = "inner", 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") -> join 
 
