# source
source("./Code/Auxilliary.R")

# packages
get.package(c("ggplot2"))

# data
df <- readRDS("./Data/China_Sourced/rds/dat_long.rds")

# class conversion
df[, 3:ncol(df)] <- lapply(df[, 3:ncol(df)], as.numeric)

# local morans I in 2021
df_2019 <- df[df[, "year"] == 2019, ]

# W Matrix KNN
coords <- st_coordinates(st_centroid(df_2019$Geom))
k.near <- spdep::knearneigh(coords, k = 3)
k5 <- spdep::knn2nb(k.near)
Wlist <- spdep::nb2listw(k5, style = "W")

# morans I
moran <- spdep::localmoran(df_2019[, "Health_Care_Expenditures"], 
                           listw = Wlist, alternative = "two.sided")

# type I error correction
moran[, 5] <- p.adjust(moran[, 5], method = "bonferroni")

# merge
moran <- data.frame(moran)
moran$Region <- df_2019$Region
plot_df <- left_join(df_2019, moran)
plot_df_sf <- st_as_sf(plot_df)

# plot
ggplot(data = plot_df_sf) +
  geom_sf(aes(fill = Ii), color = "black") +
  ggtitle("Local Moran's I", subtitle = "Health Care Expenditure 2019") +
  scale_fill_viridis_c(direction = -1, name = "Local Moran's I") +
  theme_void()
