# source
source("./Code/Auxilliary.R")

# packages
get.package(c("ggplot2", "gganimate", "tidyr", 
              "spdep", "dplyr", "gifski", "gifski"))

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

# ggsave 
# ggsave("./Data/China_Sourced/plots/moran_2019.pdf")

# plot
ggplot(data = plot_df_sf) +
  geom_sf(aes(fill = Health_Care_Expenditures), color = "black") +
  ggtitle("Health Care Expenditure", subtitle = "in millions of Yuan") +
  scale_fill_viridis_c(direction = -1, name = "Expenditure", limits = c(20, 1620)) +
  theme_void()

# ggsave 
# ggsave("./Data/China_Sourced/plots/HCE_2019.pdf")

## gganimate Health care expenditures ##

# to sf
df_sf <- st_as_sf(df)
df_sf <- df_sf[df_sf$year > 2006 & df_sf$year < 2020, ]

# plot HC EXP
ggplot(data = df_sf) +
  geom_sf(aes(fill = Health_Care_Expenditures), color = "black") +
  scale_fill_viridis_c(direction = -1, name = "Expenditure in\nMillions of Yuan", limits = c(20, 1620)) +
  theme_void() +
  transition_states(year, transition_length = 1, state_length = 30) +
  labs(title = "Health Care Expenditure in: {closest_state}.") -> temp_plot

# animate in plot pane 
gif <- animate(plot = temp_plot,
               fps = 15,
               renderer = gifski_renderer(loop = T),
               height = 4,
               width = 6, units = "in", res = 175)

# save
anim_save("./Data/China_Sourced/gifs/HC_exp.gif", gif)

# plot sulphur
mins <- df_sf$Waste_Gas_Emissions_Sulphur |> min()
maxs <- df_sf$Waste_Gas_Emissions_Sulphur |> max()

# plot
ggplot(data = df_sf) +
  geom_sf(aes(fill = Waste_Gas_Emissions_Sulphur), color = "black") +
  scale_fill_viridis_c(direction = -1, name = "Sulphur Emissions\nin Tousands of Tons", 
                       limits = c(mins, maxs)) +
  theme_void() +
  transition_states(year, transition_length = 1, state_length = 30) +
  labs(title = "Sulphur Dioxide Emissions: {closest_state}.") -> temp_plot2

# animate in plot pane 
gif2 <- animate(plot = temp_plot2,
               fps = 15,
               renderer = gifski_renderer(loop = T),
               height = 4,
               width = 6.1, units = "in", res = 200)

# save
anim_save("./Data/China_Sourced/gifs/sulphur.gif", gif2)

df_sf$Waste_Gas_Emissions_Sulphur[df_sf$year == 2019] |> sum()

