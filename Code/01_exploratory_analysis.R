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

# morans across years
df_cut_years <- df[df$year > 2006 & df$year < 2020, ]
df_split_year <- split(df_cut_years, df_cut_years$year)

lapply(df_split_year, \(df_year){

  # W Matrix KNN
  coords <- st_coordinates(st_centroid(df_year$Geom))
  k.near <- spdep::knearneigh(coords, k = 3)
  k3 <- spdep::knn2nb(k.near)
  Wlist <- spdep::nb2listw(k3, style = "W")

  # morans I
  moran <- spdep::localmoran(df_year[, "Health_Care_Expenditures"], 
                             listw = Wlist, alternative = "two.sided")
  
  # type I error correction
  moran[, 5] <- p.adjust(moran[, 5], method = "bonferroni")
  
  # merge
  moran <- data.frame(moran)
  moran$Region <- df_year$Region
 
  # return df 
  left_join(df_year, moran)
  
}) -> lst_moran_res_by_year

# link
df_incl_moran <- do.call(rbind, lst_moran_res_by_year)
df_sf_moran <- st_as_sf(df_incl_moran)

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

# gifs 
map_gif <- \(data_inp, var_str, title_main, title_legend, dest){

  # min and max for 
  mins <-  data_inp |> as.data.frame() |> select(all_of(var_str)) |> min(na.rm = TRUE)
  maxs <- data_inp |> as.data.frame() |> select(all_of(var_str)) |> max(na.rm = TRUE)
  
  # plot
  ggplot(data_inp) +
    geom_sf(aes(fill = .data[[var_str]]), color = "black") +
    scale_fill_viridis_c(direction = -1, name = title_legend, 
                         limits = c(mins, maxs)) +
    theme_void() +
    transition_states(year, transition_length = 1, state_length = 30) +
    labs(title = title_main) -> temp_plot
  
  # animate in plot pane 
  gif <- animate(plot = temp_plot,
                 fps = 15,
                 renderer = gifski_renderer(loop = T),
                 height = 4,
                 width = 6.1, units = "in", res = 200, nframes = 200)
  
  # save
  anim_save(dest, gif)
  cat("Saved gif!")
}


# map input vectors
dests <- paste0("./Data/China_Sourced/gifs/", c("HC_exp.gif", "sulphur.gif",
                                                "part_matter.gif", "smoke_dust.gif"))
titles_legend <- c("Expenditure in\n100MM of Yuan", 
                 "Sulphur Emissions\nin 10K of Tons",
                 "Particular Matter\nin 10K of Tons",
                 "Smoke and Dust\nin 10K of Tons")
titles_main <- paste0(c("Health Care Expenditure in:",
                        "Sulphur Dioxide Emissions in:",
                        "Particular Matter Emissions in:",
                        "Smoke and Dust Emissions in:"), " {closest_state}.")
var <- c("Health_Care_Expenditures", "Waste_Gas_Emissions_Sulphur",
         "Waste_Gas_Emissions_Particular_Matter", "Waste_Gas_Emissions_Smoke_and_Dust")

# map over vars
Map(map_gif, list(df_sf), var, titles_main, titles_legend, dests)

# morans I plot
map_gif(df_sf_moran, "Ii", "Local Moran's I in: {closest_state}.",
        "Local Moran's I\nKNN, n = 3", "./Data/China_Sourced/gifs/local_moran.gif")

