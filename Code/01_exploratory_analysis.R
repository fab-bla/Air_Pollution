# source
source("./Code/Auxilliary.R")

# packages
get.package(c("ggplot2", "gganimate", "tidyr", 
              "spdep", "dplyr", "gifski", "SDPDmod"))

# data
df <- readRDS("./Data/China_Sourced/rds/dat_long.rds")

# class conversion
df[, 3:ncol(df)] <- lapply(df[, 3:ncol(df)], as.numeric)

# cutoff before corona and remove early yeas because of NAs
df_cut_years <- df[df$year >= 2011 & df$year < 2020, ]

# consolidaat eudsrt and particulate matter
df_cut_years$Waste_Gas_Emissions_Particular_Matter[df_cut_years$year < 2016] <- df_cut_years$Waste_Gas_Emissions_Smoke_and_Dust[df_cut_years$year < 2016]

# write corrected dataset
# saveRDS(df_cut_years, "./Data/China_Sourced/rds/df_cut_years.rds")

# split 
df_split_year <- split(df_cut_years, df_cut_years$year)

# loop over dataset split by year
lapply(df_split_year, \(df_year){

  # W Matrix KNN
  # coords <- st_coordinates(st_centroid(df_year$Geom))
  # k.near <- spdep::knearneigh(coords, k = 3)
  # k3 <- spdep::knn2nb(k.near)
  # Wlist <- spdep::nb2listw(k3, style = "W")
  
  # W matrix inverse distance
  coords <- st_coordinates(st_centroid(df_year$Geom))
  k1 <- knn2nb(knearneigh(coords))
  critical.threshold <- max(unlist(nbdists(k1, coords)))
  nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
  distances <- nbdists(nb.dist.band,coords)
  invd1 <- lapply(distances, \(x) (1 / x))
  Wlist <- nb2listw(nb.dist.band, glist = invd1, style = "B")

  # apply over cols of interest, i.e., Expenditure and emission variables
  lapply(c("Health_Care_Expenditures", "Waste_Gas_Emissions_Sulphur",
           "Waste_Gas_Emissions_Particular_Matter",
           "Waste_Gas_Emissions_Nitrogen"), \(input_var){
             
             # morans I
             moran <- spdep::localmoran(df_year[, input_var], 
                                        listw = Wlist, alternative = "two.sided")
             
             # type I error correction
             moran[, 5] <- p.adjust(moran[, 5], method = "bonferroni")
             
             # merge
             moran <- data.frame(moran)
             moran$Region <- df_year$Region
        
             # return local Morans I and Region
             res <- moran[, c("Ii", "Pr.z....E.Ii..", "Region")]
             colnames(res)[1:2] <- paste0(c("Ii_", "Pv_"), input_var)
             return(res)
            
           }) -> morans_across_vars
  
  # reduce to one df 
  Iis <- Reduce(\(x, y) merge(x, y, all = TRUE), morans_across_vars)

  # return df 
  left_join(df_year, Iis)
  
}) -> lst_moran_res_by_year

# link
df_incl_moran <- do.call(rbind, lst_moran_res_by_year)
df_sf_moran <- st_as_sf(df_incl_moran)

# plot
# ggplot(data = plot_df_sf) +
#   geom_sf(aes(fill = Ii), color = "black") +
#   ggtitle("Local Moran's I", subtitle = "Health Care Expenditure 2019") +
#   scale_fill_viridis_c(direction = -1, name = "Local Moran's I") +
#   theme_void()

# ggsave 
# ggsave("./Data/China_Sourced/plots/moran_2019.pdf")

# plot
# ggplot(data = plot_df_sf) +
#   geom_sf(aes(fill = Health_Care_Expenditures), color = "black") +
#   ggtitle("Health Care Expenditure", subtitle = "in millions of Yuan") +
#   scale_fill_viridis_c(direction = -1, name = "Expenditure", limits = c(20, 1620)) +
#   theme_void()

# ggsave 
# ggsave("./Data/China_Sourced/plots/HCE_2019.pdf")

## gganimate variables and their spatial interaction ##

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

# Gifs of timeseries as well as gifs of Moran's I with KNN #
dests <- paste0("./Data/China_Sourced/gifs/", c("HC_exp.gif", "sulphur.gif",
                                                "part_matter.gif", 
                                                "nitrogen.gif", "HC_exp_MI.gif",
                                                "sulphur_MI.gif", "part_matter_MI.gif", 
                                                "nitrogen_MI.gif"))
titles_legend <- c("Expenditure in\n100MM of Yuan", 
                   "Sulphur Dioxide\nin 10K of Tons",
                   "Particulate Matter\nin 10K of Tons",
                   "Nitrogen Oxides\nin 10K of Tons",
                   rep("Local Moran's I\nInverse Distance", 4))

titles_main <- paste0(c("Health Care Expenditure in:",
                        "Sulphur Dioxide Emissions in:",
                        "Particulate Matter Emissions in:",
                        "Nitrogen Oxide Emissions in:",
                        paste0("Local Moran's I for ", c("Health Care Expenditure",
                                                         "Sulphur Dioxide",
                                                         "Particulate Matter", 
                                                         "Nitrogen Oxides"), " in:")), " {closest_state}.")

var <- c("Health_Care_Expenditures", "Waste_Gas_Emissions_Sulphur",
         "Waste_Gas_Emissions_Particular_Matter",
         "Waste_Gas_Emissions_Nitrogen")

var <- c(var, paste0("Ii_", var))

# map over vars
Map(map_gif, list(df_sf_moran), var, titles_main, titles_legend, dests)

################################INVERSE DISTANCE###############################################
dests <- paste0("./Data/China_Sourced/gifs/", c("HC_exp_MI_ID.gif",
                                                "sulphur_MI_ID.gif", "part_matter_MI_ID.gif", 
                                                "nitrogen_MI_ID.gif"))

titles_legend <- c(rep("Local Moran's I\nInverse Distance", 4))

titles_main <- paste0(c(paste0("Local Moran's I for ", c("Health Care Expenditure",
                                                         "Sulphur Dioxide",
                                                         "Particulate Matter", 
                                                         "Nitrogen Oxides"), " in:")), " {closest_state}.")

var <- c("Health_Care_Expenditures", "Waste_Gas_Emissions_Sulphur",
         "Waste_Gas_Emissions_Particular_Matter",
         "Waste_Gas_Emissions_Nitrogen")

var <- paste0("Ii_", var)

# map over vars
Map(map_gif, list(df_sf_moran), var, titles_main, titles_legend, dests)
