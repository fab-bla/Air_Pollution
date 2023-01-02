# source 
source("./Code/Auxilliary.R")

#  packages
get.package(c("sf", "osmextract", "dplyr", "tidyr",
              "maps", "mapdata"))

# load chinese polygon coords
poly_china <- openstreetmap_fr_zones[which(openstreetmap_fr_zones$parent == "china"), ]
                            
# files to join 
files_l <- list.files("./Data/China_sourced/csv", full.names = TRUE, pattern = ".csv")
names <- list.files("./Data/China_sourced/csv", pattern = ".csv")

# get names from filenames
var_names <- gsub(" ", "_", stringr::str_remove(names, ".csv") |> trimws())

# read files 
Map(\(file, name){
 
  # read csv
  tmp <- read.csv2(file)
  
  # correct colnames
  years <- stringr::str_remove(colnames(tmp), "X")
  name_tmp <- paste0(name, "-", years)
  colnames(tmp) <- c("Region", name_tmp[-1])
  
  # return 
  return(tmp)
  
}, files_l, var_names) |> setNames(var_names) -> input_csv

# merge on Region
df <- Reduce(\(x, y) merge(x, y, all = TRUE), input_csv)

# compare region names 
bool <- poly_china$name %in% df[, "Region"]
poly_china$name[!bool] # Hong Kong and Macao are not part of the china polygon -> exclude
poly_china$name

# wide to long 
df %>%
  gather(key, value, -Region) %>%
  separate(key, into = c("var", "year"), sep = "-") %>%
  spread(var, value) -> df_long

# add geometry: left join into polytgon dataset
tmp_join <- poly_china[, c("name", "geometry")]
colnames(tmp_join) <- c("Region", "Geom")

# merge left
df_fin <- merge(as.data.frame(tmp_join), df_long, all = FALSE)

# write final data set 
# saveRDS(df_fin, "./Data/China_Sourced/rds/dat_long.rds")

