rm(list=ls())

# source
#setwd("C:/Users/marti/Desktop/WU/Master/Spatial Economics/R&P/Research Paper/Github/Air_Pollution")
source("./Code/Auxilliary.R")

# packages
get.package(c("splm", "spatialreg", "Ecdat", "spdep"))

# data
df <- readRDS("./Data/China_Sourced/rds/df_cut_yeats_alt_W.rds")
ind <- df[, "Region"] != "Hainan"
df <- df[ind, ]

# W matrix queen contiguity
queen_nb <- readRDS("./Data/China_Sourced/rds/queen_nb.rds")
Wlist <- spdep::nb2listw(queen_nb, style = "W")

# model df without geom
df$Geom_alt <- NULL 

# pdataframe
df_panel <- plm::pdata.frame(df, index = c("Region", "year"))

# df rel vars
df_lag <- df_panel[, c("Waste_Gas_Emissions_Nitrogen",  "Waste_Gas_Emissions_Particular_Matter",  
                       "Waste_Gas_Emissions_Sulphur", "Disposable_Income_per_Capita_Rural", "Disposable_Income_per_Capita_Urban")]

#  apply over rel. vars
lapply(as.list(df_lag, keep.attributes = TRUE), \(col){
         
         # get lagged version 
         splm::slag(col, Wlist)
         
         
       }) |> setNames(paste0(colnames(df_lag), "_lag")) -> lagged_vars

# add to pdataframe
df_panel <- cbind(df_panel, lagged_vars) |> plm::pdata.frame(index = c("Region", "year"))

# build indices
!colnames(df_panel) %in% c("Region", "Geom", "year", "Number_of_Beds_in_Health_Care_Institutions",
                     "Number_of_Beds_in_Hospitals", "Number_of_Health_Care_Institutions", "Number_of_Medical_Personell",
                     "Sample_population_of_age_15_65", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
                     "Health_Care_Expenditures", "Consumer_Price_Index", "Disposable_Income_per_Capita",
                     "Gross_regional_product", "Total_Gov._Expenditure") -> incl_ind
# NAs
sapply(df_panel, \(cols) complete.cases(cols) |> all()) -> miss_ind
df_panel[, "Population_affected_by_Naural_Desasters"] |> is.na() -> ind

# build formula
fml <- paste("Health_Care_Expenditures ~", paste(colnames(df_panel)[incl_ind & miss_ind], collapse = " + ")) |> as.formula()

# first test
slmlag <- splm::slmtest(fml, data = df_panel, listw = Wlist, test = "lme", model = "within")

# panel SLX
panel_SLX <- plm::plm(fml, data = df_panel, effect = "individual", model = "within") #individual because we include the effect of the provinces in the model after demeaning over time https://www.wu.ac.at/fileadmin/wu/d/i/iqv/Gstach/Artikel/Croissant__2008_.pdf


# summary
panel_SLX |> summary()

# residuen
panel_SLX$residuals |> plot()

  
plm::pdwtest(panel_SLX)#test for serial correlation #significant
lmtest::bptest(panel_SLX)#Breusch-Pagan for heteroscedasticity
# spatial error | fixed effects 
  # yes
    # serial correlation
      # y/n


#serial correlation robust errors
#VCV
rVCV<-plm::vcovHC(panel_SLX, method = "arellano")

A<-summary(panel_SLX, vcov. = vcovHC(panel_SLX,method="arellano")) #seems like R wants robust VCV in the summary or coeftest function
A$residuals |> plot() #seems like they are the same??
