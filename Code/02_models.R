# source
source("./Code/Auxilliary.R")
# packages
get.package(c("splm", "spatialreg", "Ecdat", "spdep", "lmtest"))

# data
df <- readRDS("./Data/China_Sourced/rds/df_cut_yeats_alt_W.rds")
ind <- df[, "Region"] != "Hainan"
df <- df[ind, ]
#Inflation Adjustment
#Divide monetary variable by CPI/100 (because changes are in hundreds)
colnames(df) %in% c("Disposable_Income_per_Capita", "Disposable_Income_per_Capita_Rural","Disposable_Income_per_Capita_Urban",
                    "Gross_regional_product","Health_Care_Expenditures", "Total_Gov._Expenditure") -> num.var.index

df[num.var.index] <- lapply(df[num.var.index],\(num.var){num.var / (df$Consumer_Price_Index / 100)})

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
                     "Sample_population_of_age_0_14", "Sample_population_of_age_65_and_older", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
                     "Health_Care_Expenditures", "Consumer_Price_Index", "Disposable_Income_per_Capita",
                     "Gross_regional_product", "Total_Gov._Expenditure", "Disposable_Income_per_Capita_Rural_lag", "Disposable_Income_per_Capita_Rural", "Forest_Coverage_Rate",
                     "Rural_Population") -> incl_ind

# incl ind with controls
!colnames(df_panel) %in% c("Region", "Geom", "year", "Number_of_Beds_in_Health_Care_Institutions",
                           "Number_of_Beds_in_Hospitals", "Number_of_Health_Care_Institutions", "Number_of_Medical_Personell",
                           "Sample_population_of_age_0_14", "Sample_population_of_age_65_and_older", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
                           "Health_Care_Expenditures", "Consumer_Price_Index", "Disposable_Income_per_Capita",
                           "Gross_regional_product", "Total_Gov._Expenditure", "Rural_Population", "Sample_population_of_age_15_65", "Disposable_Income_per_Capita_Rural", 
                           "Disposable_Income_per_Capita_Rural_lag", "Disposable_Income_per_Capita_Urban_lag") -> incl_ind

# build indices
# !colnames(df_panel) %in% c("Region", "Geom", "year", "Number_of_Beds_in_Health_Care_Institutions",
#                            "Number_of_Beds_in_Hospitals", "Number_of_Health_Care_Institutions", "Number_of_Medical_Personell",
#                            "Sample_population_of_age_0_14", "Sample_population_of_age_65_and_older", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
#                            "Health_Care_Expenditures", "Consumer_Price_Index", "Disposable_Income_per_Capita",
#                            "Gross_regional_product", "Total_Gov._Expenditure", "Waste_Gas_Emissions_Nitrogen_lag", "Waste_Gas_Emissions_Particular_Matter_lag", 
#                            "Waste_Gas_Emissions_Sulphur_lag", "Disposable_Income_per_Capita_Rural_lag", "Disposable_Income_per_Capita_Urban_lag", "Sample_population_of_age_15_65", "Disposable_Income_per_Capita_Urban", "Rural_Population") -> incl_ind

# incl ind with controls
# !colnames(df_panel) %in% c("Region", "Geom", "year", "Number_of_Beds_in_Health_Care_Institutions",
#                            "Number_of_Beds_in_Hospitals", "Number_of_Health_Care_Institutions", "Number_of_Medical_Personell",
#                            "Sample_population_of_age_0_14", "Sample_population_of_age_65_and_older", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
#                            "Health_Care_Expenditures", "Consumer_Price_Index", "Disposable_Income_per_Capita",
#                            "Gross_regional_product", "Total_Gov._Expenditure", "Rural_Population", "Sample_population_of_age_15_65", "Disposable_Income_per_Capita_Rural",
#                            "Disposable_Income_per_Capita_Rural_lag", "Disposable_Income_per_Capita_Urban_lag", "Waste_Gas_Emissions_Nitrogen", "Waste_Gas_Emissions_Nitrogen_lag",
#                            "Waste_Gas_Emissions_Particular_Matter", "Waste_Gas_Emissions_Particular_Matter_lag") -> incl_ind

# incl ind with controls
# !colnames(df_panel) %in% c("Region", "Geom", "year", "Number_of_Beds_in_Health_Care_Institutions",
#                            "Number_of_Beds_in_Hospitals", "Number_of_Health_Care_Institutions", "Number_of_Medical_Personell",
#                            "Sample_population_of_age_0_14", "Sample_population_of_age_65_and_older", "Waste_Gas_Emissions_Smoke_and_Dust", "CPI_Health_Care",
#                            "Health_Care_Expenditures", "Consumer_Price_Index", "Disposable_Income_per_Capita",
#                            "Gross_regional_product", "Total_Gov._Expenditure", "Waste_Gas_Emissions_Nitrogen", "Waste_Gas_Emissions_Sulphur", "Waste_Gas_Emissions_Nitrogen_lag",
#                            "Waste_Gas_Emissions_Sulphur_lag", "Disposable_Income_per_Capita_Rural", "Forest_Coverage_Rate", "Disposable_Income_per_Capita_Rural_lag") -> incl_ind

# NAs
sapply(df_panel, \(cols) complete.cases(cols) |> all()) -> miss_ind
df_panel[, "Population_affected_by_Naural_Desasters"] |> is.na() -> ind

# build formula
fml <- paste("Health_Care_Expenditures ~", paste(colnames(df_panel)[incl_ind & miss_ind], collapse = " + ")) |> as.formula()

# first test

# panel SLX and SDE
panel_SLX <- plm::plm(fml, data = df_panel, effect = "individual", model = "within") #individual because we include the effect of the provinces in the model after demeaning over time https://www.wu.ac.at/fileadmin/wu/d/i/iqv/Gstach/Artikel/Croissant__2008_.pdf
panel_SDE <- splm::spml(fml, data = df_panel,  effect = "individual", model = "within", lag = FALSE, spatial.error = "kkp", listw = Wlist)
# summary
panel_SLX |> summary()
panel_SDE |> summary()

# residuen
panel_SLX$residuals |> plot()
panel_SDE$residuals |> plot()

#BP and PDW
plm::pdwtest(panel_SLX) # test for serial correlation #significant

lmtest::bptest(panel_SLX) # Breusch-Pagan for heteroscedasticity

#VCV
rVCV <- plm::vcovHC(panel_SLX, method = "arellano")

# coeftest
corrected_res <- lmtest::coeftest(x = panel_SLX, vcov = rVCV)

# lmtest::coeftest(panel_SDE)
corrected_res



######################## variable transformation ##############################

# formula 
fml_string_transform  <- colnames(df_panel)[incl_ind & miss_ind]

# vars not to log
ind_sample_pop <- grepl("15_65", fml_string_transform, fixed = TRUE)

# pick out all vars to log transform 
fml_log_inputs <- paste0("log(", fml_string_transform[!ind_sample_pop], ")")

# final log transformed formula
fml_log_transformed <- paste("log(Health_Care_Expenditures) ~", paste(fml_string_transform[ind_sample_pop], paste(fml_log_inputs, collapse = " + "), sep = " + ")) |> as.formula()
 
# fit with new model spec
panel_SLX_log <- plm::plm(fml_log_transformed , data = df_panel, effect = "individual", model = "within") 

# residual plot
panel_SLX_log$residuals |> plot()

# BP and PDW
plm::pdwtest(panel_SLX_log) # test for serial correlation #significant
lmtest::bptest(panel_SLX_log)

#VCV
rVCV_log <- plm::vcovHC(panel_SLX_log, method = "arellano")

# coeftest
corrected_res_log <- lmtest::coeftest(x = panel_SLX_log, vcov = rVCV_log)

# return
corrected_res_log

# export table
stargazer::stargazer(corrected_res_log)


