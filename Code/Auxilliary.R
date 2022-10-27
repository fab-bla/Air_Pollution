## Aux ##

# packages
get.package <- function(package){
  
  lapply(package, function(x){
    if(!require(x, character.only = T)){
      install.packages(x)
    }
    library(x, character.only = T)     
  })
  
}

# invis plot
invis.Map <- function(f, ...) invisible(Map(f, ...))
invis.lapply <- function(x, f) invisible(lapply(x, f))
invis.rapply <- function(object, f, classes = "ANY", deflt = NULL, 
                         how = c("unlist", "replace", "list"), ...){
                         invisible(rapply(object, f, classes = "ANY", deflt = NULL,
                         how = c("unlist", "replace", "list"), ...))}



# density and histogram 
Dens_hist_plot <- \(data = dat_bids, y, dist = "gamma", distFUN = dgamma, bg_alt = FALSE, lower_b = NULL, bins = 30){
  
  # Estimate paramerters via MLE
  dis_MLE <- MASS::fitdistr(data[[y]], densfun = dist, lower = lower_b)
  
  # init. plot
  ggplot(data, aes(.data[[y]])) +
    
    # hist
    geom_histogram(aes(y = ..density..), fill = "cornflowerblue", col = "deepskyblue4",
                   bins = bins) +
    
    # fit normal
    stat_function(fun = distFUN, 
                  args = as.list(dis_MLE$estimate),
                  col = "darkblue", lwd = 1, lty = "dashed") +
    theme_bw() +
    
    # add colored grid optionally
    {if(bg_alt) theme(panel.background = element_rect(fill = "#90EE90",
                                                      size = 2, linetype = "solid"),
                      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                      colour = "white"), 
                      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                      colour = "white"))}
  
}



