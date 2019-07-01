
#****************************************************************************************************
#                globals ####
#****************************************************************************************************
globals <- list()
globals <- within(globals,{
  endyear <- 50 # starting from year 1
  burnyears <- 11 # the burn-in period before we get to year 1
  totyears <- endyear + burnyears
  years <- (-burnyears+1):endyear
  iyears <- 1:totyears; names(iyears) <- years
  css <- c("CA", "FL", "MA", "NH", "OH") # case-study states
})
# globals
# globals$rcdir <- "data"
# globals$rcfn <- "RunControlPropTax(7).xlsx"
# globals$runcontrol <- file.path(PROJHOME, globals$rcdir, globals$rcfn)
# globals$css <- c("CA", "FL", "MA", "NH", "AZ", "NY") # case-study states
