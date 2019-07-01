

#****************************************************************************************************
#                general ####
#****************************************************************************************************
ns <- function(df) {names(df) %>% sort}

#****************************************************************************************************
#                functions needed to run the model ####
#****************************************************************************************************
insert <- function(subvec, vec, start=(globals$burnyears + 1)){
  indexes <- start:(start + length(subvec) - 1)
  vec[indexes] <- subvec
  return(vec)
}


make_vec <- function(value) {
  vec <- rep(value, globals$totyears)
  names(vec) <- globals$years
  return(vec)
}
# make_vec(.06)

create_cycle <- function(clength, cstart, zbefore=NULL){
  # returns a named vector with 1 in the years in which the cycle event is occurring
  # clength is the length of the cycle -- e.g., 3 means reassess every 3 years
  # cstart is the first year > 0 we'd like to see the cycle in
  # if burn==FALSE, zero-out the burn years
  cycle <- make_vec(0)
  icycle <- which((globals$years - cstart) %% clength == 0) # starts at 0 when cstart=0
  cycle[icycle] <- 1
  if(hasArg(zbefore)) cycle[which(globals$years < zbefore)] <- 0
  return(cycle)
}
# create_cycle(7, 40)
# create_cycle(7, 5, zbefore=5)

