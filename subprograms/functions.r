

#****************************************************************************************************
#                functions needed to run the model ####
#****************************************************************************************************

get_reassess.year <- function(year, initial.reassess.year, assess.cycle){
  # construct a logical vector that is true in reassessment years, false otherwise
  reassess.year <- {(year - initial.reassess.year) %% assess.cycle} == 0 # %% gives the remainder
  return(reassess.year)
}


get_mv.known <- function(mv.true, reassess.year){
  # Get the market value upon which the assessment roll will be based
  # The only difference between this market value and the true market value is the assessment lag
  
  # This base value is NOT necessarily the mv shown on the roll. For example, an assessor might not reflect
  # the full change in mv immediately after a drop. But this is the information gathered upon
  # which the mv on the roll will be based
  
  # The code below assumes that in a reassessment year, the new base mv will be last year's mv, and in
  # other years it will simply be carried forward
  
  # if assess.cycle=1, we reassess every year, so mv.known=mv lagged 1 year
  # if assess.cycle=2, we reassess every other year, so we retain the mv for 2 years, etc.
  
  # we have to establish what mv was in the prior years
  # for now assume it was the same as in year 1
  
  # mv.prior <- rep(mv[1], assess.cycle)
  # mv.all <- c(mv.prior, mv)
  # reassess.year <- get_reassess.year(year, initial.reassess.year, assess.cycle)
  
  mv.known <- rep(NA_real_, length(mv.true))  
  mv.known[reassess.year] <- lag(mv.true)[reassess.year]
  mv.known[1] <- mv.true[1] # IMPROVE ON THIS! -- fill initial year of mv.known with the market value from year 1
  for(y in 2:length(mv.known)) if(is.na(mv.known[y])) mv.known[y] <- mv.known[y-1]
  
  return(mv.known)
}


get_mv.on.roll <- function(mv.known, adjust.period, reassess.year){
  # Get the market value as shown on the assessment roll
  # it is based on the mv.known but may be phased in over several years
  # (think about whether this really is realistic assessor behavior)
  
  # in each reassessment year, the assessor looks a the new mv they have mv.known (which is prior year's mv, lag(mv))
  # compares it to last year's mv on the assessment roll, lag(mv.ar)
  # computes how much it has gone up or down, and divides by the adjustment period
  # then adds that to lag(mv.ar) each year until the next 
  # reassessment year, at which point process repeats
  
  # test data
  # year <- 1:12
  # initial.reassess.year.scalar <- 0
  # assess.cycle.scalar <- 1
  # adjust.period.scalar <- 1
  # 
  # initial.reassess.year <- rep(initial.reassess.year.scalar, length(year))
  # assess.cycle <- rep(assess.cycle.scalar, length(year))
  # adjust.period <- rep(adjust.period.scalar, length(year))
  # 
  # reassess.year <- get_reassess.year(year, initial.reassess.year, assess.cycle)
  # mv <- seq(100, 210, 10)
  # mv.known <- get_mv.known(mv, reassess.year) # c(100, 100, rep(110, 3), rep(140, 3), rep(170, 3), 200)
  # cbind(reassess.year, mv, mv.known)
  
  # mv.ar for 1:12, 0, 3, 4should be:
  # 100.0000 100.0000 102.5000 105.0000 107.5000 115.6250 123.7500 131.8750 141.4062 150.9375 160.4688 170.3516
  
  # Easiest way to do this is with a loop; think about whether it can be vectorized
  # print(cbind(reassess.year, adjust.period))
  
  adjust.period <- adjust.period[1]
  mv.on.roll <- rep(NA_real_, length(mv.known))
  first.reassess.year <- min(which(reassess.year == TRUE))
  
  mv.on.roll[1:(first.reassess.year - 1)] <- mv.known[1:(first.reassess.year - 1)]
  
  if(first.reassess.year==1) start.year <- 2 else start.year <- first.reassess.year
  
  for(y in start.year:length(mv.on.roll)){
    if(reassess.year[y]) {
      mv.total.diff <- mv.known[y] - mv.on.roll[y - 1]
      mv.annual.increment <- mv.total.diff / adjust.period
      periods.adjusted <- 0
    }
    periods.adjusted <- periods.adjusted + 1
    if(periods.adjusted > adjust.period) mv.annual.increment <- 0
    mv.on.roll[y] <- mv.on.roll[y - 1] + mv.annual.increment
  }
  # cbind(year, reassess.year, mv, mv.known, mv.ar)
  
  # may need some error checking for incorrect inputs
  return(mv.on.roll)
}


get_av.taxable <- function(av, tav.grcap){
  # taxable assessed value
  grcap <- tav.grcap[1]
  av.taxable <-  rep(NA_real_, length(av))
  av.taxable[1] <- av[1]
  for(y in 2:length(av.taxable)){
    av.taxable[y] <- pmin(av[y], av.taxable[y - 1] * (1 + grcap))
  }
  return(av.taxable)
}


