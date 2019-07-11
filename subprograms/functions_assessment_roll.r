
get_mat <- function(mat, values=NA, ...){
  # create matrix based on another matrix
  matrix(values, nrow=nrow(mat), ncol=ncol(mat), dimnames=dimnames(mat), ...)
}


get_mv <- function(assume, growth_scenarios, globals, nprop){
  # return:
  #   mv growth rates
  #   cumulative mv growth rates indexed to year 1
  #   market values of properties for all years, matrix
  #   matrices of acquisition value and cycle assessing
  
  # split the mv of acquisition properties - for now - across 1 property sale year
  # split the mv of cycle properties - for now - across 1 property per cycle year
  
  zpad_len <- 2 # for zero padding; maybe calculate based on nprop
  
  v_mvgr_to_next <- insert(growth_scenarios[[assume$mv_growth]] / 100, make_vec(.06)) # should we pass growth_scenarios as a target?
  
  # get cumulative growth rates, indexed to year 1
  v_icum_mvgr <- cumprod(1 + c(0, v_mvgr_to_next[-length(v_mvgr_to_next)])) # cumulative growth to the NEXT year, before indexing to year 1 
  names(v_icum_mvgr) <- names(v_mvgr_to_next) # otherwise names are WRONG and we use name indexing
  v_icum_mvgr <- v_icum_mvgr / v_icum_mvgr["1"]
  
  # matrices of market values (true) - later may update with stochastic growth
  v_mvtrue <- 100 * v_icum_mvgr
  
  str_pad(1, 4, side="left", pad="0")
  # m_mvtrue <- matrix(rep(100 * v_icum_mvgr, nprop),
  #                    byrow=TRUE, nrow=nprop, ncol=globals$totyears, 
  #                    dimnames=list(1:nprop, globals$years))
  m_mvtrue_acquisition <- matrix(rep(v_mvtrue * assume$acqvalue_share / assume$salecycle_nyears, assume$salecycle_nyears),
                                 byrow=TRUE, nrow=assume$salecycle_nyears, ncol=globals$totyears,
                                 dimnames=list(paste0("acq_", str_pad(1:assume$salecycle_nyears, zpad_len, side="left", pad="0")),
                                               globals$years))
  
  m_mvtrue_cycle <- matrix(rep(v_mvtrue * (1 - assume$acqvalue_share) / assume$avcycle_nyears, assume$avcycle_nyears),
                                 byrow=TRUE, nrow=assume$avcycle_nyears, ncol=globals$totyears,
                                 dimnames=list(paste0("avcycle_", str_pad(1:assume$avcycle_nyears, zpad_len, side="left", pad="0")),
                                               globals$years))
  
  m_mvtrue <- rbind(m_mvtrue_acquisition, m_mvtrue_cycle)
  # rownames(m_mvtrue) <- NULL
  # colSums(m_mvtrue)
  
  # matrices giving amount of mvtrue that is on acquisition assessing and amount on cycle assessing
  # m_mvtrue_acquisition <- m_mvtrue * assume$acqvalue_share
  # m_mvtrue_cycle <- m_mvtrue - m_mvtrue_acquisition
  
  # create the return list
  mv_list <- list()
  mv_list <- within(mv_list,{
    v_mvgr_to_next <- v_mvgr_to_next
    v_icum_mvgr <- v_icum_mvgr
    v_mvtrue <- v_mvtrue
    m_mvtrue <- m_mvtrue
    m_mvtrue_acquisition <- m_mvtrue_acquisition
    m_mvtrue_cycle <- m_mvtrue_cycle
  })
  
  return(mv_list)
}


get_sale_years <- function(mat, globals, nyears){
  # get a matrix of sale indicators indicating whether a property has been sold or not (1 or 0)
  # rows: 1 per property
  # columns: 1 per year
  
  # number of rows could change in the future
  # and could differ between acquisition and cycle assessing
  # and determination of sale years could change, too
  # thus I keept this general, allowing 2 different matrices, 1 for acquisition and 1 for cycle assessing
  
  # alternative approach: 
  # set.seed(1234)
  # m_sale_year <- get_mat(mv_list$m_mvtrue_acquisition,
  #                        rbinom(nprop * globals$totyears, 1, 1 / assume$salecycle_years))
  # simple alternating matrix
  
  nprop <- nrow(mat)
  m_sale_year <- get_mat(mat)
  
  for(i in 1:nrow(m_sale_year)){
    start <- nyears - i + 1
    end <- start + globals$totyears - 1
    m_sale_year[i, ] <- ((start:end %% nyears) == 0) * 1
  }
  return(m_sale_year)
}


get_acquisition_values <- function(assume, mv_list, globals, nprop){

  m_av_acquisition_sale_year <- get_sale_years(mv_list$m_mvtrue_acquisition, globals, assume$salecycle_nyears)

  
  # matrix of aquisition value ceilings and matrix of acquisition values used on AR
  m_av_acquisition_ceiling <- get_mat(mv_list$m_mvtrue_acquisition)
  m_av_acquisition_ceiling[, 1] <- mv_list$m_mvtrue_acquisition[, 1] # set first year to market value
  
  m_av_acquisition <- get_mat(mv_list$m_mvtrue_acquisition)
  m_av_acquisition[, 1] <- mv_list$m_mvtrue_acquisition[, 1] # set first year to market value
  
  for(year in 2:globals$totyears){
    m_av_acquisition_ceiling[, year] <- ifelse(m_av_acquisition_sale_year[, year]==1, mv_list$m_mvtrue_acquisition[, year], # set ceiling to mv in sale years
                                            m_av_acquisition_ceiling[, year-1] * (1 + assume$av_grcap)) # calculate ceiling growth from there
    # if policy for non-sale years is to drop the ceiling when market value is lower than celing, depending on policy
    if(assume$decline_reset==1) {
      m_av_acquisition_ceiling[, year] <- ifelse(m_av_acquisition_ceiling[, year] > mv_list$m_mvtrue_acquisition[, year],
                             mv_list$m_mvtrue_acquisition[, year],
                             m_av_acquisition_ceiling[, year])
    }
    
    m_av_acquisition[, year] <- pmin(m_av_acquisition_ceiling[, year], mv_list$m_mvtrue_acquisition[, year])
  }
  
  acquisition_val_list <- list()
  acquisition_val_list <- within(acquisition_val_list, {
    m_av_acquisition_sale_year <- m_av_acquisition_sale_year
    m_av_acquisition_ceiling <- m_av_acquisition_ceiling
    m_av_acquisition <- m_av_acquisition
  })
  return(acquisition_val_list)
}


get_assessment_cycle_values <- function(assume, mv_list, globals, nprop){
  v_reassess_year <- create_cycle(assume$avcycle_nyears, assume$avcycle_startyear) # reassessment cycle -- 1 or 0
  
  # matrix of mv on the assessment roll, based on cycle
  m_av_cycle <- get_mat(mv_list$m_mvtrue_cycle)
  m_av_cycle[, 1:assume$armv_lag] <- mv_list$m_mvtrue_cycle[, rep(1, assume$armv_lag)] # fill the initial lags with firstyear mv
  
  for(year in (assume$armv_lag + 1):globals$totyears){
    m_av_cycle[, year] <- ifelse(v_reassess_year[year]==1, 
                              mv_list$m_mvtrue_cycle[, year - assume$armv_lag], 
                              m_av_cycle[, year - 1])
  }
  
  assessment_cycle_val_list <- list()
  assessment_cycle_val_list <- within(assessment_cycle_val_list, {
    v_reassess_year <- v_reassess_year
    m_av_cycle <- m_av_cycle
  })
  return(assessment_cycle_val_list)
}


prep_mat <- function(mat){
  # mat is a matrix
  # df is a data frame
  mat_name <- deparse(substitute(mat)) %>%
    str_split(coll("$"), simplify = TRUE)
  mat_name <- mat_name[, ncol(mat_name)]
  df <- as_tibble(mat, rownames="propid") %>%
    mutate(vname=str_sub(mat_name, 3)) %>%
    gather(year, value, -propid, -vname) %>%
    mutate(year=as.integer(year))
  return(df)
}


build_assessment_roll <- function(assume, globals, growth_scenarios, nprop=10){
  # return data frame with data for a single scenario
  # Keep track of:
  #   value that is assessed using acquisition-value assessing, and value using cycle-mv assessing
  # Conventions:
  #   v_... is a vector with 
  #   m_... is a matrix
  
  # initialize -- create needed vectors and matrices
  # get all of the market value information
  mv_list <- get_mv(assume, growth_scenarios, globals, nprop)
  # names(mv_list)

  # get assessment-cycle values for those properties that are on assessment-cycle assessing
  av_cycle_list <- get_assessment_cycle_values(assume, mv_list, globals, nprop)
  # names(av_cycle_list)
    
  # get acquisition values for those properties that are on acquisition-value assessing
  av_acquisition_val_list <- get_acquisition_values(assume, mv_list, globals, nprop)
  # names(av_acquisition_val_list)
  
  # prop_vectors is df with 1 row per year, with basic information about the run
  prop_vectors <- tibble(runname=assume$runname,
                         year=globals$years,
                         index=globals$iyears,
                         gr_to_next=mv_list$v_mvgr_to_next,
                         icum_vmgr=mv_list$v_icum_mvgr,
                         reassess_year=av_cycle_list$v_reassess_year)
  
  # prop_matrices is df with 1 row per year per property for each property in a given matrix
  prop_matrices <- bind_rows(prep_mat(mv_list$m_mvtrue),
                             prep_mat(mv_list$m_mvtrue_acquisition),
                             prep_mat(mv_list$m_mvtrue_cycle),
                             
                             prep_mat(av_cycle_list$m_av_cycle),
                             
                             prep_mat(av_acquisition_val_list$m_av_acquisition),
                             prep_mat(av_acquisition_val_list$m_av_acquisition_ceiling),
                             prep_mat(av_acquisition_val_list$m_av_acquisition_sale_year))
  prop_matrices_wide <- prop_matrices %>%
    spread(vname, value)
  # glimpse(mdf)
  # count(mdf, vname)
  
  # get_mlist <- function(mlist){
  #   lname <-  deparse(substitute(mlist))
  #   mat_list <- names(mlist)[str_sub(names(mlist), 1, 2)=="m_"]
  #   mat_list <- paste0(lname, "$", mat_list)
  #   # print(mat_list)
  #   return(mat_list)
  # }
  # get_mlist(mv_list)
  # ns(prop_all)
  
  prop_all <- prop_vectors %>%
    right_join(prop_matrices_wide, by="year") %>%
    select(runname, propid, year, index, 
           gr_to_next, icum_vmgr, 
           mvtrue, mvtrue_acquisition, mvtrue_cycle,
           av_acquisition_sale_year, av_acquisition_ceiling, av_acquisition, 
           reassess_year, av_cycle)
  
  return(prop_all)
}


