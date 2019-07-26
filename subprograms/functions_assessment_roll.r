
get_mat <- function(mat, values=NA, ...){
  # create matrix based on another matrix - same dimensions and names
  matrix(values, nrow=nrow(mat), ncol=ncol(mat), dimnames=dimnames(mat), ...)
}


get_mv <- function(assume, growth_scenarios, globals){
  # return:
  #   mv growth rates
  #   cumulative mv growth rates indexed to year 1
  #   market values of properties for all years, matrix
  #   matrices of acquisition value and cycle assessing
  
  # split the mv of acquisition properties - for now - across 1 property sale year
  # split the mv of cycle properties - for now - across 1 property per cycle year
  
  # CAUTION: Make sure we don't allow an infinite # of properties!
  nprop_avcycle <- assume$salecycle_nyears * assume$avcycle_nyears
  if(assume$avcycle_salemethod != "deterministic") nprop_avcycle <- max(nprop_avcycle, assume$avcycle_nprop)
  
  nprop_acquisition <- assume$salecycle_nyears
  if(assume$acqvalue_salemethod != "deterministic") nprop_acquisition <- max(nprop_acquisition, assume$acqvalue_nprop)
  
  nprop <- nprop_avcycle + nprop_acquisition
  
  # zero padding for property ids
  if(nprop > 1000) zpad_len <- 4 else zpad_len <- 3
  
  # market-value growth vectors:
  v_mvgr_to_next <- insert(growth_scenarios[[assume$mv_growth]] / 100, make_vec(.06)) # should we pass growth_scenarios as a target?
  
  # get cumulative growth rates, indexed to year 1
  v_icum_mvgr <- cumprod(1 + c(0, v_mvgr_to_next[-length(v_mvgr_to_next)])) # cumulative growth to the NEXT year, before indexing to year 1 
  names(v_icum_mvgr) <- names(v_mvgr_to_next) # otherwise names are WRONG and we use name indexing
  v_icum_mvgr <- v_icum_mvgr / v_icum_mvgr["1"]
  v_mvtrue <- 100 * v_icum_mvgr
  
  m_mvtrue_acquisition <- matrix(rep(v_mvtrue * assume$acqvalue_share / nprop_acquisition, nprop_acquisition),
                                 byrow=TRUE, nrow=nprop_acquisition, ncol=globals$totyears,
                                 dimnames=list(paste0("acq_", str_pad(1:nprop_acquisition, zpad_len, side="left", pad="0")),
                                               globals$years))
  
  m_mvtrue_cycle <- matrix(rep(v_mvtrue * (1 - assume$acqvalue_share) / nprop_avcycle, nprop_avcycle),
                                 byrow=TRUE, nrow=nprop_avcycle, ncol=globals$totyears,
                                 dimnames=list(paste0("avcycle_", str_pad(1:nprop_avcycle, zpad_len, side="left", pad="0")),
                                               globals$years))
  
  m_mvtrue <- rbind(m_mvtrue_acquisition, m_mvtrue_cycle)
  
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


get_sale_years_binomial <- function(mat, globals, avg_years_between_sale){
  # alternative approach: 
  nprop <- nrow(mat)
  nyears <- ncol(mat)
  set.seed(1234)
  m_sale_year <- get_mat(mat,
                         rbinom(nprop * nyears, 1, 1 / avg_years_between_sale))
  return(m_sale_year)
}


get_sale_years_deterministic <- function(mat, globals, avg_years_between_sale){
  # get a matrix of sale indicators indicating whether a property has been sold or not (1 or 0)
  # mat is the matrix upon which to base the new matrix, m_sale_year
  # rows: 1 per property
  # columns: 1 per year
  
  # number of rows could change in the future
  # and could differ between acquisition and cycle assessing
  # and determination of sale years could change, too
  # thus I keept this general, allowing 2 different matrices, 1 for acquisition and 1 for cycle assessing
  
  # simple deterministic alternating matrix
  nprop <- nrow(mat)
  m_sale_year <- get_mat(mat)
  
  for(i in 1:nrow(m_sale_year)){
    start <- avg_years_between_sale - i + 1
    end <- start + globals$totyears - 1
    m_sale_year[i, ] <- ((start:end %% avg_years_between_sale) == 0) * 1
  }
  return(m_sale_year)
}


get_acquisition_values <- function(assume, mv_list, globals){
  
  # determine the years in which properties are sold - either deterministically or by a random method
  if(assume$avcycle_salemethod=="deterministic"){ # sales are deterministic
    m_av_acquisition_sale_year <- get_sale_years_deterministic(mv_list$m_mvtrue_acquisition, globals, assume$salecycle_nyears)
  } else if(assume$avcycle_salemethod=="binomial"){
    m_av_acquisition_sale_year <- get_sale_years_binomial(mv_list$m_mvtrue_acquisition, globals, assume$salecycle_nyears)
  }
  
  # m_av_acquisition_sale_year <- get_sale_years_deterministic(mv_list$m_mvtrue_acquisition, globals, assume$salecycle_nyears)

  
  # matrix of aquisition value ceilings and matrix of acquisition values used on AR
  m_av_acquisition_ceiling <- get_mat(mv_list$m_mvtrue_acquisition)
  m_av_acquisition_ceiling[, 1] <- mv_list$m_mvtrue_acquisition[, 1] # set first year to market value
  
  m_av_acquisition <- get_mat(mv_list$m_mvtrue_acquisition)
  m_av_acquisition[, 1] <- mv_list$m_mvtrue_acquisition[, 1] # set first year to market value
  
  for(year in 2:globals$totyears){
    m_av_acquisition_ceiling[, year] <- ifelse(m_av_acquisition_sale_year[, year]==1, mv_list$m_mvtrue_acquisition[, year], # set ceiling to mv in sale years
                                            m_av_acquisition_ceiling[, year-1] * (1 + assume$acqvalue_grcap)) # calculate ceiling growth from there
    # if policy for non-sale years is to drop the ceiling when market value is lower than celing, depending on policy
    if(assume$acqvalue_decline_reset==1) {
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

get_previous_reassessment_year <- function(reassess_years){
  # for each year, get the index in 1:globals$totyears of the latest previous reassessment year
  i_reassess_years <- which(reassess_years==1) # indexes of the reassessment years
  ii_prior <- findInterval(1:length(reassess_years), i_reassess_years + 1e-10) # indexes of those indexes that came sooner, EXCEPT THAT...
  # ... we need to fix any initial indexes which were set to zero - we'll set to NA instead
  ii_prior <- ifelse(ii_prior==0, NA, ii_prior)
  i_prior <- i_reassess_years[ii_prior]
  names(i_prior) <- names(reassess_years)
  return(i_prior)
}

# v_reassess_year
# get_previous_reassessment_year(v_reassess_year)

get_assessment_cycle_values <- function(assume, mv_list, globals){
  v_reassess_year <- create_cycle(assume$avcycle_nyears, assume$avcycle_startyear) # reassessment cycle -- 1 or 0
  v_prior_reassess_year <- get_previous_reassessment_year(v_reassess_year)
  baseyear_ratio <- mv_list$v_icum_mvgr[v_prior_reassess_year] / mv_list$v_icum_mvgr
  
  # determine the years in which properties are sold - either deterministically or by a random method
  if(assume$avcycle_salemethod=="deterministic"){ # sales are deterministic
    m_av_cycle_sale_year <- get_sale_years_deterministic(mv_list$m_mvtrue_cycle, globals, assume$salecycle_nyears)
  } else if(assume$avcycle_salemethod=="binomial"){
    m_av_cycle_sale_year <- get_sale_years_binomial(mv_list$m_mvtrue_cycle, globals, assume$salecycle_nyears)
  }
  
  # create matrix of assessed values and fill years in the lag period with mv for first year
  # (the lag period is the lag between the actual market value and when it shows on the assessment roll, due to admin lags)
  m_av_cycle <- get_mat(mv_list$m_mvtrue_cycle)  # get empty matrix of proper dimensions
  m_av_cycle[, 1:assume$avcycle_armv_lag] <- mv_list$m_mvtrue_cycle[, rep(1, assume$avcycle_armv_lag)]
  
  # loop through years after the initial lag period
  for(year in (assume$avcycle_armv_lag + 1):globals$totyears){
    if(v_reassess_year[year]==1) {
      # use lagged mv in reassessment years
      m_av_cycle[, year] <- mv_list$m_mvtrue_cycle[, year - assume$avcycle_armv_lag]
      } else { # not a reassessment year, so...
        sale_indexes <- which(m_av_cycle_sale_year[, year]==1)
        not_sale_indexes <- which(m_av_cycle_sale_year[, year]==0)
        
        # get lagged assessed values if NOT a sale year
        m_av_cycle[not_sale_indexes, year] <- m_av_cycle[not_sale_indexes, year - 1]
        
        # now deal with the complicated case of sale years, which depends upon the policy
        if(assume$avcycle_baseyear==0) {
          m_av_cycle[sale_indexes, year] <- mv_list$m_mvtrue_cycle[sale_indexes, year]
        } else if(assume$avcycle_baseyear==1) { # if the jurisdiction uses base-year assessing, adjust the mv back to the base year
            m_av_cycle[sale_indexes, year] <- m_av_cycle[sale_indexes, year] * baseyear_ratio[year]
        } # end of the treatment of sale years
      }
  }
  
  assessment_cycle_val_list <- list()
  assessment_cycle_val_list <- within(assessment_cycle_val_list, {
    v_reassess_year <- v_reassess_year
    m_av_cycle_sale_year <- m_av_cycle_sale_year
    m_av_cycle <- m_av_cycle
  })
  return(assessment_cycle_val_list)
}


# define values for testing:
# (assume <- as.list(rcuse[1, ]))


build_assessment_roll <- function(assume, globals, growth_scenarios){
  # return data frame with data for a single scenario
  # Keep track of:
  #   value that is assessed using acquisition-value assessing, and value using cycle-mv assessing
  # Conventions:
  #   v_... is a vector with 
  #   m_... is a matrix
  
  # initialize -- create needed vectors and matrices
  # get all of the market value information
  mv_list <- get_mv(assume, growth_scenarios, globals)
  # names(mv_list)

  # get assessment-cycle values for those properties that are on assessment-cycle assessing
  av_cycle_list <- get_assessment_cycle_values(assume, mv_list, globals)
  # names(av_cycle_list)
    
  # get acquisition values for those properties that are on acquisition-value assessing
  av_acquisition_val_list <- get_acquisition_values(assume, mv_list, globals)
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
                             prep_mat(av_cycle_list$m_av_cycle_sale_year),
                             
                             prep_mat(av_acquisition_val_list$m_av_acquisition),
                             prep_mat(av_acquisition_val_list$m_av_acquisition_ceiling),
                             prep_mat(av_acquisition_val_list$m_av_acquisition_sale_year))
  prop_matrices_wide <- prop_matrices %>%
    mutate(ptype=ifelse(str_detect(propid, "acq"), "acquisition", "cycle"),  
           propid=as.numeric(str_extract(propid, "(\\d)+"))) %>% # construct a numeric property id
    spread(vname, value)
  
  # str_sub(propid, 1, 3),
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
    select(runname, ptype, propid, year, index, 
           gr_to_next, icum_vmgr, 
           mvtrue, mvtrue_cycle, mvtrue_acquisition,
           reassess_year, av_cycle_sale_year,  av_cycle,
           av_acquisition_sale_year, av_acquisition_ceiling, av_acquisition
           )
  
  return(prop_all)
}


