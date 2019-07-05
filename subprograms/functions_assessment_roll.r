


get_mv <- function(assume, growth_scenarios, globals, nprop){
  # return:
  #   mv growth rates
  #   cumulative mv growth rates indexed to year 1
  #   market values of properties for all years, matrix
  #   matrices of acquisition value and cycle assessing
  v_mvgr_to_next <- insert(growth_scenarios[[assume$mv_growth]] / 100, make_vec(.06)) # should we pass growth_scenarios as a target?
  
  # get cumulative growth rates, indexed to year 1
  v_icum_mvgr <- cumprod(1 + c(0, v_mvgr_to_next[-length(v_mvgr_to_next)])) # cumulative growth to the NEXT year, before indexing to year 1 
  names(v_icum_mvgr) <- names(v_mvgr_to_next) # otherwise names are WRONG and we use name indexing
  v_icum_mvgr <- v_icum_mvgr / v_icum_mvgr["1"]
  
  # matrices of market values (true) - later may update with stochastic growth
  m_mvtrue <- matrix(rep(100 * v_icum_mvgr, nprop),
                     byrow=TRUE, nrow=nprop, ncol=globals$totyears, 
                     dimnames=list(1:nprop, globals$years))
  
  # matrices giving amount of mvtrue that is on acquisition assessing and amount on cycle assessing
  m_mvtrue_acquisition <- m_mvtrue * assume$acqvalue_share
  m_mvtrue_cycle <- m_mvtrue - m_mvtrue_acquisition
  
  # create the return list
  mv_list <- list()
  mv_list <- within(mv_list,{
    v_mvgr_to_next <- v_mvgr_to_next
    v_icum_mvgr <- v_icum_mvgr
    m_mvtrue <- m_mvtrue
    m_mvtrue_acquisition <- m_mvtrue_acquisition
    m_mvtrue_cycle <- m_mvtrue_cycle
  })
  
  return(mv_list)
}


get_acquisition_values <- function(assume, mv_list, globals, nprop){
  get_mat <- function(mat, values=NA, ...){
    # create matrix based on another matrix
    matrix(values, nrow=nrow(mat), ncol=ncol(mat), dimnames=dimnames(mat), ...)
  }
  
  # matrix of sale indicators - 
  set.seed(1234)
  m_sale <- get_mat(mv_list$m_mvtrue_acquisition, 
                    rbinom(nprop * globals$totyears, 1, 1 / assume$salecycle_years))
  
  # matrix of aquisition value ceiliengs and matrix of acquisition values used on AR
  m_acquisition_av_ceiling <- get_mat(mv_list$m_mvtrue_acquisition)
  m_acquisition_av_ceiling[, 1] <- mv_list$m_mvtrue_acquisition[, 1] # set first year to market value
  
  m_acquisition_av <- get_mat(mv_list$m_mvtrue_acquisition)
  m_acquisition_av[, 1] <- mv_list$m_mvtrue_acquisition[, 1] # set first year to market value
  
  for(j in 2:globals$totyears){
    m_acquisition_av_ceiling[, j] <- ifelse(m_sale[, j]==1, mv_list$m_mvtrue_acquisition[, j], # set ceiling to mv in sale years
                                            m_acquisition_av_ceiling[, j-1] * (1 + assume$av_grcap)) # calculate ceiling growth from there
    # if policy for non-sale years is to drop the ceiling when market value is lower than celing, depending on policy
    if(assume$decline_reset==1) {
      m_acquisition_av_ceiling[, j] <- ifelse(m_acquisition_av_ceiling[, j] > mv_list$m_mvtrue_acquisition[, j],
                             mv_list$m_mvtrue_acquisition[, j],
                             m_acquisition_av_ceiling[, j])
    }
    
    m_acquisition_av[, j] <- pmin(m_acquisition_av_ceiling[, j], mv_list$m_mvtrue_acquisition[, j])
  }
  
  acquisition_val_list <- list()
  acquisition_val_list <- within(acquisition_val_list, {
    m_sale <- m_sale
    m_acquisition_av_ceiling <- m_acquisition_av_ceiling
    m_acquisition_av <- m_acquisition_av
  })
  return(acquisition_val_list)
}

(assume <- as.list(rc[1, ]))

get_mv(assume, growth_scenarios, globals, 10)


build_assessment_roll <- function(assume, globals, growth_scenarios, nprop=10){
  # return data frame with data for a single scenario
  # Keep track of:
  #   value that is assessed using acquisition-value assessing, and value using cycle-mv assessing
  # Conventions:
  #   v_... is a vector with 
  #   m_... is a matrix
  #   df_... is a data frame
  
  # initialize -- create needed vectors and matrices
  # get all of the market value information
  mv_list <- get_mv(assume, growth_scenarios, globals, nprop)
  # names(mv_list)
  
  # get acquisition values for those properties that are on acquisition-value assessing
  acqv_list <- get_acquisition_values(assume, mv_list, globals, nprop)
  # names(acqv_list)
  
  # get assessment-cycle values for those properties that are on assessment-cycle assessing
  
  v_reassess <- create_cycle(assume$avcycle_nyears, assume$avcycle_startyear) # reassessment cycle -- 1 or 0

  
  # prop_base is df with 1 row per year, with basic information about the run
  prop_base <- tibble(runname=assume$runname,
                      year=globals$years,
                      index=globals$iyears,
                      reassess=v_reassess,
                      gr_to_next=mv_list$v_mvgr_to_next,
                      icum_vmgr=mv_list$v_icum_mvgr)

  # zero_years <- NULL
  # if(assume$salecycle_zbefore >= min(years)) zero_years <- min(years):(assume$salecycle_zbefore -1)
  # if(!is.null(zero_years)) msale[, as.character(zero_years)] <- 0
  
  # colMeans()
  # [1:10, 1:5]
  # matrix of mv on the assessment roll, based on cycle
  mmvar_cycle <- matrix(NA, nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  mmvar_cycle[, 1:assume$armv_lag] <- mmv_res_true[, rep(1, assume$armv_lag)] # fill the initial lags with firstyear mv
  
  # matrix of aquisition market values - if there is a sale use market values, if not use prior plus growth
  # note that this is an "on the assessment roll" concept meaning it can be lagged relative to true market values
  # use lesser of grown prior amv or the mv
  mmvar_acquisition <- matrix(NA, nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  mmvar_acquisition[, 1:assume$armv_lag] <- mmv_res_true[, rep(1, assume$armv_lag)] # fill the initial lags with first of mv
  # mmvar_acquisition[, 1:assume$armv_lag] <- mmv_res_true[, 1:assume$armv_lag] # fill the initial lags with initial years of mv
  
  # fill the residential cycle and acquisition values
  for(j in (assume$armv_lag + 1):totyears){
    mmvar_cycle[, j] <- ifelse(reassess[j]==1, 
                               mmv_res_true[, j - assume$armv_lag], 
                               mmvar_cycle[, j - 1])
    
    mmvar_acquisition[, j] <- ifelse(msale[, j]==1,
                                     mmv_res_true[, j - assume$armv_lag], 
                                     pmin(mmv_res_true[, j - assume$armv_lag], mmvar_acquisition[, j - 1] * (1 +  assume$av_grcap)))
    if(assume$fastdown==1) mmvar_acquisition[, j] <- pmin(mmvar_acquisition[, j], mmv_res_true[, j - 1]) # forces fast downward reassessment
  }
  # mmv_res_true[1:8, 1:10]
  # msale[1:8, 1:10]
  # mmvar_acquisition[1:8, 1:10]
  
  # CAUTION: mvar is market value on the assessment roll -- it can be greater than true market value 
  # because there can be lags
  mdf <- bind_rows(as_tibble(mmv_res_true) %>% mutate(vname="mv_res_true", propid=row_number()),
                   as_tibble(mmv_nonres) %>% mutate(vname="mv_nonres", propid=row_number()),
                   as_tibble(mmvar_cycle) %>% mutate(vname="mvar_cycle", propid=row_number()),
                   as_tibble(msale) %>% mutate(vname="sale", propid=row_number()),
                   as_tibble(mmvar_acquisition) %>% mutate(vname="mvar_acquisition", propid=row_number())) %>%
    gather(year, value, -propid, -vname) %>%
    mutate(year=as.integer(year)) %>%
    spread(vname, value)
  
  prop_all <- prop_base  %>%
    right_join(mdf, by="year") %>%
    select(runname, propid, year, index, sale, reassess, gr_mv, icumgr_mv, mv_nonres, mv_res_true, mvar_cycle, mvar_acquisition)
  
  return(prop_all)
}


