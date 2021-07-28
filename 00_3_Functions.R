###################################################
# Functions
###################################################

crra_u <- function(rho, x) {
  u <- (x^(1 - rho) - 1) / (1 - rho)
  return(u)
}

############################################
# Static Simulation
############################################
# Objectives:
#   1. find the best SMT for each level of the APH levels
#   2. generate the APH

# i <- 1
# ls(results)

static_sim <- function(i, data) {
  # print(paste(i,'/',APH_len,sep=''))
  print(i)

  data_temp <- copy(data)
  yield_len <- length(data_temp[, yield])

  #---------------------------
  # Update The Parameters
  #----------------------------
  APH_temp <- APH_list[i]
  rate_yield <- APH_temp
  results <- sim_for_BI(
    data[, yield], data[, hp], APH_temp, rate_yield, acres, cov_level,
    p_price, pvf, subsidy_percent, year, county_TY, c_rate_now,
    c_rate_prev, c_pars, beta_RP, sec5, sec6, data[, ir_cost]
  )

  #--- assign profits ---#
  final_results <- data_temp[, `:=`(
    pi_RP = crra_u(0.6, results$pi_RP + (acres * 35 * 30)),
    pi_non = crra_u(0.6, results$pi_non + (acres * 35 * 30))
  )] %>%
    .[, .(e_pi_RP = mean(pi_RP), e_pi_non = mean(pi_non), yield = mean(yield)), by = .(smt, year)] %>%
    .[, APH := APH_temp] %>%
    .[, APH_next := ifelse(yield < t_yield_ratio * t_yield, t_yield_ratio * t_yield, yield) / n_window + (n_window - 1) / n_window * APH]

  rm(data_temp)
  gc()
  return(final_results)
}

#/*=================================================*/
#' # Define the profit-calculation funciton for the last period (static)
#/*=================================================*/
#' This function does the followings:
#' 1. finds profits by smt for various combinations of at a given level of AY and coverage level.
#' 2. finds the expected profit by smt for the particular combination of AY and coverage level
#' Note: This does not include base premium rate, which depends on RY. They will be calcualted separately, and merged later.
#' ___

# i <- 1
# data <- base_data_s
# find_pi <- function(i,data){
#   print(paste(i,'/',AY_RY_len_g,sep=''))
#
#   #---------------------------
#   # Set the Parameters
#   #----------------------------
#   AY_temp <- AY_RY_ls_g[i,AY] # approved yield
#   RY_temp <- AY_RY_ls_g[i,RY] # rate yield
#
#   final_results <- copy(data) %>%
#     #=== profit without premium payment ===#
#     .[,pi_no_premium := pmax(raw_revenue,AY_temp*tau*acres*pmax(hp,p_price)/1000)-ir_cost] %>%
#     .[,ir_cost:=NULL] %>%
#     .[,hp:=NULL] %>%
#     .[,yield:=NULL] %>%
#     .[,raw_revenue:=NULL] %>%
#     premiums[AY==AY_temp & RY==RY_temp,.(premium,tau)][.,on=c('tau')] %>%
#     .[,.(e_u=mean(crra_u(0.6,pi_no_premium-premium))),by=.(smt,tau)] %>%
#     .[,AY:=AY_temp] %>%
#     .[,RY:=RY_temp]
#
#   gc()
#   return(final_results)
# }

# data <- base_data_s
# AY_it <- AY_list_g[16]

find_u_T <- function(AY_it, data) {

  #--------------------------
  # Calculate profit without premium
  #--------------------------
  #' Profits calcualted here are the same across RY for a given value of hAY
  print(paste0("------ working on AY = ", AY_it, " ------"))

  pi_data <- copy(data) %>%
    # === profit without premium payment ===#
    .[, pi_no_premium := pmax(raw_revenue, AY_it * tau * acres * pmax(hp, p_price) / 1000) - ir_cost] %>%
    .[, ir_cost := NULL] %>%
    .[, hp := NULL] %>%
    .[, yield := NULL] %>%
    .[, raw_revenue := NULL]

  # RY_it <- RY_list[3]
  get_pi_by_RY <- function(RY_it, pi_data) {
    print(paste0("currently at ", RY_it))

    temp_final <- premiums[AY == AY_it & RY == RY_it, .(premium, tau)][pi_data, on = "tau"] %>%
      .[, .(e_u = mean(crra_u(0.6, pi_no_premium - premium))), by = .(smt, tau)] %>%
      .[, RY := RY_it]

    return(temp_final)
  }

  final_results <- mclapply(RY_list, function(x) get_pi_by_RY(x, pi_data), mc.cores = detectCores() - 1) %>%
    rbindlist() %>%
    .[, AY := AY_it]

  rm(pi_data)
  gc()

  return(final_results)

}

#/*=================================================*/
#' # For dynamic simulation
#/*=================================================*/
#' find VF based on the VF_gam

VF_sim <- function(VF_gam, base_data) {

  #--- no hail ---#
  data_temp <- copy(base_data) %>%
    .[, V_next := predict(VF_gam, newdata = .)] %>%
    setkey(APH)

  #--- Vt hail ---#
  Vt_hail <- data.table(APH = APH_list) %>%
    .[, APH_next := t_yield_ratio * t_yield / n_window + (n_window - 1) / n_window * APH] %>%
    .[, V_hail := predict(VF_gam, newdata = .)] %>%
    setkey(APH)

  #--- expected hail ---#
  data_final <- Vt_hail[data_temp] %>%
    .[, e_V_next := (1 - damage_prob) * V_next + damage_prob * V_hail] %>%
    .[, e_V := e_pi_RP + e_V_next] %>%
    .[, .(e_pi_RP = mean(e_V)), by = .(smt, APH)] %>%
    .[, .SD[e_pi_RP == max(e_pi_RP), ], by = APH] %>%
    setnames("APH", "APH_next")

  rm(data_temp)
  gc()

  return(data_final)
}

# ===================================
# Find premium
# ===================================
#' depends on "Codes/premium_calc_RP.cpp"

find_premiums <- function(i) {
  
  print(i)

  AY_temp <- AY_RY_tau_ls_g[i, AY]
  RY_temp <- AY_RY_tau_ls_g[i, RY]
  tau_temp <- AY_RY_tau_ls_g[i, tau]
  sub_temp <- subsidy_table[coverage_level == tau_temp, subsidy_percent]

  premium_temp <- premium_calc_RP(
    RY_temp, AY_temp, tau_temp, sub_temp,
    p_price, pvf, acres, county_TY, ins_year, c_rate_now,
    c_rate_prev, c_pars, beta_RP, sec5, sec6
  )

  return(
    data.table(
      premium = premium_temp / 1000,
      RY = RY_temp,
      AY = AY_temp,
      tau = tau_temp
    )
  )
}

#/*=================================================*/
#' # Find optimal smt-tau for a given AY-RY combination
#/*=================================================*/

get_opt_ir_t <- function(AY_it, V_ls, data) {
  
  print(paste(AY_it))

  #--------------------------
  # Calculate AY_next (RY_next already clcualted)
  #--------------------------
  temp_data <- copy(data) %>%
    .[, AY_next := ifelse(yield < temp_ysr * t_yield, temp_ysr * t_yield, yield) / n_window + (n_window - 1) / n_window * AY_it] %>%
    .[AY_next <= AY_min, AY_next := AY_min]

  #--------------------------
  # V in the next period
  #--------------------------
  V_AY_RY <- temp_data[, .(AY_next, RY_next)] %>%
    unique() %>%
    # === RY_next>=150 ===#
    .[RY_next >= 150, V_hat := predict(V_ls[["RY_150up"]], newdata = .SD)] %>%
    # === 50<=RY_next<=150 ===#
    .[RY_next >= 50 & RY_next < 150, V_hat := predict(V_ls[["RY_50up_150down"]], newdata = .SD)] %>%
    # === RY_next<50 ===#
    .[RY_next < 50, V_hat := predict(V_ls[["RY_50down"]], newdata = .SD)]

  #--------------------------
  # Best smt-tau and its associated utility
  #--------------------------
  opt_ir_V <- V_AY_RY[temp_data, on = c("AY_next", "RY_next")] %>%
    .[, .(V_next = mean(V_hat)), by = .(smt, tau, RY)] %>%
    # === merge concurrent expected utility ===#
    pi_static_l[AY == AY_it, ][., on = c("smt", "tau", "RY")] %>%
    # === sum the concurrent and future V ===#
    .[, u_tot := e_u + sigma * V_next] %>%
    .[, .SD[u_tot == max(u_tot)], by = "RY"]

  rm(temp_data, V_AY_RY)
  gc()

  return(opt_ir_V)

}

# ===================================
# Simulation after backward induction
# ===================================

simulate <- function(i) {
  # /*=================================================*/
  #' # Preparation for simulation
  # /*=================================================*/
  #--------------------------
  # working parameters
  #--------------------------
  temp_irc <- ir_ys_ri_ls[i, ir_cost]
  temp_ysr <- ir_ys_ri_ls[i, ys_ratio]
  temp_ri <- ir_ys_ri_ls[i, ri]

  #--------------------------
  # optimal smt-tau look up table
  #--------------------------
  opt_ir_V_data <- readRDS(paste0(
    "./Results_TM/opt_ir_V_pw_", temp_irc,
    "_ysr_", temp_ysr,
    "_ri_", temp_ri,
    ".rds"
  )) %>%
    .[t == 1, ] %>%
    .[, .(smt, tau, AY_next, RY_next)] %>%
    setnames(c("AY_next", "RY_next"), c("AY", "RY"))

  AY_sim_list <- opt_ir_V_data[, AY] %>% unique()
  RY_sim_list <- opt_ir_V_data[, RY] %>% unique()

  # /*=================================================*/
  #' # Simulation
  # /*=================================================*/
  for (k in 1:T) {
    print(paste0("working on t=", k))
    temp_data <- copy(starting_data_store[[paste(k)]]) %>%

      #--- merge with weather data ---#
      weather_hp_data[period == k][., on = c("sim_id", "period")]

    #--- find the closest APH_start value ---#
    AY_apx_ls <- data.table(w = AY_sim_list, val = AY_sim_list) %>%
      setattr("sorted", "w") %>%
      .[J(temp_data[, AY_start]), roll = "nearest"] %>%
      .[, val]

    RY_apx_ls <- data.table(w = RY_sim_list, val = RY_sim_list) %>%
      setattr("sorted", "w") %>%
      .[J(temp_data[, RY_start]), roll = "nearest"] %>%
      .[, val]

    if (k != T) {
      final_data <- temp_data %>%
        .[, AY := AY_apx_ls] %>%
        .[, RY := RY_apx_ls] %>%
        opt_ir_V_data[., on = c("AY", "RY")] %>%
        data_for_BI[., on = c("smt", "year")] %>%
        .[, AY_start := ifelse(yield < temp_ysr * t_yield, temp_ysr * t_yield, yield) / n_window + (n_window - 1) / n_window * AY] %>%
        .[, RY_start := yield / n_window + (n_window - 1) / n_window * RY] %>%
        .[, .(sim_id, AY_start, RY_start)] %>%
        .[, period := k + 1]

      starting_data_store <- list()
      starting_data_store[[paste(k + 1)]] <- final_data
    } else {
      results <- temp_data %>%
        .[, AY := AY_apx_ls] %>%
        .[, RY := RY_apx_ls] %>%
        opt_ir_V_data[., on = c("AY", "RY")] %>%
        data_for_BI[., on = c("smt", "year")] %>%
        .[, AY_start := ifelse(yield < temp_ysr * t_yield, temp_ysr * t_yield, yield) / n_window + (n_window - 1) / n_window * AY] %>%
        .[, RY_start := yield / n_window + (n_window - 1) / n_window * RY] %>%
        .[, irc := temp_irc] %>%
        .[, ri := temp_ri] %>%
        .[, ysr := temp_ysr]
    }
  }

  return(results)
}

# /*=================================================*/
#' # Simulations with TA
# /*=================================================*/

simulate_TA <- function(i) {
  # /*=================================================*/
  #' # Preparation for simulation
  # /*=================================================*/
  #--------------------------
  # working parameters
  #--------------------------
  temp_irc <- irc_TA_ls[i, irc]
  temp_TA <- irc_TA_ls[i, TA]
  temp_t_yield_ratio <- 0.6
  temp_n_window <- 10

  #--------------------------
  # optimal smt-tau look up table
  #--------------------------
  opt_ir_V_data <- readRDS(paste0(
    "./Results_TM/opt_ir_V_pw_", temp_irc,
    "_ysr_", 0.6,
    "_ri_FALSE.rds"
  )) %>%
    .[t == 1, ] %>%
    .[, .(smt, tau, AY_next, RY_next)] %>%
    setnames(c("AY_next", "RY_next"), c("AY", "RY"))

  AY_sim_list <- opt_ir_V_data[, AY] %>% unique()
  RY_sim_list <- opt_ir_V_data[, RY] %>% unique()

  # /*=================================================*/
  #' # Simulation
  # /*=================================================*/
  for (k in 1:T) {
    print(paste0("working on t=", k))
    temp_data <- copy(starting_data_store[[paste(k)]]) %>%
      #--- merge with weather data ---#
      weather_hp_data[period == k][., on = c("sim_id", "period")]

    #--- find the closest APH_start value ---#
    AY_apx_ls <- data.table(w = AY_sim_list, val = AY_sim_list) %>%
      setattr("sorted", "w") %>%
      .[J(temp_data[, AY_start + 5.5 * temp_TA]), roll = "nearest"] %>%
      .[, val]

    RY_apx_ls <- data.table(w = RY_sim_list, val = RY_sim_list) %>%
      setattr("sorted", "w") %>%
      .[J(temp_data[, RY_start]), roll = "nearest"] %>%
      .[, val]

    if (k != T) {
      final_data <- temp_data %>%
        .[, AY := AY_apx_ls] %>%
        .[, RY := RY_apx_ls] %>%
        opt_ir_V_data[., on = c("AY", "RY")] %>%
        data_for_BI[., on = c("smt", "year")] %>%
        .[, AY_start := ifelse(yield < temp_t_yield_ratio * t_yield, temp_t_yield_ratio * t_yield, yield) / temp_n_window + (temp_n_window - 1) / temp_n_window * (AY - 5.5 * temp_TA)] %>%
        .[, RY_start := yield / temp_n_window + (temp_n_window - 1) / temp_n_window * RY] %>%
        .[, .(sim_id, AY_start, RY_start)] %>%
        .[, period := k + 1]

      starting_data_store <- list()
      starting_data_store[[paste(k + 1)]] <- final_data
    } else {
      results <- temp_data %>%
        .[, AY := AY_apx_ls] %>%
        .[, RY := RY_apx_ls] %>%
        opt_ir_V_data[., on = c("AY", "RY")] %>%
        data_for_BI[., on = c("smt", "year")] %>%
        .[, AY_start := ifelse(yield < temp_t_yield_ratio * t_yield, temp_t_yield_ratio * t_yield, yield) / temp_n_window + (temp_n_window - 1) / temp_n_window * (AY - 5.5 * temp_TA)] %>%
        .[, RY_start := yield / temp_n_window + (temp_n_window - 1) / temp_n_window * RY] %>%
        .[, irc := temp_irc] %>%
        .[, TA := temp_TA]
    }
  }

  return(results)
}
