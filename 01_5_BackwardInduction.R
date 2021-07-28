# /*=================================================*/
#' # Preparation
# /*=================================================*/
# #/*----------------------------------*/
# #' ## Basic working parameters
# #/*----------------------------------*/
# #=== t-yield ratio ===#
# temp_ysr <- tr_nw_list[i,temp_ysr]
#
# #=== number of yield years ===#
# n_window <- tr_nw_list[i,n_window]

# /*----------------------------------*/
#' ## filter out irrelevant part of data
# /*----------------------------------*/
# === minimum AY ===#
min_AY_l <- max(temp_ysr * county_TY - AY_step, AY_min)

# === concurrent utility ===#
pi_static_l <- pi_static[AY >= min_AY_l, ]

# === relevant range of AY ===#
AY_list_l <- AY_list_g[AY_list_g >= min_AY_l]

# === AY and RY list ===#
#' this is used for profit calculation
AY_RY_ls_l <- expand.grid(AY = AY_list_l, RY = RY_list) %>%
  data.table()
AY_RY_len_l <- nrow(AY_RY_ls_l)

# /*----------------------------------*/
#' ## Data to work on
# /*----------------------------------*/
e_ir_center <- e_ir_threshold_data[ir_cost == irc, e_ir]

if (temp_ri) {
  # === if complete shirking is NOT allowed ===#
  search_smt_ls <- e_data_for_summary[(e_ir <= e_ir_center + 1 & e_ir >= e_ir_center - 2), smt] %>%
    unique()
} else {
  # === if complete shirking is allowed ===#
  search_smt_ls <- e_data_for_summary[(e_ir <= e_ir_center + 1 & e_ir >= e_ir_center - 2) | e_ir <= 1, smt] %>%
    unique()
}

base_data_w <- base_data_d[smt %in% search_smt_ls, ] %>%
  .[rep(1:nrow(.), each = RY_len), ] %>%
  .[, RY := rep(RY_list, nrow(.) / length(RY_list))] %>%
  .[, RY_next := yield / n_window + (n_window - 1) / n_window * RY] %>%
  .[RY_next <= 1, RY_next := 1]

# /*=================================================*/
#' # Backward Induction
# /*=================================================*/
# AY_it <- AY_list_l[1]
# RY_it <- RY_list[1]
# V_ls <- VF_T_gam_ls
# i <- 1

# /*----------------------------------*/
#' ## Backward induction
# /*----------------------------------*/
#' the first VF_T data to be used
VF_t_gam_ls <- VF_T_gam_ls

#' create a placeholder to save the results
opt_ir_V_data_ls <- list()
# VF_ls <- list()

# k <- prod_T-5
#' list of names for VF estimation
VF_t_name_ls <- c("RY_150up", "RY_50up_150down", "RY_50down")
VF_t_condition_ls <- c("RY_next>=150", "RY_next>=50 & RY_next<=150", "RY_next<=50")

# k <- prod_T-1
for (k in (prod_T - 1):1) {
  print(paste0(" ----- working on t=", k, " ----- "))

  #--------------------------
  # Find the optimal smt-tau for all the AY-RY combinations
  #--------------------------
  # get_opt_ir_t(AY_list_l[1],V_ls=VF_t_gam_ls,data=base_data_w)

  opt_ir_V_data <- mclapply(
    AY_list_l,
    function(x) get_opt_ir_t(x, V_ls = VF_t_gam_ls, data = base_data_w),
    mc.cores = 12
  ) %>%
    rbindlist() %>%
    setnames(c("AY", "RY"), c("AY_next", "RY_next")) %>%
    .[, t := k]

  opt_ir_V_data_ls[[paste(k)]] <- opt_ir_V_data

  #--------------------------
  # Estimate VF_t
  #--------------------------

  # k <- 1
  get_VF_gam <- function(condition) {
    temp_data <- eval(parse(text = paste("opt_ir_V_data[", condition, ",]")))

    temp_VF_gam <- gam(
      u_tot ~ s(AY_next, k = 25, bs = "bs") + s(RY_next, k = 20, bs = "bs") + ti(AY_next, RY_next, k = 12),
      data = temp_data
    )
    return(temp_VF_gam)
  }

  VF_t_gam_ls <- mclapply(VF_t_condition_ls, get_VF_gam, mc.cores = 3)
  names(VF_t_gam_ls) <- VF_t_name_ls
  
}

# /*----------------------------------*/
#' ## Add some information and save
# /*----------------------------------*/
opt_ir_V_data <- opt_ir_V_data_ls %>%
  rbindlist() %>%
  .[, irc := irc] %>%
  .[, ysr := temp_ysr] %>%
  .[, ri := temp_ri]

saveRDS(
  opt_ir_V_data, 
  here(paste0("./Results_TM/opt_ir_V_pw_", irc, "_ysr_", temp_ysr, "_ri_", temp_ri, ".rds"))
)
