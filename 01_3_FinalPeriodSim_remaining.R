# /*=================================================*/
#' # Preparation for simulation
# /*=================================================*/
# /*----------------------------------*/
#' ## Identify AY-RY combinations that are missing
# /*----------------------------------*/
observed_combs <- temp_pi_static %>%
  unique(., by = c("AY", "RY")) %>%
  .[, .(AY, RY, tau)] %>%
  .[, id := 1:nrow(.)]

missing_combs <- observed_combs[AY_RY_tau_ls_g, on = c("AY", "RY")] %>%
  .[is.na(id), ] %>%
  unique(., by = c("AY", "RY")) %>%
  .[, .(AY, RY)]

missing_AY_ls <- missing_combs[, AY] %>% unique()

# /*----------------------------------*/
#' ## Define economic variables
# /*----------------------------------*/
# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Raw revenue
# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' actual price assumed to be hp
base_data_s[, raw_revenue := hp * yield * acres / 1000]

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Irrigation cost
# /*~~~~~~~~~~~~~~~~~~~~~~*/
base_data_s[, ir_cost := acres * pw_per_acre_inch * ir / 1000]
base_data_s[, ir_cost := ir_cost - max(ir_cost)]

# /*----------------------------------*/
#' ## Simulate utility
# /*----------------------------------*/
#'
#' Notes: Simulate expected utility for various combinations of
#' state variables (AY and RY) and decision variable (smt and coverage level).
#' The resulting object will be used later in the dynamic simulation to recover profit
#' for each combination of smt-tau at a given level of AY and RY

pi_static_residulas <- 
lapply(
  missing_AY_ls, 
  function(x) find_u_T(x, data = base_data_s)
) %>%
rbindlist()

# /*----------------------------------*/
#' ## Combine the two and save
# /*----------------------------------*/
saveRDS(
  rbind(temp_pi_static, pi_static_residulas),
  paste0("./Results_TM/pi_static_pw_", pw_per_acre_inch, ".rds")
)
