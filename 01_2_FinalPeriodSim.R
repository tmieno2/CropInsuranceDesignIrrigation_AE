# /*=================================================*/
#' # Preparation for simulation
# /*=================================================*/

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

# /*=================================================*/
#' # Calculate utility for the last/concurrent period
# /*=================================================*/

# /*----------------------------------*/
#' ## Simulate utility
# /*----------------------------------*/
#'
#' Notes: Simulate expected utility for various combinations of
#' state variables (AY and RY) and decision variable (smt and coverage level).
#' The resulting object will be used later in the dynamic simulation to recover profit
#' for each combination of smt-tau at a given level of AY and RY

pi_static <- 
lapply(
  AY_list_g, 
  function(x) find_u_T(x, data = base_data_s)
) %>%
rbindlist()

saveRDS(
  pi_static, 
  here(paste0("./Results_TM/pi_static_pw_", pw_per_acre_inch, ".rds"))
)

