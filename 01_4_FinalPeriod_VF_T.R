# /*=================================================*/
#' # Find the value function at t=T
# /*=================================================*/
# /*----------------------------------*/
#' ## Data for value function estimation
# /*----------------------------------*/
pi_static <- paste0("./Results_TM/pi_static_pw_", irc, ".rds") %>%
  here() %>%
  readRDS()

VF_T_data <- pi_static %>%
  .[, .SD[e_u == max(e_u), ], by = c("AY", "RY")] %>%
  .[, smt := NULL] %>%
  .[, tau := NULL] %>%
  setnames(c("AY", "RY"), c("AY_next", "RY_next"))

# /*----------------------------------*/
#' ## 3D visualization
# /*----------------------------------*/
# library(lattice)
#
# temp_data <- VF_T_data[RY>0,]
#
# temp_plot <- wireframe(
#     temp_data[,e_u] ~ temp_data[,AY] * temp_data[,RY],
#     shade = TRUE,
#     aspect = c(1, 1) ,
#     light.source = c(10,10,10),
#     scales = list(z.ticks=5,arrows=FALSE, col="black", font=10, tck=0.5),
#     col.regions = colorRampPalette(c("blue", "pink"))(100)
#     )
#
# print(temp_plot)

# /*----------------------------------*/
#' ## Value function at T (V_T)
# /*----------------------------------*/
VF_T_gam_ls <- list()

# library(ggplot2)
# library(lattice)

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### RY > 150
# /*~~~~~~~~~~~~~~~~~~~~~~*/
VF_T_gam_ls[["RY_150up"]] <- gam(
  e_u ~ s(AY_next, k = 25, bs = "bs") + s(RY_next, k = 15, bs = "bs") + ti(AY_next, RY_next, k = 15),
  data = VF_T_data[RY_next >= 150, ]
)

# #=== the size of error ===#
# temp_data <- VF_T_data[RY_next>=150,]
# temp_data[,e_u_hat:=predict(VF_T_gam_ls[['RY_150up']],data=temp_data)]
# temp_data[,abs(e_u-e_u_hat)/e_u] %>% mean
#
# #=== visualization of the prediction ===#
# temp_plot <- wireframe(
#     temp_data[,e_u_hat] ~ temp_data[,AY_next] * temp_data[,RY_next],
#     # VF_T_data[RY_next>=50 & AY_next>=200,e_u] ~ VF_T_data[RY_next>=50 & AY_next>=200,AY_next] * VF_T_data[RY_next>=50 & AY_next>=200,RY_next],
#     shade = TRUE,
#     aspect = c(1, 1),
#     light.source = c(10,10,10),
#     scales = list(z.ticks=5,arrows=FALSE, col="black", font=10, tck=0.5),
#     col.regions = colorRampPalette(c("blue", "pink"))(100)
#     )
#
# print(temp_plot)
#
# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### 50 < RY < 150
# /*~~~~~~~~~~~~~~~~~~~~~~*/
VF_T_gam_ls[["RY_50up_150down"]] <- gam(
  e_u ~ s(AY_next, k = 25, bs = "bs") + s(RY_next, k = 15, bs = "bs") + ti(AY_next, RY_next, k = 15),
  data = VF_T_data[RY_next >= 50 & RY_next <= 150, ]
)

# #=== the size of error ===#
# temp_data <- VF_T_data[RY_next>=50 & RY_next<=150,]
# temp_data[,e_u_hat:=predict(VF_T_gam_ls[['RY_50up']],data=temp_data)]
# temp_data[,abs(e_u-e_u_hat)/e_u] %>% mean
#
# #=== visualization of the prediction ===#
# temp_plot <- wireframe(
#     temp_data[,e_u_hat] ~ temp_data[,AY_next] * temp_data[,RY_next],
#     # VF_T_data[RY_next>=50 & AY_next>=200,e_u] ~ VF_T_data[RY_next>=50 & AY_next>=200,AY_next] * VF_T_data[RY_next>=50 & AY_next>=200,RY_next],
#     shade = TRUE,
#     aspect = c(1, 1),
#     light.source = c(10,10,10),
#     scales = list(z.ticks=5,arrows=FALSE, col="black", font=10, tck=0.5),
#     col.regions = colorRampPalette(c("blue", "pink"))(100)
#     )
#
# print(temp_plot)
#
# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### RY <= 50
# /*~~~~~~~~~~~~~~~~~~~~~~*/
VF_T_gam_ls[["RY_50down"]] <- gam(
  e_u ~ s(AY_next, k = 25) + s(RY_next, k = 15) + ti(AY_next, RY_next, k = 15),
  data = VF_T_data[RY_next <= 50, ]
)

# temp_data <- VF_T_data[RY_next<=50 ,]
# temp_data[,e_u_hat:=predict(VF_T_gam_ls[['RY_50down']],data=temp_data)]
# temp_data[,abs(e_u-e_u_hat)/e_u] %>% mean
# temp_plot <- wireframe(
#     temp_data[,e_u_hat] ~ temp_data[,AY_next] * temp_data[,RY_next],
#     # VF_T_data[RY_next>=50 & AY_next>=200,e_u] ~ VF_T_data[RY_next>=50 & AY_next>=200,AY_next] * VF_T_data[RY_next>=50 & AY_next>=200,RY_next],
#     shade = TRUE,
#     aspect = c(1, 1),
#     light.source = c(10,10,10),
#     scales = list(z.ticks=5,arrows=FALSE, col="black", font=10, tck=0.5),
#     col.regions = colorRampPalette(c("blue", "pink"))(100)
#     )
#
# print(temp_plot)


# /*----------------------------------*/
#' ## Save
# /*----------------------------------*/
saveRDS(
  VF_T_gam_ls, 
  paste0("./Results_TM/VF_T_pw_", irc, ".rds")
)
