# /*=================================================*/
#' # Impact of yield substitution
# /*=================================================*/
#' Simulated irrigation amount (under insurance)
ir_sim_ui <- here("Results/SimResultsSummaryUpdated.rds") %>%
  readRDS() %>%
  .[, .(
    ir = mean(ir),
    yield = mean(yield),
    AY = mean(AY_start),
    RY = mean(RY_start)
  ),
  by = .(irc, ysr, ri)
  ] %>%
  .[, type := "Under Insurance"]

#' Simulated irrigation amount (No insurance)
sim_ni <- here("Results/Sim_ni.rds") %>%
  readRDS() %>%
  setnames(
    c("e_ir", "e_yield", "ir_cost"),
    c("ir", "yield", "irc")
  ) %>%
  .[, type := "No Insurance"] %>%
  setnames("ir", "ir_ni")

tab_data_sub <- sim_ni[ir_sim_ui[ri == FALSE], on = "irc"] %>%
  .[, ir_dif := round(ir - ir_ni, digits = 2)] %>%
  .[, ir_ni := round(ir_ni, digits = 2)] %>%
  .[, ir := round(ir, digits = 2)] %>%
  .[, .(irc, ysr, ir, ir_ni, ir_dif)] %>%
  setorder(irc) %>%
  setnames(
    c("ir", "ir_ni", "irc", "ysr", "ir_dif"),
    c("With Insurance", "No Insurance", "Irrigation Cost", "Substitution Ratio", "Difference")
  )

table_ysr_impact <- kable(tab_data_sub, "latex", booktabs = T, caption = "The Impacts of Yield Substitution on Irrigation", escape = FALSE, align = "c") %>%
  kable_styling(latex_options = c("hold_position"), position = "center") %>%
  collapse_rows(columns = c(1, 4), latex_hline = "major", valign = "middle") %>%
  # collapse_rows(1:2, row_group_label_position='stack',latex_hline = 'custom',custom_latex_hline = 1) %>%
  # collapse_rows(4, valign = "middle", latex_hline='none') %>%
  add_header_above(c(" " = 2, "Irrigation (inches)" = 3)) %>%
  footnote(
    general = c(
      "This table presents the optimal irrigation amount under various levels of irrigation costs",
      'and substitution ratios. "With Insurance" and "No Insurance" columns present optimal',
      'irrigation with and without crop insurance, respectively. Finally, the "Difference" column',
      'presents the difference between the optimal irrigation amounts between the "With Insurance"',
      'and "No Insurance" cases'
    )
  )

# /*=================================================*/
#' # Impact of trend adjustment
# /*=================================================*/
#' No insurance
sim_ni <- here("Results/Sim_ni.rds") %>% 
  readRDS() %>%
  setnames(c("e_ir", "e_yield", "ir_cost"), c("ir", "yield", "irc")) %>%
  .[, type := "No Insurance"] %>%
  setnames("ir", "ir_ni")

tab_data_TA <- sim_ni[, .(ir_ni, irc)][sim_TA_summary, on = "irc"] %>%
  ir_sim_ui[ri == FALSE & ysr == 0.6, .(irc, ir_TA = ir)][., on = "irc"] %>%
  .[, .(TA = -TA, irc, ir, ir_TA, ir_ni)] %>%
  .[, ir_TA := round(ir_TA, digits = 2)] %>%
  .[, ir := round(ir, digits = 2)] %>%
  .[, ir_ni := round(ir_ni, digits = 2)] %>%
  setnames(
    c("TA", "ir_ni", "ir", "ir_TA", "irc"),
    c("TA factor", "No Insurance", "Without TA", "With TA", "Irrigation Cost")
  )

table_TA_impact <- kable(tab_data_TA, "latex", booktabs = T, caption = "The Impacts of Trend Adjustment on Irrigation", escape = FALSE, align = "c") %>%
  kable_styling(latex_options = c("hold_position"), position = "center") %>%
  collapse_rows(columns = c(1, 5), latex_hline = "major", valign = "middle", ) %>%
  add_header_above(c(" " = 2, "Irrigation (inches)" = 3)) %>%
  # pack_rows("Trend Adjustment Factor = 1", 1, 4) %>%
  # row_spec(4,hline_after=T) %>%
  # pack_rows("Trend Adjustment Factor = 2", 5, 8) %>%
  # row_spec(8,hline_after=T) %>%
  # pack_rows("Trend Adjustment Factor = 3", 9, 12) %>%
  # row_spec(12,hline_after=T) %>%
  # pack_rows("Trend Adjustment Factor = 4", 13, 16) %>%
  footnote(general = "Results are all based on the status quo yield substitution ratio of 0.6.")

# /*----------------------------------*/
#' ## Create a figure of indemnity payment
# /*----------------------------------*/

# data_indemnity_no_TA[, .N, by = irc]
# data_indemnity_TA[ysr == 0.6, .N, by = irc]

data_indemnity_no_TA <- sim_TA_results %>%
  .[, ysr := 0.6] %>%
  .[, indemnity_payment := pmax(AY_start * tau * pmax(hp, p_price) - yield * hp, 0)] %>%
  .[, .(indemnity_payment, irc, ysr)] %>%
  .[, type := "Without TA"]

data_indemnity_TA <- readRDS("Results/SimResultsSummaryUpdated.rds") %>%
  .[, indemnity_payment := pmax(AY_start * tau * pmax(hp, p_price) - yield * hp, 0)] %>%
  .[, .(indemnity_payment, irc, ysr)] %>%
  .[, type := "With TA"] %>%
  #--- make the number of observations the same ---#
  .[rep(1:.N, each = 2), ]

data_combined <- rbind(data_indemnity_no_TA, data_indemnity_TA)
data_combined_sum <- data_combined[, .(
  ip = mean(indemnity_payment),
  p0 = .SD[indemnity_payment == 0, .N] / .N
),
by = .(irc, type)
] %>%
  .[, mean_ip_txt := paste0("mean = ", round(ip, 2))] %>%
  .[, p0_txt := paste0("Pr(0) = ", round(p0, digits = 2))]

g_ip_TA <- data_combined %>%
  .[ysr == 0.6 & indemnity_payment != 0, ] %>%
  ggplot(data = .) +
  geom_histogram(aes(x = indemnity_payment)) +
  facet_grid(type ~ irc) +
  geom_text(
    data = data_combined_sum,
    aes(x = 800, y = 19000, label = mean_ip_txt),
    size = 3,
  ) +
  geom_text(
    data = data_combined_sum,
    aes(x = 750, y = 17500, label = p0_txt),
    size = 3,
  ) +
  ylab("Count") +
  xlab("Indemnity Payment ($/acre)")

ggsave(here("Figures/ir_dist_TA.pdf"), g_ip_TA, height = 6, width = 6.5)

# /*=================================================*/
#' # Impact of sufficient irrigation rule
# /*=================================================*/
tab_data_suf <- sim_ni[ir_sim_ui, on = "irc"] %>%
  .[ysr == 0.6, ] %>%
  .[, ri := ifelse(ri == TRUE, "Yes", "No")] %>%
  .[, ir_dif := round(ir - ir_ni, digits = 2)] %>%
  .[, ir_ni := round(ir_ni, digits = 2)] %>%
  .[, ir := round(ir, digits = 2)] %>%
  .[, .(irc, ri, ir, ir_ni, ir_dif)] %>%
  setorder(irc) %>%
  setnames(
    c("ir", "ri", "ir_ni", "irc", "ir_dif"),
    c("With Insurance", "Enforced", "No Insurance", "Irrigation Cost", "Difference")
  )

table_si_rule <- kable(tab_data_suf, "latex", booktabs = T, caption = "The Impact of the Sufficient Irrigation Requirement on Irrigation", escape = FALSE, align = "c") %>%
  kable_styling(latex_options = c("hold_position"), position = "center") %>%
  collapse_rows(columns = c(1, 4), latex_hline = "major", valign = "middle", ) %>%
  add_header_above(c(" " = 2, "Irrigation (inches)" = 3)) %>%
  footnote(general = "Results are all based on the status quo yield substitution ratio of 0.6.")
