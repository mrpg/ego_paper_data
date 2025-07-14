library(plm)

source("lib/preload.R")
source("lib/data.R")

# table 1 ("A2")

rt1_models <- list(
                   lm(coop ~ treat + round_, data = data35_main),
                   lm(coop ~ treat + round_, data = data4_main),
                   lm(coop ~ gpt4 + gpt4:treat + treat + round_, data = combined34_main)
)

rt1_se <- clustered(rt1_models, 2, ~ convo)
rt1_p <- clustered(rt1_models, 4, ~ convo)

rt1_coefs <- list(
                  "gpt4TRUE" = "GPT-4",
                  "treatenemy" = "Enemy",
                  "treatcompetition" = "Competition",
                  "gpt4TRUE:treatenemy" = "GPT-4 $\\times$ Enemy",
                  "gpt4TRUE:treatcompetition" = "GPT-4 $\\times$ Competition",
                  "round_" = "Round",
                  "(Intercept)" = "Constant"
)

rt1 <- tt(rt1_models,
          no_resize = T,
          placement = "[H]",
          caption = "Framing effect in GPT--GPT interaction",
          override.se = rt1_se,
          override.pvalues = rt1_p,
          custom.model.names = c("GPT-3.5", "GPT-4", "GPT-3.5 + GPT-4"),
               custom.coef.map = rt1_coefs,
               custom.gof.rows = list(
                                      "Outcome" = rep("Cooperated", length(rt1_models)),
                                      "Model" = rep("OLS", length(rt1_models)),
                                      "Standard errors" = rep("{\\small Clustered HC3}", length(rt1_models))
                                      ),
)


# table 2 ("A3")

tmpdata1 <- pdata.frame(filter(gptreact, treat == "base"), index = c("convo", "round_"))
tmpdata2 <- pdata.frame(filter(gptreact, treat == "enemy"), index = c("convo", "round_"))
tmpdata3 <- pdata.frame(filter(gptreact, treat == "competition"), index = c("convo", "round_"))

rt2_models <- list(
                   lm(coop ~ ldef + I(human * ldef) + lsdef + I(human * lsdef), data = tmpdata1),
                   plm(coop ~ ldef + I(human * ldef) + lsdef + I(human * lsdef), data = tmpdata1, model = "within"),
                   lm(coop ~ ldef + I(human * ldef) + lsdef + I(human * lsdef), data = tmpdata2),
                   plm(coop ~ ldef + I(human * ldef) + lsdef + I(human * lsdef), data = tmpdata2, model = "within"),
                   lm(coop ~ ldef + I(human * ldef) + lsdef + I(human * lsdef), data = tmpdata3),
                   plm(coop ~ ldef + I(human * ldef) + lsdef + I(human * lsdef), data = tmpdata3, model = "within")
)

rt2_se <- standard(rt2_models, 2)
rt2_p <- standard(rt2_models, 4)

rt2_se[c(1, 3, 5)] <- clustered(rt2_models[c(1, 3, 5)], 2, ~ convo)
rt2_p[c(1, 3, 5)] <- clustered(rt2_models[c(1, 3, 5)], 4, ~ convo)

rt2_coefs <- list(
                  "ldefTRUE" = "Second Mover just defected",
                  "I(human * ldef)" = "Second Mover human $\\times$ Second Mover just defected",
                  "lsdef" = "\\# Second Mover defections",
                  "I(human * lsdef)" = "Second Mover human $\\times$ \\# Second Mover defections",
                  "(Intercept)" = "Constant"
)

rt2 <- tt(rt2_models,
          override.se = rt2_se,
          override.pvalues = rt2_p,
          placement = "[H]",
          caption = "Effect of Second Mover defection on First Mover cooperation",
          custom.coef.map = rt2_coefs,
          custom.gof.rows = list(
                                 "Outcome" = rep("Cooperated", length(rt2_models)),
                                 "Subset" = rep("First Movers", length(rt2_models)),
                                 "Model" = rep(c("Pooled OLS", "Within"), 3),
                                 "Standard errors" = rep(c("{\\small Clustered HC3}", "{\\small Default}"), 3)
                                 ),
          custom.header = list("Baseline" = 1:2, "Enemy" = 3:4, "Competition" = 5:6)
)

# table 3 ("A4")

rt3_models <- list(
                   lm(coop ~ treat + round_, data = with_human_long_llm),
                   lm(coop ~ treat + round_, data = with_human_long_human)
)

rt3_se <- clustered(rt3_models, 2, ~ participant.label)
rt3_p <- clustered(rt3_models, 4, ~ participant.label)

rt3_coefs <- list(
                  "treatenemy" = "Enemy",
                  "treatcompetition" = "Competition",
                  "round_" = "Round",
                  "(Intercept)" = "Constant"
)

rt3 <- tt(rt3_models,
          no_resize = T,
          override.se = rt3_se,
          override.pvalues = rt3_p,
          placement = "[H]",
          caption = "Framing effect in GPT--human interaction",
          custom.model.names = c("GPT", "Human"),
               custom.coef.map = rt3_coefs,
               custom.gof.rows = list(
                                      "Outcome" = rep("Cooperated", length(rt3_models)),
                                      "Model" = rep("OLS", length(rt3_models)),
                                      "Standard errors" = rep("{\\small Clustered HC3}", length(rt3_models))
                                      ),

)

# table 4 ("A5")

rt4_models <- list(
                   lm(coop ~ I(treat != "base"), data = data35),
                   lm(coop ~ I(treat == "competition" | treat == "war" | treat == "procurement"), data = data35),
                   lm(coop ~ I(treat == "procurement"), data = data35),
                   lm(coop ~ I(treat != "base"), data = data4),
                   lm(coop ~ I(treat == "competition" | treat == "war" | treat == "procurement"), data = data4),
                   lm(coop ~ I(treat == "procurement"), data = data4)


)

rt4_se <- clustered(rt4_models, 2, ~ convo)
rt4_p <- clustered(rt4_models, 4, ~ convo)

rt4_coefs <- list(
                  'I(treat != "base")TRUE' = "Frame",
                  'I(treat == "competition" | treat == "war" | treat == "procurement")TRUE' = "Conflict",
                  'I(treat == "procurement")TRUE' = "Procurement",
                  "(Intercept)" = "Constant"
)

rt4 <- tt(rt4_models,
          override.se = rt4_se,
          override.pvalues = rt4_p,
          placement = "[H]",
          caption = "Framing effect of additional frames in GPT--GPT interaction",
          custom.model.names = rep(c("Model 1", "Model 2", "Model 3"), 2),
          custom.coef.map = rt4_coefs,
          custom.gof.rows = list(
                                 "Outcome" = rep("Cooperated", length(rt4_models)),
                                 "Model" = rep("OLS", length(rt4_models)),
                                 "Standard errors" = rep("{\\small Clustered HC3}", length(rt4_models))
                                 ),
          custom.header = list("GPT-3.5" = 1:3, "GPT-4" = 4:6)
)


# table 5 ("A6")

rt5_models <- list(
                   lm(coop ~ I(treat != "base"), data = bind_rows(with_human_long_human, with_human_long_llm))
)

rt5_se <- clustered(rt5_models, 2, ~ participant.label)
rt5_p <- clustered(rt5_models, 4, ~ participant.label)

rt5_coefs <- list(
                  'I(treat != "base")TRUE' = "Competitive frame",
                  "(Intercept)" = "Constant"
)

rt5 <- tt(rt5_models,
          no_resize = T,
          override.se = rt5_se,
          override.pvalues = rt5_p,
          placement = "[H]",
          caption = "Effect of a competitive frame in GPT--human interaction",
          custom.coef.map = rt5_coefs,
          custom.gof.rows = list(
                                 "Outcome" = rep("Cooperated", length(rt5_models)),
                                 "Model" = rep("OLS", length(rt5_models)),
                                 "Standard errors" = rep("{\\small Clustered HC3}", length(rt5_models))
                                 )
)

# table 6 ("A7")

rt6_models <- list(
                   lm(coop ~ human + treat + human:treat, data = filter(all_data_main, gpt4)) # only use GPT4-GPT4 or GPT4-human data
)

rt6_se <- clustered(rt6_models, 2, ~ convo)
rt6_p <- clustered(rt6_models, 4, ~ convo)

rt6_coefs <- list(
                  "humanTRUE" = "Human second mover",
                  "treatenemy" = "Enemy",
                  "treatcompetition" = "Competition",
                  "humanTRUE:treatenemy" = "Human second mover $\\times$ Enemy",
                  "humanTRUE:treatcompetition" = "Human second mover $\\times$ Competition",
                  "(Intercept)" = "Constant"
)

rt6 <- tt(rt6_models,
          no_resize = T,
          override.se = rt6_se,
          override.pvalues = rt6_p,
          placement = "[H]",
          caption = "Cooperation by treatment and dyad composition",
          custom.coef.map = rt6_coefs,
          custom.gof.rows = list(
                                 "Outcome" = rep("Cooperated", length(rt6_models)),
                                 "Model" = rep("OLS", length(rt6_models)),
                                 "Standard errors" = rep("{\\small Clustered HC3}", length(rt6_models))
                                 )
)

# save all tables

save_to("tex", rt1, rt2, rt3, rt4, rt5, rt6)
