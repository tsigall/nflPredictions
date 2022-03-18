#Fit to a lm model
model <- lm(total ~
              a_orya +
              a_opya +
              a_ofum +
              a_oepa +
              a_oqepa +
              a_ocpoe +
              #a_ocmp +
              #a_oatt +
              #a_oyds +
              #a_otd +
              #a_oint +
              #a_orat +
              #a_oyatt +
              #a_oaya +
              a_drya +
              a_dpya +
              a_dfum +
              a_depa +
              a_dqepa +
              a_dcpoe +
              #h_orya +
              #h_opya +
              #h_ofum +
              #h_oepa +
              #h_oqepa +
              #h_ocpoe +
              #h_ocmp +
              #h_oatt +
              #h_oyds +
              #h_otd +
              #h_oint +
              #h_orat +
              #h_oyatt +
              #h_oaya +
              h_drya +
              h_dpya +
              h_dfum +
              h_depa +
              h_dqepa +
              h_dcpoe,
            data = modelData)
summary(model)
ols_step_both_p(model, details = TRUE, penter=0.1, prem=0.1)


stepwise.model <- lm(total ~ h_oaya + a_otd + h_ocpoe, data = modelData)
summary(stepwise.model)

