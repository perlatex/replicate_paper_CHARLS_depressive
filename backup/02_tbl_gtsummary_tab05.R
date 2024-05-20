library(tidyverse)
library(gtsummary)
library(lavaan)
library(semTools)  
   

mydataIMP <- read_rds("mydataImp1.rds") %>% 
  rename(facility = type_number)


glimpse(mydataIMP)




#####################################################################
library(processR)
labels = list(X = "facility", M1 = "LPA", M2 = "IADL", Y = "depression")
statisticalDiagram(6, labels = labels)
#####################################################################





#####################################################################
# Table 05
# Standardized factor loadings for the multigroup measurement model
# 注意
# urban (0=rural 1=urban)
# dc013, dc016 未使用
#####################################################################




##################################################################
semmodel_LPA1 <- "
  # measurement models
  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  # regression models
  LPA        ~ a1 * facility
  IADL       ~ a2 * facility + d21 * LPA
  depression ~ b1 * LPA + b2 * IADL + cprime * facility

  # effect values
  serial_indirect_eff := a1*d21*b2
  indirect_eff        := a1*d21*b2 + a1*b1 + a2*b2
  direct_eff          := cprime
  total_eff           := a1*d21*b2 + a1*b1 + a2*b2 + cprime

"


fit_sem_LPA1 <- sem(semmodel_LPA1,
                   data        = mydataIMP,
                   group       = "urban_nbs",
                   estimator   = "ML",
                   se          = "bootstrap",
                   bootstrap   = 1000
)


fit_sem_LPA1 %>%
  standardizedsolution() %>%
  filter(op == "~") %>%
  mutate(pvalue = gtools::stars.pval(pvalue)) %>%
  select(group, lhs, rhs, label, est.std, ci.lower, ci.upper) %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::hline(i = 6) %>%
  flextable::autofit()

##################################################################



##################################################################
semmodel_LPA2 <- "
  # measurement models
  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  # regression models
  LPA        ~ c(a1_x, a1_y) * facility
  IADL       ~ c(a2_x, a2_y) * facility + c(d21_x, d21_y) * LPA
  depression ~ c(b1_x, b1_y) * LPA + c(b2_x, b2_y) * IADL + c(cprime_x, cprime_y) * facility

  # effect values x
  serial_indirect_eff_x := a1_x*d21_x*b2_x
  indirect_eff_x        := a1_x*d21_x*b2_x + a1_x*b1_x + a2_x*b2_x
  direct_eff_x          := cprime_x
  total_eff_x           := a1_x*d21_x*b2_x + a1_x*b1_x + a2_x*b2_x + cprime_x

  # effect values y
  serial_indirect_eff_y := a1_y*d21_y*b2_y
  indirect_eff_y        := a1_y*d21_y*b2_y + a1_y*b1_y + a2_y*b2_y
  direct_eff_y          := cprime_y
  total_eff_y           := a1_y*d21_y*b2_y + a1_y*b1_y + a2_y*b2_y + cprime_y
"

fit_sem_LPA2 <- sem(semmodel_LPA2,
  data        = mydataIMP,
  group       = "urban_nbs",
  estimator   = "ML",
  se          = "bootstrap",
  bootstrap   = 1000
)


fit_sem_LPA2 %>%
  standardizedsolution() %>%
  filter(op == "~") %>%
  mutate(pvalue = gtools::stars.pval(pvalue)) %>%
  select(group, lhs, rhs, label, est.std, ci.lower, ci.upper) %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::hline(i = 6) %>%
  flextable::autofit()


fit_sem_LPA1 %>%
  standardizedsolution() %>%
  filter(op == "=~")

fit_sem_LPA2 %>%
  standardizedsolution() %>%
  filter(op == "=~")
##################################################################






##################################################################
semmodel_TPA1 <- "
  # measurement models
  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  # regression models
  TPA        ~ a1 * facility
  IADL       ~ a2 * facility + d21 * TPA
  depression ~ b1 * TPA + b2 * IADL + cprime * facility

  # effect values
  serial_indirect_eff := a1*d21*b2
  indirect_eff        := a1*d21*b2 + a1*b1 + a2*b2
  direct_eff          := cprime
  total_eff           := a1*d21*b2 + a1*b1 + a2*b2 + cprime

"


fit_sem_TPA1 <- sem(semmodel_TPA1,
  data        = mydataIMP,
  group       = "urban_nbs",
  estimator   = "ML",
  se          = "bootstrap",
  bootstrap   = 1000
)


fit_sem_TPA1 %>%
  standardizedsolution() %>%
  filter(op == "~") %>%
  mutate(pvalue = gtools::stars.pval(pvalue)) %>%
  select(group, lhs, rhs, label, est.std, ci.lower, ci.upper) %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::hline(i = 6) %>%
  flextable::autofit()



##################################################################







##################################################################
semmodel_TPA2 <- "
  # measurement models
  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  # regression models
  TPA        ~ c(a1_x, a1_y) * facility
  IADL       ~ c(a2_x, a2_y) * facility + c(d21_x, d21_y) * TPA
  depression ~ c(b1_x, b1_y) * TPA + c(b2_x, b2_y) * IADL + c(cprime_x, cprime_y) * facility

  # effect values x
  serial_indirect_eff_x := a1_x*d21_x*b2_x
  indirect_eff_x        := a1_x*d21_x*b2_x + a1_x*b1_x + a2_x*b2_x
  direct_eff_x          := cprime_x
  total_eff_x           := a1_x*d21_x*b2_x + a1_x*b1_x + a2_x*b2_x + cprime_x

  # effect values y
  serial_indirect_eff_y := a1_y*d21_y*b2_y
  indirect_eff_y        := a1_y*d21_y*b2_y + a1_y*b1_y + a2_y*b2_y
  direct_eff_y          := cprime_y
  total_eff_y           := a1_y*d21_y*b2_y + a1_y*b1_y + a2_y*b2_y + cprime_y
"


fit_sem_TPA2 <- sem(semmodel_TPA2,
                   data        = mydataIMP,
                   group       = "urban_nbs",
                   estimator   = "ML",
                   se          = "bootstrap",
                   bootstrap   = 1000
)


fit_sem_TPA2 %>%
  standardizedsolution() %>%
  filter(op == "~") %>%
  mutate(pvalue = gtools::stars.pval(pvalue)) %>%
  select(group, lhs, rhs, label, est.std, ci.lower, ci.upper) %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::hline(i = 6) %>%
  flextable::autofit()

##################################################################


