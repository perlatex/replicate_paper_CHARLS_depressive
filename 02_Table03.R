library(tidyverse)
library(gtsummary)
library(lavaan)
library(semTools)  
   

mydataIMP <- read_rds("mydataImp1.rds")


glimpse(mydataIMP)



#####################################################################
# Table 03
# Standardized factor loadings for the multigroup measurement model
# 注意
# urban (0=rural 1=urban)
# dc013, dc016 未使用
#####################################################################


cfamodel <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

"

# 朴素的方法

## step 1: fit the configural invariance model 
fit1 <- cfa(cfamodel, data = mydataIMP, 
            group = "urban_nbs")



## step 2: fit the weak invariance model 
fit2 <- cfa(cfamodel, data = mydataIMP, 
            group = "urban_nbs", 
            group.equal = "loadings")



## step 3: fit the strong invariance model 
fit3 <- cfa(cfamodel, data = mydataIMP, 
            group = "urban_nbs",  
            group.equal = c("loadings", "intercepts"))



fitMeasures(fit1, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))
fitMeasures(fit2, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))
fitMeasures(fit3, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))




# 偷懒的方法 
fit_mi <- semTools::measurementInvariance(
  model     = cfamodel, 
  data      = mydataIMP,
  group     = "urban_nbs",
  estimator = "ML"
)

comp <- compareFit(fit_mi$fit.configural, fit_mi$fit.loadings, fit_mi$fit.intercepts)


tab01 <- comp@fit %>% 
  as_tibble() %>% 
  mutate(term = "Measurement model") %>% 
  mutate(models = c("Configural invariant model", "Metric invariant model", "Scalar invariant model")) %>% 
  select(term, models, chisq, df, cfi, rmsea, rmsea.ci.lower, rmsea.ci.upper, rmsea.ci.level, srmr) 

##################################################################







##################################################################
semmodel_LPA_unconstr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  LPA        ~ facility
  IADL       ~ LPA + facility
  depression ~ IADL + LPA + facility

"

fit_sem_LPA_unconstr <- sem(semmodel_LPA_unconstr,
      data        = mydataIMP,
      group       = "urban_nbs",
      estimator   = "ML",
      mimic       = "Mplus"
)


semmodel_LPA_constr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  LPA        ~ facility
  IADL       ~ LPA + facility
  depression ~ c(a, a) * IADL + LPA + c(b, b) * facility

"


fit_sem_LPA_constr <- sem(semmodel_LPA_constr,
      data          = mydataIMP,
      group         = "urban_nbs",
      estimator     = "ML",
      mimic         = "Mplus"
)


##################################################################






##################################################################
semmodel_TPA_unconstr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  TPA        ~ facility
  IADL       ~ TPA + facility
  depression ~ IADL + TPA + facility

"

fit_sem_TPA_unconstr <- sem(semmodel_TPA_unconstr,
            data        = mydataIMP,
            group       = "urban_nbs",
            estimator   = "ML",
            mimic       = "Mplus")




semmodel_TPA_constr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  TPA        ~ facility
  IADL       ~ c(a, a) * TPA + facility
  depression ~ c(b, b) * IADL + c(d, d) * TPA + c(e, e) * facility

"

fit_sem_TPA_constr <- sem(semmodel_TPA_constr,
            data          = mydataIMP,
            group         = "urban_nbs",
            estimator     = "ML",
            mimic         = "Mplus")


##################################################################








##################################################################
# 致敬马保国先生的“接、化、发”

indices <- c("chisq", "df", "cfi", "rmsea", 
             "rmsea.ci.lower", "rmsea.ci.upper", 
             "rmsea.ci.level", "srmr")

tab02 <- lst(
  fit_sem_LPA_unconstr, fit_sem_LPA_constr, 
  fit_sem_TPA_unconstr, fit_sem_TPA_constr
  ) %>% 
  map(
    ~ .x %>% 
      fitMeasures(indices) %>% 
      as_tibble_row()
  ) %>% 
  list_rbind(names_to = "models") %>%  
  mutate(across(-models, as.double)) %>% 
  mutate(
    term = rep(c("Structural model (LTPA)", "Structural model (total PA)"), each = 2),
    .before = 1L
  )
  
  
bind_rows(tab01, tab02) %>% 
  flextable::as_grouped_data(groups = "term") %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::colformat_double(j = "df", digits = 0) %>% 
  flextable::autofit()
##################################################################


