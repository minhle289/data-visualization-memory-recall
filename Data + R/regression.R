# gap = normal + explainOnly + predictOnly + predictExplain + spatail + + i.id + i.race
install.packages("lme4")
install.packages("lmerTest")
install.packages("MuMIn")

library(lmerTest)
library(lme4)
library(MuMIn)
library(dotwhisker)
library(broom)
library(dplyr)

#Centering variables
center_scale <- function(x) {
  scale(x, scale = FALSE)
}


#================Normal versus Misleading================
normalMis <- read.csv("~/Downloads/norm.csv")
norm <- lmer(gap ~  misleading + center_scale(spatial) + (1|edu) + (1|id), data=normalMis)
summary(norm)

norm <- lme4::lmer(gap ~  misleading + center_scale(spatial) + (1|edu) + (1|id), data=normalMis)
tidynorm <-tidy(norm)
tidynorm <- filter(tidynorm, term != "sd_(Intercept).edu" 
                  & term != "(Intercept)"
                  & term != "sd_(Intercept).id" 
                  & term != "sd_Observation.Residual"
                  & term != "center_scale(spatial)") 
dwplot(tidynorm, conf.level = .95) + scale_y_discrete(labels=c("Baseline-Misleading"))+ xlab("Coefficient Estimate") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)


#ABSOLUTE ERROR
#================Absolute error for normal charts================
absolute <- read.csv("~/Downloads/absolute.csv")

# gap = normal + explainOnly + predictOnly + predictExplain + spatial + + i.id + i.region
absModel <- lmer(gap ~  explainonly + predictonly + predictexplain+ center_scale(spatial)  + (1|region) + (1|id), data=absolute)
summary(absModel)

# regress again for plotting
absModel <- lme4::lmer(gap ~  explainonly + predictonly + predictexplain+ center_scale(spatial)  + (1|region) + (1|id), data=absolute)
tidyreg <-tidy(absModel)
tidyreg <- filter(tidyreg, term != "sd_(Intercept).age" 
                   & term != "(Intercept)"
                   & term != "sd_(Intercept).id" 
                   & term != "sd_Observation.Residual") 
dwplot(tidyreg, conf.level = .95) + scale_y_discrete(labels=c("Spatial Ability", "Predict-Explain", "Predict-Only",  "Explain-Only"))+ xlab("Coefficient Estimate") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)

#================Absolute error for Misleading================
misleading <- read.csv("~/Downloads/misleading.csv")
misleadModel <- lmer(gap ~ mispe + center_scale(spatial) +(1|region) + (1|id), data=misleading)
summary(misleadModel)

misleadModel <- lme4::lmer(gap ~ misPE + center_scale(spatial) + (1|region) + (1|id), data=misleading)
tidyMislead <- tidy(misleadModel)
tidyMislead <- filter(tidyMislead, term != "sd_(Intercept).age" 
                   & term != "(Intercept)"
                   & term != "sd_(Intercept).id" 
                   & term != "sd_Observation.Residual")
dwplot(tidyMislead, conf.level = .95) + scale_y_discrete(labels=c("Spatial Ability", "Predict-Explain", "Predict-Only",  "Explain-Only"))+ xlab("Coefficient Estimate") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)


## RELATIVE ERROR
#================Relative error for normal charts================
relModel <- lmer(trend2 ~  explainonly + predictonly + predictexplain+ center_scale(spatial)  + (1|region) + (1|id), data=absolute)
summary(relModel)

relModel <- lme4::lmer(trend2 ~  explainonly + predictonly + predictexplain+ center_scale(spatial)  + (1|region) + (1|id), data=absolute)
tidyRel <- tidy(relModel)
tidyRel <- filter(tidyRel, term != "sd_(Intercept).age" 
                      & term != "(Intercept)"
                      & term != "sd_(Intercept).id" 
                      & term != "sd_Observation.Residual")
dwplot(tidyRel, conf.level = .95) + scale_y_discrete(labels=c("Spatial Ability", "Predict-Explain", "Predict-Only",  "Explain-Only"))+ xlab("Coefficient Estimate") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)

#================Relative error for misleading charts================
relModel2 <- lmer(trend2 ~  mispe + center_scale(spatial) + (1|region) + (1|id), data=misleading)
summary(relModel2)

relModel2 <- lme4::lmer(trend2 ~  misPE + center_scale(spatial) + (1|region) + (1|id), data=misleading)
tidyRel2 <- tidy(relModel2)
tidyRel2 <- filter(tidyRel2, term != "sd_(Intercept).age" 
                  & term != "(Intercept)"
                  & term != "sd_(Intercept).id" 
                  & term != "sd_Observation.Residual")
dwplot(tidyRel2, conf.level = .95) + scale_y_discrete(labels=c("Spatial Ability", "Predict-Explain", "Predict-Only",  "Explain-Only"))+ xlab("Coefficient Estimate") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)

#================Learning gain================
learning <- read.csv("~/Downloads/learning.csv")
learnGain <- lm(gain ~ sgroup2 + sgroup3 + sgroup4 + sgroup5 + edu, data=learning)
summary(learnGain)

dwplot(learnGain, conf.level = .95) + scale_y_discrete(labels=c("Education", "Spatial 8-10", "Spatial 6-7", "Spatial 4-5",  "Spatial 2-3"))+ xlab("Coefficient Estimate") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2)
