library(dplyr)
library(future)
library(here)
library(texreg)
library(mice)
library(plm)

finaldata <- read.csv(here("output_files", "finaldata.csv"))

finaldata$pctpopdens <- finaldata$popdens / 100
finaldata$loggdp <- log(finaldata$gdp1000)
finaldata <- select(finaldata, -popdens, -gdp1000)

mi_data <- finaldata %>% 
  mutate(ISO_numeric = as.numeric(as.factor(finaldata$ISO))) %>% 
  select(-country_name, -ISO, -region, -OECD2023)

#"dry run" to get the meth and pred objects
mice0  <- mice(mi_data, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "matmort", "infant", "neonatal", "under5", "loggdp", "pctpopdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix

#setting ISO as the class variable
pred[c("urban", "male_edu", "temp", "rainfall1000", "matmort", "infant", "neonatal", "under5", "loggdp", "pctpopdens"), "ISO_numeric"] <- -2

plan(multisession, workers = 7)  

mice.multi.out  <- mice(mi_data, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred, future = TRUE)
length(meth) == ncol(mi_data)  # Should return TRUE


fit.mi.matmort <- with(mice.multi.out, 
                      lm(matmort ~ -1 + conflict + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISO_numeric) + as.factor(year)))
fit.mi.infant <- with(mice.multi.out, 
                      lm(infant ~ -1 + conflict + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISO_numeric) + as.factor(year)))
fit.mi.neonatal <- with(mice.multi.out, 
                      lm(neonatal ~ -1 + conflict + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISO_numeric) + as.factor(year)))
fit.mi.under5 <- with(mice.multi.out, 
                      lm(under5 ~ -1 + conflict + loggdp + OECD + pctpopdens + urban + 
                           agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                           as.factor(ISO_numeric) + as.factor(year)))

out.matmor <- pool(fit.mi.matmort)
out.infmor <- pool(fit.mi.infant)
out.neomor<- pool(fit.mi.neonatal)
out.un5mor <- pool(fit.mi.under5)

#Complete
preds <- as.formula(" ~ conflict + loggdp + OECD + pctpopdens + urban + 
                     agedep + male_edu + temp + rainfall1000 + earthquake + drought")

matmormod <- plm(update.formula(preds, matmort ~ .), data = finaldata, index = c("ISO", "year"),
                 effect = "twoways", model = "within")
un5mormod <- plm(update.formula(preds, under5 ~ .), data = finaldata, index = c("ISO", "year"),
                 effect = "twoways", model = "within")
infmormod <- plm(update.formula(preds, infant ~ .), data = finaldata, index = c("ISO", "year"),
                 effect = "twoways", model = "within")
neomormod <- plm(update.formula(preds, neonatal ~ .), data = finaldata, index = c("ISO", "year"),
                 effect = "twoways", model = "within")

key <- list("Conflict" = "Armed conflict",
                 "loggdp" = "log(GDP)",
                 "OECD" = "OECD",
                 "pctpopdens" = "Population density",
                 "urban" = "Urban",
                 "agedep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "rainfall" = "Average rainfall",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")

htmlreg(list(matmormod, un5mormod, infmormod, neomormod, out.matmor, out.infmor, out.neomor, out.un5mor),
        file = "table3.html",
        custom.model.names = c("MI MM", "MI U5", "MI IM", "MI NE", "CC MM", "CC U5", "CC IM", "CC NE"),
        custom.coef.map = key,
        ci.force = TRUE,             
        ci.force.level = 0.95,         
        digits = 3)
