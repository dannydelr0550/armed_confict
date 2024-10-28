library(here)
library(plm)
library(texreg)

finaldata <- read.csv(here("output_files", "finaldata.csv"), header = TRUE)
finaldataforreg <- finaldata
finaldataforreg$pctpopdens <- finaldata$popdens / 100
finaldataforreg$loggdp <- log(finaldata$gdp1000)

preds <- as.formula(" ~ conflict + loggdp + OECD + pctpopdens + urban + 
                     agedep + male_edu + temp + rainfall1000 + earthquake + drought")

matmormod <- plm(update.formula(preds, matmort ~ .), data = finaldataforreg, index = c("ISO", "year"),
                 effect = "twoways", model = "within")
un5mormod <- plm(update.formula(preds, under5 ~ .), data = finaldataforreg, index = c("ISO", "year"),
                 effect = "twoways", model = "within")
infmormod <- plm(update.formula(preds, infant ~ .), data = finaldataforreg, index = c("ISO", "year"),
                 effect = "twoways", model = "within")
neomormod <- plm(update.formula(preds, neonatal ~ .), data = finaldataforreg, index = c("ISO", "year"),
                 effect = "twoways", model = "within")

htmlreg(list(matmormod, un5mormod, infmormod, neomormod),
        file = "table2.html",
        custom.model.names = c("Maternal Mortality", "Under-5 Mortality", "Infant Mortality", "Neonatal Mortality"),
        custom.coef.names = c("Conflict", "log(GDP)", "OECD", "Population Density (%)", "Urbanization", 
                              "Age Dependency", "Male Education", "Temperature", "Rainfall", 
                              "Earthquake", "Drought"),
        ci.force = TRUE,             
        ci.force.level = 0.95,         
        digits = 3)





