
library(beezdemand)
library(broom)
library(emmeans)
library(ggplot2)
library(jtools)
library(kableExtra)
library(knitr)
library(nlme)
library(scales)
library(tidyverse)

tn <- trans_new("inhs",
                function(x) log((x * 0.5) + ((0.5^2) * (x^2) + 1)^0.5)/log(10),
                function(y) (1/10^(1*y))*((10^(2*y)) - 1),
                domain = c(0, Inf),
                breaks = c(0, 0.1, 1, 10, 100))

mSurveyResults <- readRDS("offlineDataETM.rds")

mSurveyResults.clean <- mSurveyResults %>%
  filter(numChildren > 0 & !is.na(dob) & Finished == TRUE)

strongMedDemandFrame.first <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_subTaskSM_1',
           '2_subTaskSM_1',
           '3_subTaskSM_1',
           '4_subTaskSM_1',
           '5_subTaskSM_1',
           '6_subTaskSM_1',
           '7_subTaskSM_1',
           numChildren,
           gender,
           education)) %>%
  rename(`50`   = `1_subTaskSM_1`,
         `100`  = `2_subTaskSM_1`,
         `150`  = `3_subTaskSM_1`,
         `200`  = `4_subTaskSM_1`,
         `250`  = `5_subTaskSM_1`,
         `300`  = `6_subTaskSM_1`,
         `350`  = `7_subTaskSM_1`) %>%
  mutate(Task = "Demand-Own [Moderate Available]")

strongLowDemandFrame.first <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_subTaskSW_1',
           '2_subTaskSW_1',
           '3_subTaskSW_1',
           '4_subTaskSW_1',
           '5_subTaskSW_1',
           '6_subTaskSW_1',
           '7_subTaskSW_1',
           numChildren,
           gender,
           education)) %>%
  rename(`50`   = `1_subTaskSW_1`,
         `100`  = `2_subTaskSW_1`,
         `150`  = `3_subTaskSW_1`,
         `200`  = `4_subTaskSW_1`,
         `250`  = `5_subTaskSW_1`,
         `300`  = `6_subTaskSW_1`,
         `350`  = `7_subTaskSW_1`) %>%
  mutate(Task = "Demand-Own [Weak Available]")

pureDemandFrame <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_subTaskSS_1',
           '2_subTaskSS_1',
           '3_subTaskSS_1',
           '4_subTaskSS_1',
           '5_subTaskSS_1',
           '6_subTaskSS_1',
           '7_subTaskSS_1',
           numChildren,
           gender,
           education)) %>%
  rename(`50` =  `1_subTaskSS_1`,
         `100` = `2_subTaskSS_1`,
         `150` = `3_subTaskSS_1`,
         `200` = `4_subTaskSS_1`,
         `250` = `5_subTaskSS_1`,
         `300` = `6_subTaskSS_1`,
         `350` = `7_subTaskSS_1`) %>%
  mutate(Task = "Demand-Own [Strong]")

combinedConsumptionFrame <- rbind(
  strongMedDemandFrame.first,
  strongLowDemandFrame.first,
  pureDemandFrame
)

combinedChoices.long <- combinedConsumptionFrame %>%
  gather(Price, Consumption,
         `50`:`350`, -numChildren, -gender, -education, factor_key = TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

globalDataAggragateFrame <- combinedChoices.long %>%
  group_by(Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption)
  ) %>%
  ungroup()

npar <- 3

out <- gnls(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
              (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
              (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
            data = combinedChoices.long,
            na.action = na.omit,
            params = list(q0 + alpha ~ Task),
            start = list( fixed = c(q0 = rep(10, 3),
                                    alpha = rep(0.0001, 3))),
            weights = varPower(),
            control = gnlsControl(msMaxIter = 200,
                                  maxIter = 300,
                                  opt = "optim"))

combinedChoices.long$Task = factor(combinedChoices.long$Task,
                                   levels = c("Demand-Own [Weak Available]",
                                              "Demand-Own [Moderate Available]",
                                              "Demand-Own [Strong]"),
                                   labels = c("Demand-Own [Weak Available]",
                                              "Demand-Own [Moderate Available]",
                                              "Demand-Own [High Available]"))

firstModelTest <- nlme(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~ (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) * (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
                       data    = combinedChoices.long,
                       fixed   = list(
                         q0    ~ Task,
                         alpha ~ Task
                       ),
                       random  = pdBlocked(list(
                         pdSymm(q0 + alpha ~ 1),
                         pdDiag(q0 + alpha ~ Task - 1)
                       )),
                       method  = "ML",
                       groups  = ~ ResponseId,
                       start   = coef(out),
                       control = list(msMaxIter = 5000,
                                      niterEM   = 5000,
                                      maxIter   = 5000,
                                      pnlsTol   = 1,
                                      tolerance = 1,
                                      apVar     = T,
                                      minScale  = .00001,
                                      opt       = "nlminb"),
                       verbose = F)

emmeaner <- emmeans(firstModelTest, ~ Task, param = c("q0"), transform = "response",
                    at = list(
                      Price = c(0)
                    ))

rg = ref_grid(firstModelTest, param = "q0")
rg.emm = emmeans(regrid(rg), "Task", at = list(Price = 0))
pairs(rg.emm)

rg = ref_grid(firstModelTest, param = "alpha")
rg.emm = emmeans(regrid(rg), "Task", at = list(Price = 0))
pairs(rg.emm)

combinedChoices.long$pred  <- tn$inverse(predict(firstModelTest, level = 0))
combinedChoices.long$predi <- tn$inverse(predict(firstModelTest, level = 1))

plot <- ggplot(combinedChoices.long, aes(Price, pred)) +
  geom_line(size = 1.75,
            color = "gray") +
  geom_line(size = 0.8,
            color = "gray",
            alpha = 0.25,
            mapping = aes(Price, predi,
                          group = interaction(Task, ResponseId))) +
  scale_x_continuous(breaks = c(0, 1, 10, 50, 100, 200, 400, 500, 1000, 5000, 10000),
                     labels = c(0, 1, 10, 50, 100, 200, 400, 500, 1000, 5000, 10000),
                     limits = c(50, 400),
                     trans = tn) +
  scale_y_continuous(breaks = c(0, 1, 5, 10, 20),
                     labels = c(0, 1, 5, 10, 20),
                     limits = c(0, 20),
                     trans = tn) +
  theme_bw() +
  scale_color_grey() +
  xlab("Price/Hour of Treatment") +
  ylab("Predicted Treatment Consumption") +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text       = element_text(colour = 'black',
                                        face   = 'bold'),
        text=element_text(family="Times New Roman", size=10)) +
  facet_wrap(~ Task) +
  beezdemand::theme_apa()

plot

# will add to plot in second file
save(plot, file = "demandPlot.RData")

