library(beezdemand)
library(broom)
library(dplyr)
library(emmeans)
library(geepack)
library(ggplot2)
library(ggsci)
library(kableExtra)
library(knitr)
library(lattice)
library(latticeExtra)
library(MuMIn)
library(scales)
library(tibble)
library(tidyr)

tn <- trans_new("inhs",
                function(x) log((x * 0.5) + ((0.5^2) * (x^2) + 1)^0.5)/log(10),
                function(y) (1/10^(1*y))*((10^(2*y)) - 1),
                domain = c(0, Inf),
                breaks = c(0, 0.1, 1, 10, 100))

mSurveyResults <- readRDS("offlineDataETM.rds")

mSurveyResults.clean <- mSurveyResults %>%
  filter(numChildren > 0 & !is.na(attn1_1) & !is.na(dob))

strongStrongDemandFrame.second <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_subTaskSS_2',
           '2_subTaskSS_2',
           '3_subTaskSS_2',
           '4_subTaskSS_2',
           '5_subTaskSS_2',
           '6_subTaskSS_2',
           '7_subTaskSS_2',
           numChildren,
           gender,
           education)) %>%
  rename(`50`   = `1_subTaskSS_2`,
         `100`  = `2_subTaskSS_2`,
         `150`  = `3_subTaskSS_2`,
         `200`  = `4_subTaskSS_2`,
         `250`  = `5_subTaskSS_2`,
         `300`  = `6_subTaskSS_2`,
         `350`  = `7_subTaskSS_2`) %>%
  mutate(Task = "SS")

strongStrongDemandFrame.second.long <- strongStrongDemandFrame.second %>%
  gather(Price, Consumption, `50`:`350`, -numChildren, -gender, -education, factor_key=TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

strongMedDemandFrame.second <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_subTaskSM_2',
           '2_subTaskSM_2',
           '3_subTaskSM_2',
           '4_subTaskSM_2',
           '5_subTaskSM_2',
           '6_subTaskSM_2',
           '7_subTaskSM_2',
           numChildren,
           gender,
           education)) %>%
  rename(`50`   = `1_subTaskSM_2`,
         `100`  = `2_subTaskSM_2`,
         `150`  = `3_subTaskSM_2`,
         `200`  = `4_subTaskSM_2`,
         `250`  = `5_subTaskSM_2`,
         `300`  = `6_subTaskSM_2`,
         `350`  = `7_subTaskSM_2`) %>%
  mutate(Task = "SM")

strongMedDemandFrame.second.long <- strongMedDemandFrame.second %>%
  gather(Price, Consumption, `50`:`350`, -numChildren, -gender, -education, factor_key=TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

strongLowDemandFrame.second <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_subTaskSW_2',
           '2_subTaskSW_2',
           '3_subTaskSW_2',
           '4_subTaskSW_2',
           '5_subTaskSW_2',
           '6_subTaskSW_2',
           '7_subTaskSW_2',
           numChildren,
           gender,
           education)) %>%
  rename(`50`   = `1_subTaskSW_2`,
         `100`  = `2_subTaskSW_2`,
         `150`  = `3_subTaskSW_2`,
         `200`  = `4_subTaskSW_2`,
         `250`  = `5_subTaskSW_2`,
         `300`  = `6_subTaskSW_2`,
         `350`  = `7_subTaskSW_2`) %>%
  mutate(Task = "SW")

strongLowDemandFrame.second.long <- strongLowDemandFrame.second %>%
  gather(Price, Consumption, `50`:`350`, -numChildren, -gender, -education, factor_key=TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  as.data.frame()

combinedFrame.long <- rbind(
  strongLowDemandFrame.second.long,
  strongMedDemandFrame.second.long,
  strongStrongDemandFrame.second.long
)

geeFittingFrame <- as_tibble(combinedFrame.long) %>%
  mutate(NewID = as.numeric(factor(ResponseId, levels = unique(ResponseId))),
         gender = factor(as.character(gender))) %>%
  mutate(Education = dplyr::recode(education,
                            `Professional degree (JD, MD) (8) `              = ">= 4 Yr College",
                            `Doctoral degree (7)`                            = ">= 4 Yr College",
                            `Master's degree (6)`                            = ">= 4 Yr College",
                            `Bachelor's degree in college (4-year) (5)`      = ">= 4 Yr College",
                            `Associate degree in college (2-year) (4)`       = "<= 2 Yr College",
                            `Some college but no degree (3)`                 = "<= 2 Yr College",
                            `High school graduate (high school diploma) (2)` = "No College",
                            `Less than high school degree (1)`               = "No College")) %>%
  mutate(Education = factor(Education,
                            levels = c("No College", "<= 2 Yr College", ">= 4 Yr College" ))) %>%
  mutate(familysize = ifelse(numChildren > 1, "Multiple", "Single")) %>%
  select(Task, NewID, Price, Consumption, gender, education, familysize) %>%
  arrange(Task, NewID, Price, Consumption, gender, education, familysize)

geeFittingFrame$Task <- factor(geeFittingFrame$Task,
                        levels = c("SS", "SM", "SW"),
                        labels = c("Demand-Own [High Available]",
                                   "Demand-Own [Moderate Available]",
                                   "Demand-Own [Weak Available]"))

fit.full <- geeglm(Consumption ~ Price * Task * gender * familysize,
                   id = NewID,
                   data = geeFittingFrame,
                   family = gaussian,
                   corstr = "exchangeable")

fit.1 <- geeglm(Consumption ~ Price * Task * gender,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.2 <- geeglm(Consumption ~ Price * Task,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

fit.3 <- geeglm(Consumption ~ Price + Task,
                id = NewID,
                data = geeFittingFrame,
                family = gaussian,
                corstr = "exchangeable")

model.sel(fit.full,
          fit.1,
          fit.2,
          fit.3,
          rank = "QIC")

emmeaner <- emmeans(fit.3, specs = pairwise ~ Task, type = "response",
                    at = list(Price = c(50,
                                        100,
                                        150,
                                        200,
                                        250,
                                        300,
                                        350)))

plotData <- emmip(fit.3, Task ~ Price,
                  CIs = TRUE,
                  type = "response",
                  plotit = FALSE,
                  at = list(Price = c(50,
                                      100,
                                      150,
                                      200,
                                      250,
                                      300,
                                      350)))

load("demandPlot.RData")

plot2 = plot +
  geom_ribbon(data = plotData,
              aes(ymin = yvar - 1.96*SE,
                  ymax = yvar + 1.96*SE,
                  x = Price),
              alpha = 0.4,
              fill = "black",
              inherit.aes = FALSE) +
  geom_line(data = plotData,
            color = "black",
            size = 1.5,
            mapping = aes(Price, yvar),
            inherit.aes = FALSE)

plot2

ggsave("Figure 2.png",
       plot = plot2,
       units = "in",
       height = 6,
       width = 9, dpi = 600)
