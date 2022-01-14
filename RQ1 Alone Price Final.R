
library(beezdemand)
library(broom)
library(extrafont)
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

pureDemandFrame <- mSurveyResults.clean %>%
  select(c(ResponseId,
           '1_pureDemandTask_1',
           '2_pureDemandTask_1',
           '3_pureDemandTask_1',
           '4_pureDemandTask_1',
           '5_pureDemandTask_1',
           '6_pureDemandTask_1',
           '7_pureDemandTask_1',
           numChildren,
           gender,
           education)) %>%
  rename(`50` = `1_pureDemandTask_1`,
         `100` = `2_pureDemandTask_1`,
         `150` = `3_pureDemandTask_1`,
         `200` = `4_pureDemandTask_1`,
         `250` = `5_pureDemandTask_1`,
         `300` = `6_pureDemandTask_1`,
         `350` = `7_pureDemandTask_1`) %>%
  mutate(Task = "Demand-Alone")

pureDemandFrame.long <- pureDemandFrame %>%
  gather(Price, Consumption,
         `50`:`350`, -numChildren, -gender, -education, factor_key = TRUE) %>%
  mutate(Price = as.numeric(as.character(Price))) %>%
  mutate(Consumption = as.numeric(Consumption)) %>%
  mutate(education = recode(education,
                            `Master's degree (6)`                       = "Bachelors+",
                            `Bachelor's degree in college (4-year) (5)` = "Bachelors+",
                            `Associate degree in college (2-year) (4)`  = "NoBachelors",
                            `Some college but no degree (3)`            = "NoBachelors",
                            `High school graduate (high school diploma) (2)` = "NoBachelors",
                            `Less than high school degree (1)` = "NoBachelors")) %>%
  mutate(education = factor(as.character(education))) %>%
  mutate(FamilySize = ifelse(numChildren > 1, "Multiple", "Single")) %>%
  mutate(FamilySize = factor(FamilySize)) %>%
  mutate(gender = recode(gender,
                         `Man (cisgender)` = "Male",
                         `Woman (cisgender)` = "Female")) %>%
  mutate(gender = factor(as.character(gender))) %>%
  as.data.frame()

pureDemandGroupFrame <- pureDemandFrame.long %>%
  group_by(Price) %>%
  summarise(
    ConsumptionAve = mean(Consumption),
    ConsumptionMdn = median(Consumption)
  ) %>%
  ungroup()

out <- gnls(log((Consumption * 0.5) + ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
              (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
              (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
            data = pureDemandFrame.long,
            na.action = na.omit,
            params = list(q0    ~ gender + education + FamilySize,
                          alpha ~ gender + education + FamilySize),
            start = list( fixed = c(q0 = rep(10, 4),
                                    alpha = rep(0.001, 4))),
            weights = varPower())

pureDemandFrame.long$Task = as.factor(pureDemandFrame.long$Task)

firstModelTest <- nlme(log((Consumption * 0.5) +
                             ((0.5^2) * (Consumption^2) + 1)^0.5)/log(10) ~
                         (log((q0 * 0.5) + ((0.5^2) * (q0^2) + 1)^0.5)/log(10)) *
                         (exp(((-alpha)/(log((q0 * 0.5) + ((0.5^2) *
                                                             (q0^2) + 1)^0.5)/log(10))) * q0 * Price)),
                       data    = pureDemandFrame.long,
                       fixed   = list(
                         q0    ~ gender + education + FamilySize,
                         alpha ~ gender + education + FamilySize
                       ),
                       random  = list(pdDiag(q0 + alpha ~ 1)),
                       method  = "ML",
                       groups  = ~ ResponseId,
                       start   = coef(out),
                       control = list(msMaxIter = 5000,
                                      niterEM   = 5000,
                                      maxIter   = 5000,
                                      pnlsTol   = .1,
                                      tolerance = .1,
                                      apVar     = T,
                                      minScale  = .00001,
                                      opt       = "nlminb"),
                       verbose = F)

pureDemandFrame.long$pred  <- tn$inverse(predict(firstModelTest, level = 0))
pureDemandFrame.long$predi <- tn$inverse(predict(firstModelTest, level = 1))

pureDemandFrame.long = pureDemandFrame.long %>%
  mutate(FamilySize = factor(FamilySize,
                             levels = c("Single", "Multiple"),
                             labels = c("Single Child Household",
                                        "Multi Child Household"))) %>%
  mutate(education = factor(education,
                             levels = c("NoBachelors", "Bachelors+"),
                             labels = c("No College",
                                        "College")))

plot <- ggplot(pureDemandFrame.long, aes(Price, pred,
                                   lty   = gender)) +
  geom_line(size = 1) +
  geom_line(mapping = aes(Price, predi,
                          lty = gender,
                          group = ResponseId),
            size  = 0.4,
            alpha = 0.25) +
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
  ylab("Treatment Consumption (Hours)") +
  theme(legend.position  = "bottom",
        strip.background = element_blank(),
        strip.text       = element_text(colour = 'black',
                                        face   = 'bold'),
        text             = element_text(family = "Times New Roman",
                                        size   = 10),
        panel.spacing    = unit(1.25, "lines"),
        axis.title.y     = element_text(margin = margin(t = 0,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0)),
        axis.title.x     = element_text(margin = margin(t = 10,
                                                        r = 0,
                                                        b = 0,
                                                        l = 0))) +
  facet_grid(FamilySize ~ education) +
  beezdemand::theme_apa() +
  labs(lty="Gender")

plot

ggsave("Figure 1.png",
       plot = plot,
       units = "in",
       height = 6,
       width = 9, dpi = 600)
