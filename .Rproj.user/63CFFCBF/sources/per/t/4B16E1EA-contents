# Lab 3

## Lab 3.A(a): Linear leverage
library(tidyverse)
load("Data/Pb_all.rda")

# Model 1: Pb = B_0 + B_1(year-1975)
Pb_all_lm <- lm(Pb ~ I(log(year) - 1975), data = Pb_all)
Pb_all_sum <- summary(Pb_all_lm)
Pb_all_sum
head()
# Model 2: ln(Pb) = B_0 + B_1(year -1975)
logPb_all_lm <- lm(log(Pb) ~ I(year - 1975), data = Pb_all)
logPb_all_sum <- summary(logPb_all_lm)

# Plot the leverage against year and add horizontal lines at 1/n and 2(p+1)/n
glimpse(Pb_all)
# with 1/n and 2(p+1)/n horizontal lines:
pplus1 <- length(Pb_all_lm$coefficients)
n <- nobs(Pb_all_lm)
n
ppl
mean(Pb_all$year)
Pb_pred <- mutate(Pb_all,
                    yhat = predict(Pb_all_lm),
                    r = rstudent(Pb_all_lm),
                    v = hatvalues(Pb_all_lm),
                    D = cooks.distance(Pb_all_lm))
ggplot(Pb_pred, aes(x = year, y = v)) +
  geom_point(size = 3) +  # Ensure points are colored by region
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Pike: leverage Pb vs year",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Region") +  # Adjust legend title for clarity
  theme(legend.position = "bottom")


## Lab 3.A(b): Region leverage
# Västernorrland as reference
Pb_all <- mutate(Pb_all, region = relevel(region, "Vasternorrland"))
logPb_all <- mutate(Pb_all, log(Pb))
head(logPb_all)

logPb_pred <- mutate(logPb_all,
                    yhat = predict(logPb_allregion_lm),
                    r = rstudent(logPb_allregion_lm),
                    v = hatvalues(logPb_allregion_lm),
                    D = cooks.distance(logPb_allregion_lm))
head(logPb_pred)
glimpse(logPb_pred)
logPb_allregion_lm <- lm(log(Pb) ~ I(year - 1975) + region, data = Pb_all)
logPb_allregion_sum <- summary(logPb_allregion_lm)
logPb_allregion_sum
ggplot(logPb_pred, aes(x = year, y = v)) +
  geom_point(aes(color = region), size = 3) +  # Color points by region
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Leverage vs. Year",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")
pplus1_3Ab <- length(logPb_allregion_lm$coefficients)
pplus1_3Ab
n <- nobs(logPb_allregion_lm)
n


# Lab 3.A(c): studentized
# Calculate the studentized residuals
# Plot them against the linear predictor
highlightcolors <- c("|r*|>3" = "red")

ggplot(logPb_pred, aes(x = yhat, y = r)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_point(data = filter(logPb_pred, abs(r) > 3), 
             aes(color = "|r*|>3"), size = 3) +
  facet_wrap(~region) +
  labs(title = "Studentized residuals vs linear predictor",
       subtitle = "Log-lin model",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom")

num_large_residuals <- length(which(abs(logPb_pred$r) > 3))

print(num_large_residuals)


# Lab 3.A(f): Cook's distance
# Calculate Cook's distance and plot the, against year, separately for each region.
f1.Pb <- pplus1_3Ab
f2.Pb <- logPb_allregion_lm$df.residual
cook.limit.Pb <- qf(0.5, f1.Pb, f2.Pb)

f2.Pb
max(logPb_pred$D)
highlightshapes <- c("Cook's D>0.1" = 24)
ggplot(logPb_pred, aes(yhat, D)) + 
  geom_point(size = 2) +
  geom_hline(yintercept = cook.limit.Pb, color = "red") +
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +
  facet_wrap(~region) +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)

# Find the row with the maximum Cook's distance
max_D_row <- logPb_pred[which.max(logPb_pred$D), ]
print(max_D_row$region)

n
# Lab 3.A(g+h): DFBETAS
# Lab 3.A(g): influential observations
dfbetas <- data.frame(dfbetas(logPb_allregion_lm))
glimpse(dfbetas)
logPb_pred <- logPb_pred %>% mutate(dfbetas = dfbetas[,2])
head(logPb_pred)
ggplot(logPb_pred, aes(x = year, y = dfbetas)) +
  geom_point(size = 2) +
  geom_point(data = filter(logPb_pred, abs(r) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = filter(logPb_pred, D > 0.1),
             aes(shape = "Cook's D>0.1"),
             size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.Pb)*c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2/sqrt(n)*c(-1, 1),
             color = "red", linetype = "dashed") +
  facet_wrap(~region) +
  ylab("DFBETAS_1") +
  xlab("Year") +
  labs(title = "DFBETAS_1: impact on the slope",
       subtitle = "without the strange fish",
       caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)

# Lab 3.A(h): why?
# Plot the lead concentrations against year, 
# separately for each region and highlight the influential obervation in Västra Götaland.


ggplot(Pb_all, aes(x = year, y = Pb)) +
  geom_point(size = 4) +
  facet_wrap(~region)
xlab("Year") +
  ylab("Pb concentration") +
  labs(title = "Pb concentration changes over year")
