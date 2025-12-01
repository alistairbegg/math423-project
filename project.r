## Load Dataset
file1 <- paste0("https://raw.githubusercontent.com/",
                "mcgillstat/regression/main/data/data-table-B3.csv")
b3 <- read.csv(file = file1)

## Q1
mult_model <- lm(y ~ x1 + x6, data = b3)

## Q2
a   <- anova(mult_model); a
SSR <- sum(a[1:(nrow(a)-1), "Sum Sq"])
SSE <- a[nrow(a), "Sum Sq"]
SST <- SSR + SSE

df_reg <- length(coef(mult_model)) - 1
df_res <- df.residual(mult_model)

MSR <- SSR / df_reg
MSE <- SSE / df_res
Fvalue <- MSR / MSE
pvalue <- pf(Fvalue, df_reg, df_res, lower.tail = FALSE)

data.frame(
  Source = c("Regression", "Residual", "Total"),
  DF     = c(df_reg, df_res, df_reg + df_res),
  SS     = c(SSR, SSE, SST),
  MS     = c(MSR, MSE, NA),
  F      = c(Fvalue, NA, NA),
  p.value= c(pvalue, NA, NA)
)

## Q3
simple_model <- lm(y ~ x1, data = b3)
s_simple <- summary(simple_model)
s_mult <- summary(mult_model)
r2_mult <- s_mult$r.squared
adj_r2_mult <- s_mult$adj.r.squared
r2_simple <- s_simple$r.squared
adj_r2_simple <- s_simple$adj.r.squared

## Q4
coef(s_mult)

## Q5
confint(mult_model, "x1", level=0.95)

## Q6
coef(s_mult)

## Q7
test <- data.frame(
  x1 = 275,
  x6 = 2
)
predict(mult_model, newdata = test, interval = "confidence", level = 0.95)

## Q8
predict(mult_model, newdata = test, interval = "prediction", level = 0.95)

## Q9
r <- resid(mult_model)
pr <- r/(1-lm.influence(mult_model)$hat)
ss_res <- sum(r^2)
press <- sum(pr^2)
SST <- sum((b3$y - mean(b3$y))^2)
r2_pred <- 1 - press/SST

## Q10
set.seed(1)
half_b3 <- b3[sample(nrow(b3), 16), ]
half_model <- lm(y ~ x1 + x6, data = half_b3)
r_half <- resid(half_model)
pr_half <- r_half / (1 - lm.influence(half_model)$hat)
PRESS_half <- sum(pr_half^2)
SST_half <- sum((half_b3$y - mean(half_b3$y))^2)
r2_pred_half <- 1 - PRESS_half/SST_half
