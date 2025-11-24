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
r2_mult <- s$r.squared
adj_r2_mult <- s$adj.r.squared
r2_mult <- s_simple$r.squared
adj_r2_mult <- s_simple$adj.r.squared


