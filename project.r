## Load Dataset
file1 <- paste0("https://raw.githubusercontent.com/",
                "mcgillstat/regression/main/data/data-table-B3.csv")
b3 <- read.csv(file = file1)

## Q1
mult_model <- lm(y ~ x1 + x6, data = b3)

## Q2
a <- anova(mult_model)
MSR <- a[1, "Mean Sq"]
MSE <- a[nrow(a), "Mean Sq"]
f <- MSR / MSE
p <- pf(f, 2, df.residual(mult_model), lower.tail = FALSE)