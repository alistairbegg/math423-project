## Load Dataset
file1 <- paste0("https://raw.githubusercontent.com/",
                "mcgillstat/regression/main/data/data-table-B3.csv")
b3 <- read.csv(file = file1)

## Q1
mult_model <- lm(y ~ x1 + x6, data = b3)

## Q2
a <- anova(mult_model)
s <- summary(mult_model)
f <- s$fstatistic[1]
df1 <- s$fstatistic[2]
df2 <- s$fstatistic[3]
p <- pf(f, df1 = df1, df2 = df2, lower.tail = FALSE)

