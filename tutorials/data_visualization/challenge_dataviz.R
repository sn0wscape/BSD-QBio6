library(ggplot2)
p1 <- ggplot(pheno, aes_string(x = "TA")) +
  geom_histogram(binwidth = 15)

#This needs to be corrected to better specify outliers
length(which(pheno$TA < 40))

#This too
pheno_out_rm <- subset(pheno, TA > 40)

lm1 <- lm(tibia ~ TA, pheno)
lm2 <- lm(tibia ~ TA, pheno_out_rm)

p2 <- ggplot(pheno, aes_string(x = "TA", y = "tibia")) +
  geom_point() +
  geom_abline(color = "blue", intercept = coef(lm1)[1], slope = coef(lm1)[2])

p3 <- ggplot(pheno_out_rm, aes_string(x = "TA", y = "tibia")) +
  geom_point() +
  geom_abline(color = "blue", intercept = coef(lm2)[1], slope = coef(lm2)[2])

print(lm1)
print(lm2)


p4 <- ggplot(pheno, aes_string(x = "AvToneD3")) +
  geom_histogram(binwidth = 15) #The distribution is not normal

#Creating a backup of the pheno dataset
pheno2 <- pheno

#logit
logit <- function(x) log((x + 0.001) / (1 - x + 0.001))

#Convert values with logit
pheno2$AvToneD3 <- logit(pheno2$AvToneD3)

#Re-do the p4 histogram with for converted values
p5 <- ggplot(pheno2, aes_string(x = "AvToneD3")) +
  geom_histogram(binwidth = 15)

#Boxplot
p6 <- ggplot(pheno2, aes_string(x = "FCbox", y = "AvToneD3")) +
  geom_boxplot()
