library(lme4)

plaque <- read.csv("C:/Users/sgtur/OneDrive - Clemson University/R - Fall 2024/STAT8030/In-Class examples/dna_plaque.csv")

plaque$Subject <- as.factor(plaque$Subject)
plaque$Analyst <- as.factor(plaque$Analyst)

plot(plaque$Analyst,plaque$Subect, xlab='Analyst',ylab='Subject')
#boxplot(plaque$~pres$Gauge, xlab='gauge',ylab='pressure')
lm.re <- lmer(DNAcontent~ Subject + (1|Analyst),data=plaque, REML = FALSE)

#fit full model with both random effects
lm.re.full <- lmer(DNAcontent ~ (1|Subject) + (1|Analyst), data = plaque, REML = FALSE)
summary(lm.re.full)
VarCorr(lm.re.full)

#Is there an effect of Subject? So you do the opposite one as the Random?
lm.red1 <- lmer(DNAcontent ~ (1|Analyst), data = plaque, REML = FALSE)
summary(lm.red1)

anova(lm.red1, lm.re.full)

#is there an effect of Analyst?
lm.red2 <- lmer(DNAcontent ~ (1|Subject), data = plaque, REML = FALSE)
anova(lm.red2, lm.re.full)


#===============================================

energy<- read.csv("C:/Users/sgtur/OneDrive - Clemson University/R - Fall 2024/STAT8030/In-Class examples/energy.csv")

pairs(energy[,1:10])
pairs(energy[,11:18])
pairs(energy[,19:27])

# correlation matrix
round(cor(energy),3)

# fit an lm with everything
lm.energy <- lm(Appliances ~ ., data = energy_data)
summary(lm.energy)

#Find VIF for TI: 91.7 Tdewpoint: 2093 or 2000
lm.t1 <- lm(T1~., data = energy[,-1])
summary(lm.t1)
1/(1-(0.9891)) #VIF for T1 #sub R^2 in for the 
#about 9 times bigger than if wasn't related to other regressors

lm.dewpoint <- lm(Tdewpoint ~., data= energy[,-1])
summary(lm.dewpoint)
1/(1-summary(lm.dewpoint)$r.squared)
#TI ~ All x's
#centering by subtracting mean from every regressor can help with collinearity
#also helpful for

#Ridge Regression
#alpha determines if use lasso or ridge. ridge just makes the coefficients closer to zero

