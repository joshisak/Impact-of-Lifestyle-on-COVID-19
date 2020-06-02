######################## Modeling - COVID 19 Lifestyle Factors ######################## 

# Data and Libraries
setwd("C:/Users/saksh/Desktop/Summer Semester/STT/Project")

options(scipen = 999)
data <- read.csv("Dataset for Modelling.csv")
names(data)

# Outcome variable
summary(data$standardized_deaths)
str(data)

data_c <- data[,!colnames(data) %in% c("State","County","Presence.of.Water.Violation")]
corr <- round(cor(data_c[,c(ncol(data_c),1:(ncol(data_c)-1))]),3)

corr[corr>0.65]
corr[corr<(-0.65)]

data3 <- data

data3$State <- NULL
data3$County <- NULL
data3$X <- NULL

## Modeling ------------------

# Forward Selection with AIC
biggest <- formula(lm(standardized_deaths ~ ., data = data3))
step1 <- step(lm(standardized_deaths ~ 1, data = data3), direction = "forward", scope = biggest, trace = 0)
summary(step1)

# Backward Elimination with AIC
step2 <- step(lm(standardized_deaths ~ ., data = data3), direction = "backward", trace = 0)
summary(step2)

# Forward Selection with BIC
n <- nrow(data3)
biggest <- formula(lm(standardized_deaths ~ .,data = data3))
step3 <- step(lm(standardized_deaths ~ 1, data = data3), direction = "forward", scope = biggest, k = log(n), trace = 0)
summary(step3)

# Backward Elimination with BIC
step4 <- step(lm(standardized_deaths ~ ., data = data3), direction = "backward", k = log(n), trace = 0)
summary(step4)

# Calculate PRESS
Formulas <- c(as.formula(step1),as.formula(step2),as.formula(step3),as.formula(step4))
PRESS <- vector()
for(i in 1:length(Formulas)){
  mod1 <- lm(as.formula(Formulas[[i]]), data = data3)
  PRESS[i] <- sum((resid(mod1)/(1 - lm.influence(mod1)$hat))^2)
}

mods <- c("step1","step2","step3","step4")
rbind(mods,PRESS) 

# Calculate Cp values
fullmod <- lm(standardized_deaths ~ ., data = data3)
myANOVA <- anova(fullmod)
dim(myANOVA)
MSE <- myANOVA$`Mean Sq`[32]
p <- vector()
SSE <- vector(); Cp=vector()
for(i in 1:length(Formulas)){
  mod1 <- lm(as.formula(Formulas[[i]]), data = data3)
  p[i] <- length(mod1$coefficients)
  SSE[i] <- sum((mod1$residuals)^2)
  Cp[i] <- SSE[i]/MSE-(nrow(data3)-2*p[i])
}
rbind(mods,p,Cp) # Step 3 is the best as of right now

# Use regsubsets
library(leaps)
regsubsets.out <- regsubsets(standardized_deaths ~ ., data = data3, nbest = 2, nvmax = 10)
summary1 <- summary(regsubsets.out)
summary1


Formulas <- unlist(lapply(1:nrow(summary1$outmat), function(i) {
  temp <- summary1$outmat[i, ]
  vars <- names(temp)[temp == "*"]
  paste("standardized_deaths ~", paste(vars, collapse = " + "))
}))
Formulas <- c("standardized_deaths ~ 1", Formulas)

fullmod <- lm(standardized_deaths ~ ., data = data3)
myANOVA <- anova(fullmod)
dim(myANOVA)
MSE <- myANOVA$`Mean Sq`[32]
p <- vector()
R2adj <- vector(); aic <- vector(); bic <- vector(); R2=vector(); PRESS <- vector(); SSE <- vector(); Cp=vector()
for(i in 1:length(Formulas)){
  mod1 <- lm(as.formula(Formulas[[i]]), data = data3)
  p[i] <- length(mod1$coefficients)
  R2adj[i] <- summary(mod1)$adj.r.squared
  R2[i] <- summary(mod1)$r.squared
  aic[i] <- AIC(mod1)
  bic[i] <- BIC(mod1)
  PRESS[i] <- sum((resid(mod1)/(1 - lm.influence(mod1)$hat))^2)
  SSE[i] <- sum((mod1$residuals)^2)
  Cp[i] <- SSE[i]/MSE-(nrow(data3)-2*p[i])
}
Selection.Criteria <- data.frame(p, R2, SSE, R2adj, aic, bic, PRESS, Cp)
Selection.Criteria

maxp <- unique(p)
maxR2 <- vector(); maxR2adj <- vector(); maxSSE <- vector(); maxaic <- vector(); maxbic <- vector(); maxPRESS <- vector(); maxCp <- vector()
for(i in 1:length(maxp)){
  temp <- Selection.Criteria[Selection.Criteria$p == i, ]
  maxR2[i] <- max(temp$R2)
  maxR2adj[i] <- max(temp$R2adj)
  maxSSE[i] <- min(temp$SSE)
  maxaic[i] <- min(temp$aic)
  maxbic[i] <- min(temp$bic)
  maxPRESS[i] <- min(temp$PRESS)
  maxCp[i] <- min(temp$Cp)
}

Criteria <- c("SSE", "R2adj", "aic", "bic", "PRESS", "Cp")
library(ggplot2)
for (i in 1:6) {
  plot.data <- data.frame(p, "value" = Selection.Criteria[, i + 2])
  line.data <- data.frame(maxp, "value" = get(paste("max", Criteria[i], sep = "")))
  assign(paste("p", i, sep = ""), 
         ggplot() + geom_point(data = plot.data, aes(x = p, y = value)) +
           geom_line(data = line.data, aes(x = maxp, y = value)) +
           labs(y = Criteria[i]))
}
library(cowplot)
plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2)


# Final Model 

Selection.Criteria
FinalModel <- lm(as.formula(Formulas[[10]]), data = data3)
summary(FinalModel)
BIC(FinalModel)

# Anova
anova(FinalModel)

# F-test
p <- length(coef(FinalModel))
MSR <- sum((FinalModel$fitted.values - mean(data3$standardized_deaths))^2)/(p-1)
MSE <- sum(FinalModel$residuals^2)/(nrow(data3)-p)
Fstar <- MSR/MSE
Fstar

Fcrit <- qf(1-.05, p-1 , nrow(data3)-p)
Fcrit

pval <- 1-pf(Fstar, p-1, nrow(data3)-p)
pval

# Anova comparison
nullmod <- lm(standardized_deaths ~ 1, data = data3)
anova(nullmod, FinalModel)

par(mfrow = c(1,2))  # same oma as above
plot(FinalModel, which = 1:2)




# Data Transformation

as.character(as.formula(FinalModel)[3])

lambda <- c(.3, .4, .5, .6, .7)
SSE <- vector()
for(l in lambda){
  ytemp <- data3$standardized_deaths^l
  modtemp <- lm(ytemp ~ Population + Average.Daily.PM2.5 + HIV.Prevalence.Rate + Percentage_.Not.Proficient.in.English + Percentage_Female, data = data3)
  SSEtemp <- sum((data3$standardized_deaths-modtemp$fitted.values^(1/l))^2)
  SSE <- c(SSE, SSEtemp)
}
rbind(lambda,SSE)

data4 <- subset(data3, data3$standardized_deaths > 0)
library(MASS)

FinalModel2 <- lm(as.formula(Formulas[[10]]), data = data4)
summary(FinalModel2)
bc <- boxcox(FinalModel2) # No need of transformation

as.formula(Formulas[[8]])
as.formula(Formulas[[10]])
as.formula(Formulas[[12]])

lm(as.formula(Formulas[[8]]), data = data3)
lm(as.formula(Formulas[[10]]), data = data3)
lm(as.formula(Formulas[[12]]), data = data3)


# Remove Outliers

# Identification of Outliers
data3.1 <- data3[c(-39,-84,-32),]

FinalModel2 <- lm(as.formula(Formulas[[10]]), data = data3.1)
summary(FinalModel2)
plot(FinalModel2, which = 1:2)

p_f <- length(FinalModel2$coefficients)
R2adj_f <- summary(FinalModel2)$adj.r.squared
R2_f <- summary(FinalModel2)$r.squared
aic_f <- AIC(FinalModel2)
bic_f <- BIC(FinalModel2)
PRESS_f <- sum((resid(FinalModel2)/(1 - lm.influence(FinalModel2)$hat))^2)
SSE_f <- sum((FinalModel2$residuals)^2)
MSE <- myANOVA$`Mean Sq`[32]
Cp_f <- SSE_f/MSE-(nrow(data3.1)-2*p_f)

Selection.Criteria2 <- data.frame(p_f, R2_f, SSE_f, R2adj_f, aic_f, bic_f, PRESS_f)
names(Selection.Criteria2) <- names(Selection.Criteria)[1:7]
Selection.Criteria2 <- rbind(Selection.Criteria[10,1:7],Selection.Criteria2)
Selection.Criteria2

# Prediction

predict = predict(FinalModel2,newdata = data.frame(Population = 1250843, Average.Daily.PM2.5 = 11.1,HIV.Prevalence.Rate = 183.8,
                                         Percentage_.Not.Proficient.in.English = 1.868834853,Percentage_Female = 50.99320919),interval = "prediction")
predict
