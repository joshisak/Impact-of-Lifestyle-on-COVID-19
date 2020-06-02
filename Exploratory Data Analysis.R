######################## Data Exploration - COVID 19 Lifestyle Factor ######################## 

# Data and Libraries
setwd("C:/Users/saksh/Desktop/Summer Semester/STT/Project")
dir()

options(scipen = 999)
data <- read.csv("Dataset for Modelling.csv")[,-1]
names(data)

# Outcome variable
summary(data$standardized_deaths)

str(data)

# Delete outliers
summary(data$standardized_deaths)
threshold <- quantile(data$standardized_deaths,0.95)
data3 <- subset(data, data$standardized_deaths<threshold)
outliers <- subset(data, data$standardized_deaths>=threshold)

library(ggplot2)
d1=ggplot(data=data3,mapping=aes(x=State,y=standardized_deaths))+
  stat_boxplot()
d1

e1=ggplot(data4,aes(Percentage_Unemployed,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Unemployment Percentage (%)",y="Standarized Deaths",color="State"
  )
e1

# Correlation Analysis
data4.1 <- data3[,!colnames(data3) %in% c("State","County","Presence.of.Water.Violation",
                                          "Other.Primary.Care.Provider.Rate")]
corr <- round(cor(data4.1[,c(ncol(data4.1),1:(ncol(data4.1)-1))]),2)
corr <- data.frame(corr)
corr[corr>0.35]

# Population
summary(data3$Population)
e2=ggplot(data3,aes(Population,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Population",y="Standarized Deaths",color="State"
  )
e2

# Average.Traffic.Volume.per.Meter.of.Major.Roadways
summary(data3$Average.Traffic.Volume.per.Meter.of.Major.Roadways)
e3=ggplot(data3,aes(Average.Traffic.Volume.per.Meter.of.Major.Roadways,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Average.Traffic.Volume.per.Meter.of.Major.Roadways",y="Standarized Deaths",color="State"
  )
e3

# Median.Household.Income
summary(data3$Median.Household.Income)
e4=ggplot(data3,aes(Median.Household.Income,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Median Household Income",y="Standarized Deaths",color="State"
  )
e4

# Percentage_Vaccinated 
summary(data3$Percentage_Vaccinated )
e5=ggplot(data3,aes(Percentage_Vaccinated ,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Percentage Vaccinated ",y="Standarized Deaths",color="State"
  )
e5

# Percentage_Some.College 
summary(data3$Percentage_Some.College )
e6=ggplot(data3,aes(Percentage_Some.College ,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Percentage Some College ",y="Standarized Deaths",color="State"
  )
e6


# Negative Correlations
corr[corr<(-0.35)]

# Percentage_Rural
summary(data3$Percentage_Rural)
e7=ggplot(data3,aes(Percentage_Rural,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Percentage of Rural Pop.",y="Standarized Deaths",color="State"
  )
e7

# Clinical.Care_Z.Score
summary(data3$Clinical.Care_Z.Score)
e8=ggplot(data3,aes(Clinical.Care_Z.Score,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Clinical Care Z.Score",y="Standarized Deaths",color="State"
  )
e8

# Percentage_.65.and.over
summary(data3$Percentage_.65.and.over)
e9=ggplot(data3,aes(Percentage_.65.and.over,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Percentage 65 and over",y="Standarized Deaths",color="State"
  )
e9

# Percentage_.Enrolled.in.Free.or.Reduced.Lunch
summary(data3$Percentage_.Enrolled.in.Free.or.Reduced.Lunch)
e10=ggplot(data3,aes(Percentage_.Enrolled.in.Free.or.Reduced.Lunch,standardized_deaths))+
  geom_point(aes(color=State))+
  geom_smooth(method="lm",se=FALSE)+
  labs(x="Percentage Enrolled in Free or Reduced Lunch",y="Standarized Deaths",color="State"
  )
e10

