library("readxl")
require(sqldf)
require(car)
setwd("C:/Users/saksh/Desktop/Summer Semester/STT/Project")
#-----------------------Data Collection--------------------------------
#Michigan Covid excel has data regarding number of covid cases and population of each county \
#This data is merged into 1 dataframe and then the output variable "standardized deaths" is calculated
#This data is then included in the michigan county lifestyle data
#----------------------------------------------------------------------

states = c("Michigan","Louisiana","Rhode Island","District of Columbia")
coviddata = "Covid dataset.csv"
lifestyle ="All County Lifestyle dataset.csv"

data <- read.csv(coviddata)  #reading population data of a state countywise
lifestyle <- read.csv(lifestyle)  #reading the county lifestyle data
df <- data.frame()
for (state in states){
    df1 <- data[data$State == state,]
    df2 <- lifestyle[lifestyle$State == state,]
    
    #manipulating the data
    df1$standardized_cases <- (df1$Confirmed_cases/df1$Population)*1000  #finding number of covid cases per 1000 county population
    df1$standardized_deaths <- df1$deaths/df1$standardized_cases  #finding number of deaths in proportion to number of covid case per 1000 population
    df1$standardized_deaths[is.na(df1$standardized_deaths)] <- 0  #filling empty values with 0
    
    df3 = sqldf("select l.*, d.Population, d.standardized_deaths from df2 as l, df1 as d on l.County = d.County")  #joining the covid data with the lifestyle data
    df = rbind(df,df3)
    #file <- paste(state, "-data.csv",sep="")
    write.csv(df,"Lifestyle Covid dataset.csv")  #saving the final data of a state
  }


#######################################################################
#-----------------------Data Cleaning ----------------------------------

data = read.csv("Lifestyle Covid dataset.csv")  
data$X <- NULL
df <- data.frame()
str(data)
list = names(data)
list1 = c(71:87)
df = data[-list1]
df$CalField_1 <- NULL
df$CalField_2 <- NULL
df$CalField_3 <- NULL
df$Other.Primary.Care.Provider.Rate <- as.integer(df$Other.Primary.Care.Provider.Rate)

#NAs
df_con = df[,!names(df) %in% c("State","County","Presence.of.Water.Violation")]
for (i in 1:length(names(df_con))){
  #print(i)
  mean = mean(df_con[,i],na.rm=TRUE)
  df_con[is.na(df_con[,i]), i] = mean
}
df_con = cbind(df$State,df$County,df$Presence.of.Water.Violation,df_con)
names(df_con)[names(df_con) == "df$State"] <- "State"
names(df_con)[names(df_con) == "df$County"] <- "County"
names(df_con)[names(df_con) == "df$Presence.of.Water.Violation"] <- "Presence.of.Water.Violation"
write.csv(df_con,"Cleaned dataset.csv")



###################################################################################################
#-----------------------------Multicolinearityy----------------------------------------------------


d <- read.csv("Cleaned dataset.csv")#),na.strings = c("", "NA", "#N/A"))
d$X <- NULL
#multicollinearity and VIF

X=cbind(d[,!colnames(d) %in% c("State","Presence.of.Water.Violation","County")]) #"standardized_deaths"

mod = lm(standardized_deaths~.,data = X)
list = car::vif(mod)
list=list[list<10]
mcv_results=data.frame(VIF=list)

parameters = rownames(mcv_results)
df<- data.frame()
df <- d[,colnames(d) %in% parameters]
df <- cbind(d[,colnames(d) %in% c("standardized_deaths","State","Presence.of.Water.Violation","County")],df)

# Correlation Analysis
data4.1 <- df[,!colnames(df) %in% c("State","County","Presence.of.Water.Violation")]
corr <- round(cor(data4.1[,c(ncol(data4.1),1:(ncol(data4.1)-1))]),2)
corr <- data.frame(corr)

df<- df[,!colnames(df) %in% c("Child.Mortality.Rate",
                                     "Firearm.Fatalities.Rate",
                                     "Percentage_With.Annual.Mammogram",
                                     "Homicide.Rate")]

write.csv(df,"Dataset for Modelling.csv")

