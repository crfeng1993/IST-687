install.packages("RJSONIO")
library(RJSONIO)
load("R-Project.RData")
load("Airdata.R")
hist(Sati_flight$Satisfaction,breaks = 10)
nrow(Sati_flight[Sati_flight$Satisfaction<1.5,])
Age_Sat <- lm(formula = Price.Sensitivity ~ .,data = Sati_flight)
summary(Age_Sat)


#Using Association rule modeling to refine to the existing model
#convert useful column into factors
levelit <- function(v){
  new.v <- v
  level4 <- quantile(v[v>0],probs = c(0.3,0.7))
  new.v[v<level4[1]] <- "Low"
  new.v[level4[1]<=v & v<level4[2]] <- "Average"
  new.v[level4[2]<=v & v<level4[3]] <- "High"
  new.v <- factor(new.v)
  return(new.v)
}
Airdata_new <- Airdata
Airdata_new$Satisfaction <- "Average"
Airdata_new$Satisfaction[Airdata$Satisfaction>3.5] <- "High"
Airdata_new$Satisfaction[Airdata$Satisfaction<3] <- "Low"

newSati <- na.omit(Sati_flight)
newSati$Satisfaction <- ceiling(newSati$Satisfaction)

for (i in c(3,5,6,7,9,10,11,18,19,20,21)){
  Airdata_new[,i] <- levelit(Airdata_new[,i])
}

for (i in 1:21){
  Airdata_new[,i] <- factor(Airdata_new[,i])
}

table(Airdata_new$Satisfaction)



# Start association rules modeling to see the character of customers with diverse satisfaction
install.packages("arules")
library(arules) 

newSurvey <- as(Airdata_new,"transactions")
inspect(head(newSurvey,10))
itemFrequency(newSurvey)
itemFrequencyPlot(newSurvey,paramter = list(confidence = 0.9))

result_Sati <- apriori(newSurvey)#,parameter = list(confidence=0.9)



result_Sati_3 <- apriori(newSurvey,parameter = list(confidence=0.6),appearance = list(default="lhs",rhs="Satisfaction=High"))
summary(result_Sati_3)
result_Sati.new_3 <- result_Sati_3[order(-quality(result_Sati_3)$lift),]
inspect(head(result_Sati.new_3,15))

result_Sati_2 <- apriori(newSurvey,appearance = list(rhs="Satisfaction=Average"))
summary(result_Sati_2)
result_Sati.new_2 <- result_Sati_2[order(-quality(result_Sati_2)$lift),]
inspect(result_Sati.new_2)

result_Sati_1 <- apriori(newSurvey,parameter = list(confidence=0.3),appearance = list(default="lhs",rhs="Satisfaction=Low"))
summary(result_Sati_1)
result_Sati.new_1 <- result_Sati_1[order(-quality(result_Sati_1)$lift),]
inspect(result_Sati.new_1)

# Calculate the frequency of each rules appearing 
# E.X. the customer with satisfaction score euqualing to 1
install.packages("wordcloud")
library(wordcloud)

countit <- function(result,num){
  v1 <- as(lhs(head(result,num)),"list")
  dfSat1 <- data.frame(table(unlist(v1)))
  vSat1 <- dfSat1[order(-dfSat1$Freq),]
  return(vSat1)
}


vSati1 <- as(rhs(result_Sati),"list")
dfSati1 <- data.frame(table(unlist(vSati1)))
person1 <- dfSati1[order(-dfSati1$Freq),]

sum(dfSati1$Freq)
dfSatis <-as(lhs(result))
View(dfSati1)
#                             Var1 Freq
#2             Airline.Status=Blue   18
#5                       Class=Eco   17
#9    No..of.other.Loyalty.Cards=3   13
#12 Type.of.Travel=Personal Travel   13
#10            Price.Sensitivity=4   12
#7                   Gender=Female   10
#11   Shopping.Amount.at.Airport=3    7
#6    Departure.Delay.in.Minutes=3    3
#1                           Age=5    2
#8                     Gender=Male    2
#3      Arrival.Delay.in.Minutes=3    1
#4      Arrival.Delay.in.Minutes=5    1



# Draw correlation matrix and visualize it
Airdata_new1 <- Airdata
Airdata_new1$Gender <- 0
Airdata_new1$Gender[Airdata$Gender == "Male"] <- 1

Airdata_new1$Airline.Status <- as.character(Airdata_new1$Airline.Status)
Airdata_new1$Airline.Status <- 0
Airdata_new1$Airline.Status[Airdata$Airline.Status == "Silver"] <- 1
Airdata_new1$Airline.Status[Airdata$Airline.Status == "Gold"] <- 2
Airdata_new1$Airline.Status[Airdata$Airline.Status == "Platinum"] <- 3

Airdata_new1$Class <- as.character(Airdata_new1$Class)
Airdata_new1$Class <- 0
Airdata_new1$Class[Airdata$Class == "Eco Plus"] <- 1
Airdata_new1$Class[Airdata$Class == "business"] <- 2
Airdata_new1 <- Airdata_new1[,c(-8,-13,-14,-15,-16,-17)]
View(Airdata_new1)
cor_Sati <- cor(Airdata_new1)
cor_Sati

#visualize it
install.packages("corrplot")
library(corrplot)
corrplot(cor_Sati, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.5)

# Seems not obivious at all
# Normalize it before cor
newSati_2 <- newSati[,c(-8,-13,-14,-15,-16,-17)]
newSati_2$Gender <- newSati_1$Gender
newSati_2$Airline.Status <- newSati_1$Airline.Status
newSati_2$Class <- newSati_1$Class
for( i in 1:15){
  newSati_2[,i] <- as.numeric(newSati_2[,i])
}

new.cor_Sati <- cor(newSati_2)
corrplot(new.cor_Sati, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 1)
hist(Airdata$Satisfaction)

install.packages("psych")
library(psych)
pairs.panels(Airdata_new1)

# Seems more clear, but it may due to my wrong sense
# Analyze the heatmap, and we can find it out that XXX has a positive effort on the satisfaction, while XXX have a negative effort on it
# Then use the plot to see the relationship between one or several attributes
install.packages("ggplot2")
library(ggplot2)

Sati_flight_1 <- na.omit(Sati_flight)
Sati_flight_1$Satisfaction <- factor(ceiling(Sati_flight_1$Satisfaction))

Sati_flight_1 <- Airdata
# Negative factors:
# Satisfaction and Age
Sati_Age <- ggplot(Sati_flight_1,aes(x=Age))
Sati_Age <- Sati_Age + geom_histogram(aes(fill=Satisfaction),position = "dodge")
Sati_Age <- Sati_Age + ggtitle("Satisfaction versus Age")
Sati_Age

Sati_Age_1 <- ggplot(Sati_flight_1,aes(x=Age)) + geom_density(aes(fill=Satisfaction),position = "fill")
Sati_Age_1

# Satisfaction and No of Flights p. a. 
Sati_NoFl <- ggplot(Sati_flight_1,aes(x=No.of.Flights.p.a.))
Sati_NoFl <- Sati_NoFl + geom_histogram(aes(fill=Satisfaction),position = "dodge")
Sati_NoFl <- Sati_NoFl + ggtitle("Satisfaction versus No of Flights")
Sati_NoFl

Sati_NoFl_1 <- ggplot(Sati_flight_1,aes(x=No.of.Flights.p.a.)) + geom_density(aes(fill=Satisfaction),position = "fill")
Sati_NoFl_1

# Satisfaction and Departure delay
Dpdy96 <- quantile(Sati_flight_1$Departure.Delay.in.Minutes,0.96)
Sati_Dpdy <- ggplot(Sati_flight_1[Sati_flight_1$Departure.Delay.in.Minutes<Dpdy96,],aes(x=Departure.Delay.in.Minutes))
Sati_Dpdy <- Sati_Dpdy + geom_histogram(aes(fill=Satisfaction),position = "dodge")
Sati_Dpdy <- Sati_Dpdy + ggtitle("Satisfaction versus Departure Delay")
Sati_Dpdy

Sati_Dpdy_1 <- ggplot(Sati_flight_1[Sati_flight_1$Departure.Delay.in.Minutes<Dpdy96,],aes(x=Departure.Delay.in.Minutes)) + geom_density(aes(fill=Satisfaction),position = "fill")
Sati_Dpdy_1

# Satisfaction and Arrival delay
Ardy96 <- quantile(Sati_flight_1$Arrival.Delay.in.Minutes,0.96)
Sati_Ardy <- ggplot(Sati_flight_1[Sati_flight_1$Arrival.Delay.in.Minutes<Ardy96,],aes(x=Arrival.Delay.in.Minutes))
Sati_Ardy <- Sati_Ardy + geom_histogram(aes(fill=Satisfaction),position = "dodge")
Sati_Ardy <- Sati_Ardy + ggtitle("Satisfaction versus Arrival Delay")
Sati_Ardy

Sati_Ardy_1 <- ggplot(Sati_flight_1[Sati_flight_1$Arrival.Delay.in.Minutes<Ardy96,],aes(x=Arrival.Delay.in.Minutes)) + geom_density(aes(fill=Satisfaction),position = "fill")
Sati_Ardy_1

# Satisfaction and Price Sensitivity
Sati_Prsy <- ggplot(Sati_flight_1,aes(x=Price.Sensitivity))
Sati_Prsy <- Sati_Prsy + geom_histogram(aes(fill=Satisfaction),position = "dodge")
Sati_Prsy <- Sati_Prsy+ ggtitle("Satisfaction versus Price Sensitivity")
Sati_Prsy

Sati_Prsy_1 <- ggplot(Sati_flight_1,aes(x=Price.Sensitivity)) + geom_density(aes(fill=Satisfaction),position = "fill")
Sati_Prsy_1

# Positive factors:
# Satisfaction and Arrival delay
Sati_As <- ggplot(Airdata_new1,aes(x=Airline.Status))
Sati_As <- Sati_As + geom_histogram(aes(fill=factor(ceiling(Satisfaction))),position = "dodge")
Sati_As <- Sati_As + ggtitle("Satisfaction versus Airline Status")
Sati_As

Sati_As_1 <- ggplot(Airdata_new1,aes(x=Airline.Status)) + geom_density(aes(fill=factor(ceiling(Satisfaction))),position = "fill")
Sati_As_1

# Satisfaction and No. Of other Loyalty Cards
maxlc <- quantile(Sati_flight_1$No..of.other.Loyalty.Cards,probs = 0.95)
Sati_Lc <- ggplot(Sati_flight_1[Sati_flight_1$No..of.other.Loyalty.Cards<maxlc,],aes(x=No..of.other.Loyalty.Cards))
Sati_Lc <- Sati_Lc + geom_histogram(aes(fill=Satisfaction),position = "dodge")
Sati_Lc <- Sati_Lc + ggtitle("Satisfaction versus Loyalty Cards")
Sati_Lc

Sati_Lc_1 <- ggplot(Sati_flight_1[Sati_flight_1$No..of.other.Loyalty.Cards<maxlc,],aes(x=No..of.other.Loyalty.Cards)) + geom_density(aes(fill=Satisfaction),position = "fill")
Sati_Lc_1
  
# Satisfaction and Percent of Flight with other Airlines 
Fa96 <- quantile(Sati_flight_1$X..of.Flight.with.other.Airlines,0.96)
Sati_Fa <- ggplot(Airdata_new1[Airdata_new1$X..of.Flight.with.other.Airlines<Fa96,],aes(x=X..of.Flight.with.other.Airlines))
Sati_Fa <- Sati_Fa + geom_histogram(aes(fill=factor(ceiling(Satisfaction))),position = "dodge")
Sati_Fa <- Sati_Fa + ggtitle("Satisfaction versus Flight with other airlines")
Sati_Fa

Sati_Fa_1 <- ggplot(Airdata_new1[Airdata_new1$X..of.Flight.with.other.Airlines<Fa96,],aes(x=X..of.Flight.with.other.Airlines)) + geom_density(aes(fill=factor(ceiling(Satisfaction))),position = "fill")
Sati_Fa_1

# Using groupby to 
#Normalize the Airdata
Airdata_zscore <- Airdata
for(i in c(1,3,5,6,7,9,10,11,18,19,20,21)){
Airdata_zscore[,i] <- scale(Airdata_zscore[,i],center = T,scale=T)
}

#Using the SVM model to predict the data
install.packages("kernlab")
library(kernlab)

trainindex <- sample(c(1,2,3), nrow(Airdata),replace= T,prob = c(0.15,0.45,0.4))
traindata <- Airdata[trainindex==1,]
testdata <- Airdata[trainindex==2,]

svmOutput <- ksvm(Satisfaction ~ Airline.Status + Age + Gender + Price.Sensitivity + 
                    No.of.Flights.p.a. + Type.of.Travel + No..of.other.Loyalty.Cards + 
                    Shopping.Amount.at.Airport + Class + Airline.Code + 
                    Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes, data=traindata,kernel="rbfdot", kpar="automatic",C=40,cross=4, prob.model=TRUE)
svmOutput

svmresult <- predict(svmOutput,testdata,type="votes")
View(svmresult)
compactable <- (testdata[,1]-svmresult)<0.8&(testdata[,1]-svmresult)>-0.7
result <- table(compactable)
result
erroratio <- result[2]/(sum(result))
erroratio
