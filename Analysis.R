install.packages("RJSONIO")
library(RJSONIO)
load("R-Project.RData")
hist(Sati_flight$Satisfaction,breaks = 10)
nrow(Sati_flight[Sati_flight$Satisfaction<1.5,])
Age_Sat <- lm(formula = Price.Sensitivity ~ .,data = Sati_flight)
summary(Age_Sat)


#Using Association rule modeling to refine to the existing model
#convert useful column into factors
levelit <- function(v){
  new.v <- v
  level4 <- quantile(v,probs = c(0.2,0.4,0.6,0.8))
  new.v[v<level4[1]] <- 1
  new.v[level4[1]<=v & v<level4[2]] <- 2
  new.v[level4[2]<=v & v<level4[3]] <- 3
  new.v[level4[3]<=v & v<level4[4]] <- 4
  new.v[v>=level4[4]] <- 5
  new.v <- factor(new.v)
  return(new.v)
}
newSati <- na.omit(Sati_flight)
newSati$Satisfaction <- ceiling(newSati$Satisfaction)

for (i in c(3,5,6,7,9,10,11,18,19,20,21)){
  newSati[,i] <- levelit(newSati[,i])
}
for (i in 1:21){
  newSati[,i] <- factor(newSati[,i])
}

hist(as.numeric(newSati$Satisfaction))
# Start association rules modeling to see the character of customers with diverse satisfaction
install.packages("arules")
library(arules) 

newSurvey <- as(newSati,"transactions")
inspect(head(newSurvey,10))
itemFrequency(newSurvey)
itemFrequencyPlot(newSurvey,paramter = list(confidence = 0.9))

result_Sati_5 <- apriori(newSurvey,appearance = list(default="lhs",rhs="Satisfaction=5"))
summary(result_Sati_5)
result_Sati.new_5 <- result_Sati_5[order(-quality(result_Sati_5)$lift),]
inspect(head(result_Sati.new_5,10))

result_Sati_4 <- apriori(newSurvey,parameter = list(confidence=0.3),appearance = list(default="lhs",rhs="Satisfaction=4"))
summary(result_Sati_4)
result_Sati.new_4 <- result_Sati_4[order(-quality(result_Sati_4)$lift),]
inspect(head(result_Sati.new_4,10))

result_Sati_3 <- apriori(newSurvey,parameter = list(confidence=0.3),appearance = list(default="lhs",rhs="Satisfaction=3"))
summary(result_Sati_3)
result_Sati.new_3 <- result_Sati_3[order(-quality(result_Sati_3)$lift),]
inspect(head(result_Sati.new_3,10))

result_Sati_2 <- apriori(newSurvey,parameter = list(confidence=0.1),appearance = list(default="lhs",rhs="Satisfaction=2"))
summary(result_Sati_2)
result_Sati.new_2 <- result_Sati_2[order(-quality(result_Sati_2)$lift),]
inspect(head(result_Sati.new_2,10))

result_Sati_1 <- apriori(newSurvey,appearance = list(default="lhs",rhs="Satisfaction=1"))
summary(result_Sati_1)
result_Sati.new_1 <- result_Sati_1[order(-quality(result_Sati_1)$lift),]
inspect(head(result_Sati.new_1,10))

# The 1St and 5th cannot work
# Try to seperate it before modeling
newSati.Sat5 <- newSati[newSati$Satisfaction ==5,]
newSurvey.Sat5 <- as(newSati.Sat5,"transactions")

new.result_5 <- apriori(newSurvey.Sat5,appearance = list(default="lhs",rhs="Satisfaction=5"))
summary(new.result_5)
result_Sati5 <- new.result_5[order(-quality(new.result_5)$support),]
inspect(head(result_Sati5,15))


newSati.Sat1 <- newSati[newSati$Satisfaction ==1,]
newSurvey.Sat1 <- as(newSati.Sat1,"transactions")

new.result_1 <- apriori(newSurvey.Sat1,appearance = list(default="lhs",rhs="Satisfaction=1"))
summary(new.result_1)
result_Sati1 <- new.result_1[order(-quality(new.result_1)$support),]
inspect(head(result_Sati1,15))

# After seperate it, we mainly focus on the value of support


# Draw correlation matrix and visualize it
newSati_1 <- na.omit(Sati_flight)
newSati_1$Gender <- 0
newSati_1$Gender[newSati$Gender == "Male"] <- 1

newSati_1$Airline.Status <- as.character(newSati_1$Airline.Status)
newSati_1$Airline.Status <- 0
newSati_1$Airline.Status[newSati$Airline.Status == "Silver"] <- 1
newSati_1$Airline.Status[newSati$Airline.Status == "Gold"] <- 2
newSati_1$Airline.Status[newSati$Airline.Status == "Platinum"] <- 3

newSati_1$Class <- as.character(newSati_1$Class)
newSati_1$Class <- 0
newSati_1$Class[newSati$Class == "Eco Plus"] <- 1
newSati_1$Class[newSati$Class == "business"] <- 2
newSati_1 <- newSati_1[,c(-8,-13,-14,-15,-16,-17)]

cor_Sati <- cor(newSati_1)
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

# Seems more clear, but it may due to my wrong sense
# Analyze the heatmap, and we can find it out that XXX has a positive effort on the satisfaction, while XXX have a negative effort on it
# Then use the plot to see the relationship between one or several attributes
