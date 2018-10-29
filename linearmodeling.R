Sati <- data.frame(Satisfaction_Survey,stringsAsFactors = FALSE)
Sati_flight <- Sati[which(Sati$Flight.cancelled=="No"),]
Sati_flight <- Sati_flight[,-c(6,14,19,21,25,27,28)]
View(Sati_flight)
model2 <- lm(formula = Satisfaction ~ Scheduled.Departure.Hour + Flight.time.in.minutes + Arrival.Delay.in.Minutes + Departure.Delay.in.Minutes, data = Sati_flight)
str(Sati)
summary(model2)

Sati_flight$Airline.Status <- factor(Sati_flight$Airline.Status,ordered = T, levels = c("Blue","Silver","Gold","Platinum"))
Sati_flight$Class <- factor(Sati_flight$Class,ordered = T, levels = c("Eco","Eco Plus","Business"))

model3 <- lm(formula = Airline.Status ~ Age+Gender+Destination.City+Orgin.City+Airline.Code+Flight.date+Type.of.Travel, data = Sati_flight)
summary(model3)