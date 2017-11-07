
#Set the working directory
setwd("D:/STUDIES/Big Data/R/Projects_for_R/Healthcare")

#Read the HospitalCosts Dataset
hospital.data <- read.csv("HospitalCosts.csv")
head(hospital.data)

##Find the age category of people who frequent the hospital and has the maximum expenditure
age.factor<- as.factor(hospital.data$AGE)
levels(age.factor)

hist(hospital.data$AGE)
summary(age.factor)

#One way of finding the expenditure
maxexpenditure <- tapply(hospital.data$TOTCHG, age.factor, sum)
max(maxexpenditure)

#Another way of finding the expenditure
max.exp <- aggregate(hospital.data$TOTCHG~age.factor ,FUN = sum , data = hospital.data)
max(aggregate(hospital.data$TOTCHG~hospital.data$AGE ,FUN = sum, data = hospital.data))
max.exp[which.max(max.exp$`hospital.data$TOTCHG`),]

##Find the diagnosis related group that has maximum hospitalization and expenditure
class(hospital.data$APRDRG)
APRDRG.factor <- as.factor(hospital.data$APRDRG)

which.max(summary(APRDRG.factor))

hist(hospital.data$APRDRG)
#One way of finding expenditure
maxdiagexp <- tapply(hospital.data$TOTCHG,as.factor(hospital.data$APRDRG),sum)
max(maxdiagexp)
#Another way of finding expenditure
max.diag.exp <- aggregate(hospital.data$TOTCHG~hospital.data$APRDRG, FUN = sum , data= hospital.data)
max.diag.exp[which.max(max.diag.exp$`hospital.data$TOTCHG`),]
max.diag.exp

##Analyse if the race of the patient is related to hospitalization costs
hospital.data <- na.omit(hospital.data)
race.factor <- as.factor(hospital.data$RACE)
levels(race.factor)
summary(race.factor)

race.model <- aov(hospital.data$TOTCHG~hospital.data$RACE, data = hospital.data)
race.model
summary(race.model)


##To analyze the severity of the hospital costs by age and gender for proper allocation of resources
model <- lm(TOTCHG~AGE+FEMALE, data = hospital.data)
summary(model)
summary(as.factor(hospital.data$FEMALE))


##To find if the length of the stay can be predicted from age, gender and race
female.factor <- as.factor(hospital.data$FEMALE)
race.factor <- as.factor(hospital.data$RACE)
los.model <- lm(LOS~AGE+female.factor+race.factor, data = hospital.data)
los.model
summary(los.model)

##To find the variable that mainly affects the hospital costs
#model1 <- lm(TOTCHG~AGE, data = hospital.data)
#summary(model1)
#model2 <- lm(TOTCHG~as.factor(FEMALE),data = hospital.data)
#summary(model2)
#model3 <- lm(TOTCHG~as.factor(RACE),data = hospital.data)
#summary(model3)
#model4 <- lm(TOTCHG~LOS, data = hospital.data)
#summary(model4)
model5 <- lm(TOTCHG~., data = hospital.data)
summary(model5)
