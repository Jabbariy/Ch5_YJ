
library(aod)
library (readxl)
library(nlme)
library(car)
library(multcomp)
library(jmv)
library(emmeans)

setwd("")
data<- read_excel("Ch5_YJ.xlsx")



#Dependent Variables 

data$Ego  <- as.numeric(data$Ego)
data$Allo  <- as.numeric(data$Allo)
data$SBSOD <-as.numeric(data$SBSOD)
data$sID<-as.integer(data$sID)
data$agegroup <- factor(data$agegroup, level=c("3", "2", "1"))
data$computerHours<- as.integer(data$computerHours)
data$sex<-factor(data$sex, level=c("1", "0"))
data$map <-factor(data$pathN , level=c("1", "0"))
data$videoGameHours<- as.integer(data$videoGameHours)
data$drivingHours<- as.integer(data$drivingHours)
data$drivingGameHours<- as.integer(data$drivingGameHours)
data$Mean<- as.numeric(data$Mean)


##########regression##########

fit0 <- nlme::lme(Allo ~  videoGameHours + drivingHours  + sex + computerHours + map+ block, random=~1+block|sID, data=data, control=lmeControl(opt='optim'))
summary(fit0)


fit00 <- nlme::lme(Ego ~   videoGameHours + drivingHours  + sex + computerHours + map+ block, random=~1+block|sID, data=data, control=lmeControl(opt='optim'))
summary(fit00)


fit1 <- nlme::lme(Allo ~ agegroup + map + sex + drivingHours + computerHours , random=~1+block|sID, data=data, control=lmeControl(opt='optim'))
summary(fit1)

pwc<-lsmeans :: lsmeans(fit1, pairwise~agegroup,adjust="tukey")
summary(pwc)
confint(pwc)
confint(fit1) #CIs of the log odds
lsmeans :: lsmeans(fit1, pairwise~agegroup,adjust="tukey")
confint(lsmeans :: lsmeans(fit1, pairwise~agegroup,adjust="tukey"))
intervals(fit1, which = "fixed")

fit2<- lme(Ego ~ agegroup + map + sex + block  + drivingHours, random=~1+block|sID, data=data, control=lmeControl(opt='optim'))
summary(fit2)

pwc<-lsmeans :: lsmeans(fit2, pairwise~agegroup,adjust="tukey")
summary(pwc)
confint(pwc)
confint(fit2) #CIs of the log odds
lsmeans :: lsmeans(fit2, pairwise~agegroup,adjust="tukey")
confint(lsmeans :: lsmeans(fit2, pairwise~agegroup,adjust="tukey"))
intervals(fit2, which = "fixed")







