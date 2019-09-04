ozone1=read.csv2("ozone.txt",sep= " ",dec=".")
ozone2=ozone1[2:13]
#1)

pairs(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15,data=ozone2)
pairs(maxO3~.,data = ozone2)
cor.test(ozone2$maxO3,ozone2$T9)
cor.test(ozone2$maxO3,ozone2$T12)
cor.test(ozone2$maxO3,ozone2$T15)
cor.test(ozone2$maxO3,ozone2$Ne9)
cor.test(ozone2$maxO3,ozone2$Ne12)
cor.test(ozone2$maxO3,ozone2$Ne15)
cor.test(ozone2$maxO3,ozone2$Vx9)
cor.test(ozone2$maxO3,ozone2$Vx12)
cor.test(ozone2$maxO3,ozone2$Vx15)
#2)
regs=lm(maxO3~Ne12,data = ozone2)
summary(regs)
reg=lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15,data=ozone2)
summary(reg)
#3)

library("car")

vif(reg)
#4)
reg1=lm(maxO3~T9+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15,data=ozone2)
summary(reg1)
vif(reg1)
#5)

step(reg1,trace = TRUE,direction = "backward",k=log(112))

#lm(maxO3 ~ T9+ T15 + Ne12 + Vx9, data = ozone2) est le meilleur modele retenu
reg2=lm(maxO3 ~ T9+ T15 + Ne12 + Vx9, data = ozone2)
summary(reg2)

#lorsqu'on a refait la modelisation de reg2 ,on constate que tous le variable sont
#siginificatif et R2 est plus grand  ,donc c'est bien le meilleur modele


#6)
reg0=lm(maxO3~1,data=ozone2)
step(reg0,scope = ~.+T9+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15,direction = "forward",trace = TRUE,k=log(122))

# on a obtenu aussi lm(maxO3 ~ T9+ T15 + Ne12 + Vx9, data = ozone2) comme meilleur modele retenu

#7)
plot(reg2$fitted,rstudent(reg2),xlab="Valeurs pr?dites",ylab="R?sidus studentis?s")
abline(h=c(2,-2),col="red")#V?rifier les points aberrants
par(mfrow=c(1,2))
hist(rstudent(reg2),prob=TRUE,main="Distribution des residus",xlab="residuals")
lines(density(rstudent(reg2)),col="red")
qqnorm(rstudent(reg2),datax=TRUE,main="Q-Q plot des residus")  
qqline(rstudent(reg2),datax=TRUE,col="red")
shapiro.test(rstudent(reg2))#ve rifier la normalit?
plot(reg2$fitted.values,rstudent(reg2),main = "Homosc?dasticit?",xlab="Valeurs pr?dites",ylab = "Valeurs r?siduelles") 
abline(h=0,col="red")#v?rifier l'homosc?dasticit?
install.packages("lmtest")
library("lmtest")
dwtest(reg2)#v?rifier l'ind?pendance
 