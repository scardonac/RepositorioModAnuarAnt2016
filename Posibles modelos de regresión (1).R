## HORIZONTES
require(MASS)
HORIZONTE1 <- as.formula("~(HURTO + TOT_GRAD + PIB + AFI_REG_SUB + TOT_EMP)^2")
HORIZONTE2 <- as.formula("~HURTO + TOT_GRAD + PIB + AFI_REG_SUB + TOT_EMP ")

HORIZONTE3 <- as.formula("~HURTO + TOT_GRAD + PIB + AFI_REG_SUB + TOT_EMP + 
                    I(TOT_GRAD^3)+I(TOT_EMP^3)+I(HURTO^3)+
                    I(PIB^3)+I(AFI_REG_SUB^3)+I(TOT_GRAD^2)+I(TOT_EMP^2)+
                    I(HURTO^2)+I(PIB^2)+I(AFI_REG_SUB^2)")

HORIZONTE4 <- as.formula("~(HURTO + TOT_GRAD + PIB + AFI_REG_SUB + 
                    TOT_EMP)^2+I(TOT_GRAD^3)+I(TOT_EMP^3)+I(HURTO^3)+I(PIB^3)+
I(AFI_REG_SUB^3)+I(TOT_GRAD^2)+I(TOT_EMP^2)+I(HURTO^2)+I(PIB^2)+
I(AFI_REG_SUB^2)")

attach(BASE) 
## Fijamos la distribucion LOGNO y seleccionamos variables

con1 <- gamlss.control(c.crit=0.01, n.cyc=10000)
mod1<-gamlss(DEF~1, data=BASE, family=LOGNO, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
mod2 <- refit(mod2)
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
mod3 <- refit(mod3)
summary(mod3)


GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
mod4 <- refit(mod4)
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
mod5 <- refit(mod5)
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))


## Fijamos la distribucion IG y seleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=IG, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
mod2 <- refit(mod2)
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))


## Fijamos la distribucion GG y seleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=GG, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
mod2 <- refit(mod2)
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
mod5 <- refit(mod5)
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))


## Fijamos la distribucion BCCGo y seleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=BCCGo, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))


## Fijamos la distribucion GIG y seleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=GIG, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribucion BCPEo y seleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=BCPEo, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribucion BCTo y seleccionamos variables
mod1<-gamlss(DEF~1, data=BASE, family=BCTo, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribución IGAMMA y sleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=IGAMMA, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribución GA y sleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=GA, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribución WEI3 y sleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=WEI3, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribución EXP y seleccionamos variables
mod1<-gamlss(DEF~1, data=BASE, family=EXP, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribución PARETO2 y sleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=PARETO2, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)

GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))

## Fijamos la distribución exGAUS y seleccionamos variables

mod1<-gamlss(DEF~1, data=BASE, family=exGAUS, trace=FALSE) 

mod2<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE1))
summary(mod2)


GAIC(mod2)
BIC(mod2)
cor(DEF, fitted.values(mod2))

mod3 <- stepGAICAll.A(mod1, scope = list(lower=~1, upper=HORIZONTE2))
summary(mod3)

GAIC(mod3)
BIC(mod3)
cor(DEF, fitted.values(mod3))


mod4<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE3))
summary(mod4)

GAIC(mod4)
BIC(mod4)
cor(DEF, fitted.values(mod4))


mod5<-stepGAICAll.A(mod1, scope=list(lower=~1, upper=HORIZONTE4))
summary(mod5)

GAIC(mod5)
BIC(mod5)
cor(DEF, fitted.values(mod5))
