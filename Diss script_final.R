
###read datasets and packages

library(rio)
ethgr <- import("ethgr.csv")
sccs <- import("SCCS_data.xlsx")

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(coefplot)

###multinomial logistic regression on EA dataset using ordinal rankings of reliance on foraging (low, mid, high, numbered 1,2,3)

##run multinom test

#set descent variable to factor for analysis  
ethgr$EA043=as.factor(ethgr$EA043)
ethgr$EA_ord_val=as.factor(ethgr$EA_ord_val)

#establish base line case for comparison
ethgr$EA043 <- relevel(ethgr$EA043, ref = "Bilateral")

#run multinom test
test <- multinom(EA043 ~ EA_ord_val, data = ethgr)
test
summary(test)

##get p-values

#2 tailed z test
z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(test))

head(pp <- fitted(test))


##create data subset with predicted probabilities 

d_ord_val<- data.frame(EA_ord_val = c("1", "2", "3"))
predict(test, newdata = d_ord_val, "probs")

df_ordval <- data.frame(EA_ord_val = rep(c("1", "2", "3"), each = 3))

pp.ordval <- cbind(df_ordval, predict(test, newdata = df_ordval, type = "probs", se = TRUE))

lpp <- melt(pp.ordval, id.vars = c("EA_ord_val"), value.name = "probability")
head(lpp)

ggplot(lpp, aes(x = as.numeric(EA_ord_val), y = probability, colour=variable))+geom_smooth(method = "lm")+geom_line()

par(mfrow=c(1,1))

###multinom log regression on EA data using numeric ratings of reliance on foraging (5-100%)

##NOTE: despite having a numeric class, variable 'EAHG' must to reconverted to numeric or converted to factor to get probabilities 

test1 <- multinom(EA043 ~ EAHG, data = ethgr)
test1
summary(test1)

z1<- summary(test1)$coefficients/summary(test1)$standard.errors
z1


p1 <- (1 - pnorm(abs(z1), 0, 1)) * 2
p1


d_HG<- data.frame(EAHG = as.numeric(c("5", "13", "21","23","31","33","41","43","51","53","61","63","71","81","91","100")))
predict(test1, newdata = d_HG, "probs")

df_HG <- data.frame(EAHG=as.numeric(rep(c("5", "13", "21","23","31","33","41","43","51","53","61","63","71","81","91","100"), each = 16)))
pp.HG <- cbind(df_HG, predict(test1, newdata = df_HG, type = "probs", se = TRUE))

lpp1 <- melt(pp.HG, id.vars = c("EAHG"), value.name = "probability")
head(lpp)

ggplot(lpp1, aes(x = as.numeric(EAHG), y = probability, colour=variable))+geom_smooth()+geom_line()


###multinomial test for sccs data 

test2 <- multinom(SCCSrd ~ SCCShg, data = sccs)
test2
summary(test2)

z2<- summary(test2)$coefficients/summary(test2)$standard.errors
z2

p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2

d_hg.sccs<- data.frame(SCCShg = as.numeric(c("5", "13", "21","23","31","33","41","43","51","61","71","81","91","100")))
predict(test2, newdata = d_hg.sccs, "probs")

df_hg.sccs <- data.frame(SCCShg=as.numeric(rep(c("5", "13", "21","23","31","33","41","43","51","53","61","63","71","81","91","100"), each = 14)))
pp.hg.sccs <- cbind(df_hg.sccs, predict(test2, newdata = df_hg.sccs, type = "probs", se = TRUE))

lpp2 <- melt(pp.hg.sccs, id.vars = c("SCCShg"), value.name = "probability")
head(lpp2)

ggplot(lpp2, aes(x = as.numeric(SCCShg), y = probability, colour=variable))+geom_smooth()+geom_line()

#compare data in ggplot

vis.EA <- ggplot(lpp1, aes(x = as.numeric(EAHG), y = probability, colour=variable))+geom_smooth()+geom_line()
vis.sccs <- ggplot(lpp2, aes(x = as.numeric(SCCShg), y = probability, colour=variable))+geom_smooth()+geom_line()
vis.EAlab <- vis.EA+xlab("Reliance on hunting and gathering (%)")+ggtitle("All societies in the Ethnographic Atlas")+theme_minimal()
vis.sccslab <- vis.sccs+xlab("Reliance on hunting and gathering (%)")+ggtitle("SCCS representative sample")+theme_minimal()
library(ggpubr)
ggarrange(vis.EAlab,vis.sccslab)





