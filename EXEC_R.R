# Espécies coletadas sp1:sp60
Especies <- c("sp1", "sp2", "sp3", "sp4", "sp5","sp6", "sp7", "sp8", "sp9",
              "sp10", "sp11", "sp12", "sp13", "sp14", "sp15", "sp16", "sp17", 
              "sp18", "sp19", "sp20", "sp21", "sp22", "sp23", "sp24", "sp25",
              "sp26", "sp27", "sp28", "sp29", "sp30", "sp31", "sp32", "sp33", 
              "sp34", "sp35", "sp36", "sp37", "sp38", "sp39", "sp40", "sp41", 
              "sp42", "sp43", "sp44", "sp45", "sp46", "sp47", "sp48", "sp49", 
              "sp50", "sp51", "sp52", "sp53", "sp54", "sp55", "sp56", "sp57", 
              "sp58", "sp59", "sp60")
Esporos <- sample(1:6, 60, replace=TRUE)
Areas <- sample(1:3, 60, replace = TRUE)
Altitude <- sample(1100:1600, 60, replace = TRUE)
Temperatura <- sample(20:28, 60, replace=TRUE)

# data-frame
coleta <- data.frame(Especies, Areas, Esporos, Altitude, Temperatura)


plot(coleta$Esporos,
     ylim = c(0,8),
     xlim = c(0,60),
     main = "Esporos",
     ylab = "Tamano dos esporos (mm)",
     xlab = "Quantidade das amostras",
     pch = 22)
  

# Correlação
cor(coleta$Esporos, coleta$Temperatura)
cor(coleta$Altitude, coleta$Temperatura)

# Variância
var(coleta$Esporos)
var(coleta$Temperatura)


# Norm
n_Esporo <- rnorm(coleta$Esporos)
n_Temperatura <- rnorm(coleta$Esporos)
n_Altitude <- rnorm(coleta$Altitude)

# Teste shapiro valor p < 0.05
shapiro.test(n_Esporo) # Hipotese nula recusada
shapiro.test(n_Temperatura) # Hipotese nula recusada
shapiro.test(n_Altitude) # Hipotese nula recusada

# ANOVA
trat<-factor(coleta$Esporos)
oneway<-aov(coleta$Esporos~trat)
oneway
anova(oneway)
shapiro.test(resid(oneway))
