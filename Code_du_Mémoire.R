##################################################

set.seed(123)
library(xts)
library(zoo)
library(sp)
library(CASdatasets)
library(MASS)
library(lattice)
library(ggplot2)
library(caret)
library(FactoMineR)
library(factoextra)
library(gplots)
library(openxlsx)
data(freMPL3)
data(freMPL4)

freMPL34 <- rbind(freMPL3, freMPL4)

#-------------------------------------------------
#Statistiques descriptives Variables quantitatives
#-------------------------------------------------

# Certaines analyses statistiques de type summary ou autres
# ne sont pas toujours indiquees dans ce programme car ce 
# sont des elements triviaux

Variables_quantitatives_freMPL3 <- freMPL3[,+c(1,2,10,11,13,20,21)]


#Representation des correlations : plus l'ellipse ressemble a un cercle et moins
#les variables sont correlees. Plus l'ellipse ressemble a une droite et plus les 
#variables sont correlees.

corrplot(cor(Variables_quantitatives_freMPL3),method = "ellipse")

plot(Variables_quantitatives_freMPL3)

# => Une forte correclation entre DriveAge et LicAge.

Variables_quantitatives_freMPL4 <- freMPL4[,+c(1,2,10,11,13,20,21)]


#Representation des correlations : plus l'ellipse ressemble a un cercle et moins
#les variables sont correlees. Plus l'ellipse ressemble a une droite et plus les 
#variables sont correlees.

corrplot(cor(Variables_quantitatives_freMPL4),method = "ellipse")

plot(Variables_quantitatives_freMPL4)

# => Une forte correclation entre DriveAge et LicAge.


#-------------------------------------------------
#                          ACP
#-------------------------------------------------

TabACP <- freMPL4[,+c(1,2,10,13,20,21)]

summary(TabACP)

res.pca <- PCA(TabACP, scale.unit = TRUE, ncp=2, graph = F)

res.pca

round(res.pca$eig,2)

res.pca <- PCA(TabACP, scale.unit = TRUE, ncp=2, graph = T)

#-------------------------------------------------
#                          AFC
#-------------------------------------------------

tableau <- housetasks[c(1,2,3,4,5,6),c(1,2)]
colnames(tableau) = c("ClaimInd=0","ClaimInd=1")
rownames(tableau) <- c("Male","Female","Alone","Other","HasKmLimit=0",
                       "HasKmLimit=1")

tableau[1,1] <- 18263
tableau[2,1] <- 11085
tableau[3,1] <- 7113
tableau[4,1] <- 22235
tableau[5,1] <- 26111
tableau[6,1] <- 3237

tableau[1,2] <- 762
tableau[2,2] <- 483
tableau[3,2] <- 311
tableau[4,2] <- 936
tableau[5,2] <- 1138
tableau[6,2] <- 109


View(tableau)

chisq <- chisq.test (tableau)
chisq

# 1. convertir les donnees en tant que table
dt <- as.table(as.matrix (tableau))
# 2. Graphique
balloonplot(t (dt), main = "tableau", xlab = "", ylab = "", label = FALSE,
            show.margins = FALSE)


res.ca <- CA (tableau, graph = FALSE)

res.ca <- CA (tableau, graph = FALSE)

eig.val <- get_eigenvalue (res.ca)

########################   GLM ClaimInd    #################


levels(freMPL34$SocioCateg) <- factor(c("CSP1","CSP1","CSP1","CSP1","CSP2",
                                        "CSP2","CSP2","CSP2","CSP2","CSP2",
                                        "CSP3","CSP3","CSP3","CSP4","CSP4",
                                        "CSP4","CSP4","CSP4","CSP4","CSP4",
                                        "CSP5","CSP5","CSP5","CSP5","CSP5",
                                        "CSP5","CSP6","CSP6","CSP6","CSP6",
                                        "CSP6","CSP6","CSP7","CSP7","CSP7",
                                        "CSP7","CSP7","CSP9"))
levels(freMPL34$VehPrice) <- factor(c("A","A","A","D","E","F","G","H","I","J",
                                      "K","L","M","N","O","P","Q","Q","Q","Q",
                                      "Q","Q","Q","Q","Q","Q","Q"))
levels(freMPL34$VehMaxSpeed) <- factor(c("1-140 km/h","1-140 km/h",
                                         "140-150 km/h","150-160 km/h",
                                         "160-170 km/h","170-180 km/h",
                                         "180-190 km/h","190-200 km/h",
                                         "200-220 km/h","220+ km/h "))

freMPL34 <- freMPL34[(freMPL34$VehEnergy != "eletric") &
                       (freMPL34$VehEnergy != "GPL"),]
freMPL34$VehEnergy <- droplevels(freMPL34$VehEnergy)
freMPL34 <- freMPL34[(freMPL34$VehEnergy != "eletric") &
                       (freMPL34$VehEnergy != "GPL"),]
freMPL34$VehEnergy <- droplevels(freMPL34$VehEnergy)
freMPL34$VehEngine <- droplevels(freMPL34$VehEngine)
freMPL34$LicAge <- floor(freMPL34$LicAge/12)
freMPL34$LicAge3 <- (freMPL34$LicAge-23)^2

n <- NROW(freMPL34) ; p <- round(0.8*n)
index.app <- sample(1:n, p)
freMPL34.app <- freMPL34[index.app, ]
freMPL34.test <- freMPL34[-index.app, ]


freMPL34.app.reg <- model.matrix(ClaimInd ~ LicAge+LicAge3 + VehAge + 
                                   Gender + MariStat + SocioCateg + VehUsage + 
                                   DrivAge + HasKmLimit + DeducType + 
                                   BonusMalus + VehBody + VehPrice + VehEngine +
                                   VehEnergy + VehMaxSpeed + VehClass + 
                                   RiskVar + Garage, data=freMPL34.app)

freMPL34.test.reg <- model.matrix(ClaimInd ~ LicAge+LicAge3 + VehAge + 
                                   Gender + MariStat + SocioCateg + VehUsage +
                                   DrivAge + HasKmLimit + DeducType + 
                                   BonusMalus + VehBody + VehPrice + 
                                   VehEngine + VehEnergy + VehMaxSpeed + 
                                   VehClass + RiskVar + Garage,
                                   data=freMPL34.test)

colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg)
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"


colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"


# GLM ClaimInd avec le modele Binomiale

GLM.claimInd <- glm(ClaimInd ~ LicAge3 + VehAge1 + VehAge10 + VehAge2 + VehAge3 +
                      VehAge4 + VehAge5 + VehAge6 + VehAge8 + GenderMale + 
                      MariStatOther + SocioCategCSP2 + SocioCategCSP3 + 
                      SocioCategCSP4 + SocioCategCSP5 + SocioCategCSP6 + 
                      SocioCategCSP7 + SocioCategCSP9 + 
                      VehUsagePrivate_trip_to_office + VehUsageProfessional + 
                      VehUsageProfessional_run + DrivAge + HasKmLimit + 
                      DeducTypeNormal + DeducTypePartially_refunded + 
                      DeducTypeProportional + DeducTypeRefunded + BonusMalus +
                      VehBodycabriolet + VehBodycoupe + VehBodymicrovan + 
                      VehBodyother_microvan + VehBodysedan + 
                      VehBodysport_utility_vehicle + VehBodystation_wagon +
                      VehBodyvan + VehPriceD + VehPriceE + VehPriceF + VehPriceG +
                      VehPriceH + VehPriceI + VehPriceJ + VehPriceK + VehPriceL + 
                      VehPriceM + VehPriceN + VehPriceO + VehPriceP + VehPriceQ +
                      VehEnginedirect_injection_overpowered + VehEngineinjection +
                      VehEngineinjection_overpowered + VehEnergyregular + 
                      VehMaxSpeed140_150_km_h + VehMaxSpeed150_160_km_h + 
                      VehMaxSpeed160_170_km_h + VehMaxSpeed170_180_km_h + 
                      VehMaxSpeed180_190_km_h + VehMaxSpeed190_200_km_h + 
                      VehMaxSpeed200_220_km_h + VehMaxSpeed220_km_h  + VehClassA +
                      VehClassB + VehClassH + VehClassM1 + VehClassM2 + RiskVar +
                      GarageNone + GaragePrivate_garage,
                    family=binomial(link = 'logit'),
                    data=cbind.data.frame(ClaimInd=freMPL34.app$ClaimInd, 
                    freMPL34.app.reg))

#Selection par AIC (Forward, Backward, Stepwise)

GLM.claimInd <- stepAIC(GLM.claimInd, trace=TRUE, direction=c("both"))
AIC(GLM.claimInd)

GLM.claimInd <- stepAIC(GLM.claimInd, trace=TRUE, direction=c("backward"))
AIC(GLM.claimInd)

GLM.claimInd <- stepAIC(GLM.claimInd, trace=TRUE, direction=c("forward"))
AIC(GLM.claimInd)

#le GLM ClaimInd selectionne 


GLM.ClaimInd <- glm(ClaimInd ~ LicAge3 + BonusMalus + VehAge10 + VehAge6 + 
                      VehAge8 + SocioCategCSP6 + VehUsageProfessional + 
                      VehUsageProfessional_run + DeducTypePartially_refunded +
                      DeducTypeRefunded + VehBodystation_wagon + VehPriceO +
                      VehPriceP + VehPriceQ + VehMaxSpeed140_150_km_h + 
                      VehMaxSpeed150_160_km_h + VehMaxSpeed160_170_km_h +
                      VehMaxSpeed170_180_km_h + VehMaxSpeed180_190_km_h +
                      VehMaxSpeed190_200_km_h + VehMaxSpeed200_220_km_h + 
                      VehMaxSpeed220_km_h + VehClassA + GaragePrivate_garage, 
                    family = binomial(link = 'logit'), 
                    data = cbind.data.frame(ClaimInd = freMPL34.app$ClaimInd,
                    freMPL34.app.reg))


termplot(GLM.claimInd, partial.resid = TRUE, se = TRUE)

plot(GLM.claimInd)

summary(GLM.claimInd)

#Proba d'avoir ClaimInd=1

p1 <- length(freMPL34[freMPL34$ClaimInd==1,]$ClaimInd)
p2 <- length(freMPL34$ClaimInd)

proba_ind_1 <- p1 / p2

#Prediction du modele

predict.GLM.claimInd.test <- predict(GLM.claimInd, newdata 
                          = as.data.frame(freMPL34.test.reg), type = "response")

predict.GLM.claimInd.test.bis=as.factor(ifelse(predict.GLM.claimInd.test
                                               > proba_ind_1,1,0))

predict.GLM.claimInd.app <- predict(GLM.claimInd, newdata 
                           = as.data.frame(freMPL34.app.reg), type = "response")

predict.GLM.claimInd.app.bis=as.factor(ifelse(predict.GLM.claimInd.app
                                              > proba_ind_1,1,0))

#Matrice de confusion sur l'echantillon test

confusionMatrix(data = predict.GLM.claimInd.test.bis, reference 
                =as.factor(freMPL34.test$ClaimInd))

#Matrice de confusion sur l'echantillon d'apprentissage

confusionMatrix(data = predict.GLM.claimInd.app.bis, reference 
                =as.factor(freMPL34.app$ClaimInd))

#Erreur sur l'echantillon d'apprentissage

mean((predict.GLM.claimInd.app - freMPL34.app[,"ClaimInd"])**2)

#Erreur sur l'echantillon test

mean((predict.GLM.claimInd.test - freMPL34.test[,"ClaimInd"])**2)

# Conclusion:

# => L'erreur est petit, Accuracy acceptable donc on peut accepter le modele 


########################   GLM ClaimAmount    #################

freMPL34.app.sinistre <- subset(freMPL34.app, ClaimAmount > 0)
freMPL34.test.sinistre <- subset(freMPL34.test, ClaimAmount > 0)

freMPL34.app.sinistre.reg <- model.matrix(ClaimAmount ~ LicAge + VehAge + 
                                            Gender + MariStat + SocioCateg + 
                                            VehUsage + DrivAge + HasKmLimit + 
                                            DeducType + BonusMalus + VehBody + 
                                            VehPrice + VehEngine + VehEnergy + 
                                            VehMaxSpeed + VehClass + RiskVar + 
                                            Garage, data=freMPL34.app.sinistre)

freMPL34.test.sinistre.reg <- model.matrix(ClaimAmount ~ LicAge + VehAge + 
                                             Gender + MariStat + SocioCateg +
                                             VehUsage + DrivAge + HasKmLimit +
                                             DeducType + BonusMalus + VehBody + 
                                             VehPrice + VehEngine + VehEnergy +
                                             VehMaxSpeed + VehClass + RiskVar +
                                             Garage, data=freMPL34.test.sinistre)

colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"

colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"



GLM.claimAmount <- glm(ClaimAmount ~  LicAge + DrivAge + BonusMalus + 
                         HasKmLimit + RiskVar+VehAge1 + VehAge10 + VehAge2 +
                         VehAge3 + VehAge4 + VehAge5 + VehAge6 + VehAge8 + 
                         DeducTypeNormal + DeducTypePartially_refunded +
                         DeducTypeProportional + DeducTypeRefunded + VehPriceD +
                         VehPriceE + VehPriceF + VehPriceG + VehPriceH +
                         VehPriceI + VehPriceJ + VehPriceK + VehPriceL + 
                         VehPriceM + VehPriceN + VehPriceO + VehPriceP +
                         VehPriceQ + + VehEnginedirect_injection_overpowered + 
                         VehEngineinjection + VehEngineinjection_overpowered +
                         VehEnergyregular + VehMaxSpeed140_150_km_h + 
                         VehMaxSpeed150_160_km_h + VehMaxSpeed160_170_km_h +
                         VehMaxSpeed170_180_km_h + VehMaxSpeed180_190_km_h + 
                         VehMaxSpeed190_200_km_h + VehMaxSpeed200_220_km_h +
                         VehMaxSpeed220_km_h + VehClassA + VehClassB +
                         VehClassH + VehClassM1 + VehClassM2+ GarageNone + 
                         GaragePrivate_garage + VehUsagePrivate_trip_to_office +
                         VehUsageProfessional_run + VehBodycabriolet + 
                         VehBodymicrovan + VehBodycoupe + SocioCategCSP2 +
                         SocioCategCSP4 + SocioCategCSP5 + SocioCategCSP7 +
                         SocioCategCSP9 , 
        family=Gamma(link = 'log') ,
        data= cbind.data.frame(ClaimAmount = freMPL34.app.sinistre$ClaimAmount,
        freMPL34.app.sinistre.reg) )


#Selection par AIC (Forward, Backward, Stepwise)

GLM.claimAmount <- stepAIC(GLM.claimAmount, trace=TRUE, direction=c("both"))
AIC(GLM.claimAmount)

GLM.claimAmount <- stepAIC(GLM.claimAmount, trace=TRUE, direction=c("backward"))
AIC(GLM.claimAmount)

GLM.claimAmount <- stepAIC(GLM.claimAmount, trace=TRUE, direction=c("forward"))
AIC(GLM.claimAmount)

#le GLM selectionne 

GLM.claimAmount <- glm(ClaimAmount ~ LicAge + BonusMalus + HasKmLimit + 
                         RiskVar + VehAge3 + VehAge4 + VehAge6 + VehAge8 + 
                         DeducTypePartially_refunded + VehPriceE +
                         VehPriceJ + VehPriceM + VehPriceN + VehPriceQ + 
                         VehEnergyregular + VehMaxSpeed140_150_km_h +  
                         VehMaxSpeed160_170_km_h + VehMaxSpeed170_180_km_h +
                         VehMaxSpeed180_190_km_h + VehMaxSpeed190_200_km_h + 
                         VehMaxSpeed200_220_km_h + VehClassA + VehClassM1 +
                         GarageNone + VehUsagePrivate_trip_to_office 
                       + VehBodycabriolet, 
                       family = Gamma(link = "log"), 
        data = cbind.data.frame(ClaimAmount = freMPL34.app.sinistre$ClaimAmount,
        freMPL34.app.sinistre.reg),
                       weights = VehBodymicrovan + VehBodycoupe + 
                         SocioCategCSP2 + SocioCategCSP4 +
                         SocioCategCSP5 + SocioCategCSP7 +
                         SocioCategCSP9)



summary(GLM.claimAmount)


#Prediction du modele

predict.GLM.claimAmount.app <- predict(GLM.claimAmount, newdata 
                  = as.data.frame(freMPL34.app.sinistre.reg), type = "response")

predict.GLM.claimAmount.test <- predict(GLM.claimAmount, newdata 
                 = as.data.frame(freMPL34.test.sinistre.reg), type = "response")

#Graphe de prediction

freMPL34.app.sinistre$prediction <- predict.GLM.claimAmount.app

freMPL34.test.sinistre$prediction <- predict.GLM.claimAmount.test

ggplot(freMPL34.test.sinistre, aes(x = prediction, y=ClaimAmount))  
+ geom_point(color = "darkgreen", size = 3, alpha = 0.3) + geom_abline(color="blue")

ggplot(freMPL34.app.sinistre, aes(x = prediction, y=ClaimAmount))  
+ geom_point(color = "darkgreen", size = 3, alpha = 0.3) + geom_abline(color="blue")


freMPL34.test.sinistre <- subset(freMPL34.test,ClaimAmount > 250 & ClaimAmount < 6000)

plot.new() 
par(mar=c(4,4,3,5),main = "") 
plot(freMPL34.test.sinistre$ClaimAmount,col = "blue",,axes=F,xlab="",ylab="")
axis(2, col="blue",col.axis="blue") 
mtext("ClaimAmount",side=2,line=2.5,col="blue") 
par(new = T) 
plot(predict.GLM.claimAmount.test,col = "red",,axes=F,xlab="",ylab="")
axis( 4 ,col="red",col.axis="red") 
mtext("Prédiction",side=4,line=2.5,col="red") 
axis( 1 ,col="black",col.axis="black") 
mtext("",side=1,line=2.5,col="black")


hist(predict.GLM.claimAmount.test)
densite <- density(predict.GLM.claimAmount.test)
plot(density(predict.GLM.claimAmount.test))


# Conclusion:

# =>On peut accepter le modele (modele assez bien) 

#Calcul Prime Pure

predict.GLM.claimAmount.app <- predict(GLM.claimAmount, newdata 
                          = as.data.frame(freMPL34.app.reg), type = "response")
PrimeP_Individus_GLM_app <- predict.GLM.claimAmount.app*predict.GLM.claimInd.app

PrimeP_Unique_GLM_app <- mean(PrimeP_Individus_GLM_app)

boxplot(PrimeP_Individus_GLM_app, 
      main="Boxplot de la prédiction de la prime pur pour la base freMPL34.app")

hist(PrimeP_Individus_GLM_app,breaks=60,col="blue",density=5,xlab="Prime Pure",
ylab="Fréquences",main="Calculée par les GLM",ylim=c(0,10000),xlim=c(0,250),tck=0.01)

densite.predict.PrimePur.app <- density(PrimeP_Individus_GLM_app)
plot(densite.predict.PrimePur.app,xlim=c(0,250))


predict.GLM.claimAmount.test <- predict(GLM.claimAmount, newdata 
                          = as.data.frame(freMPL34.test.reg), type = "response")
PrimeP_Individus_GLM_test <- predict.GLM.claimAmount.test*predict.GLM.claimInd.test

PrimeP_Unique_GLM_test <- mean(PrimeP_Individus_GLM_test)

boxplot(PrimeP_Individus_GLM_test, 
    main="Boxplot de la prédiction de la prime pur pour la base freMPL34.test")

hist(PrimeP_Individus_GLM_test,breaks=60,col="red",density=5,xlab="Prime Pure",
ylab="Fréquences",main="Calculée par les GLM",ylim=c(0,1800),xlim=c(0,250),tck=0.01)

densite.predict.PrimePur.test <- density(PrimeP_Individus_GLM_test)
plot(densite.predict.PrimePur.test,xlim=c(0,250))

# /           /             /               /
#
#             CALCUL AVEC LES GAMs
#
#/           /             /               /

set.seed(123)

library(xts)
library(zoo)
library(sp)
library(CASdatasets)
library(MASS)
library(lattice)
library(ggplot2)
library(caret)
library(mgcv)

data(freMPL3)
data(freMPL4)


freMPL34 <- rbind(freMPL3, freMPL4)




########################   GAM ClaimInd    #################

levels(freMPL34$SocioCateg) <- factor(c("CSP1","CSP1","CSP1","CSP1","CSP2",
                                        "CSP2","CSP2","CSP2","CSP2","CSP2",
                                        "CSP3","CSP3","CSP3","CSP4","CSP4",
                                        "CSP4","CSP4","CSP4","CSP4","CSP4",
                                        "CSP5","CSP5","CSP5","CSP5","CSP5",
                                        "CSP5","CSP6","CSP6","CSP6","CSP6",
                                        "CSP6","CSP6","CSP7","CSP7","CSP7",
                                        "CSP7","CSP7","CSP9"))
levels(freMPL34$VehPrice) <- factor(c("A","A","A","D","E","F","G","H","I","J",
                                      "K","L","M","N","O","P","Q","Q","Q","Q",
                                      "Q","Q","Q","Q","Q","Q","Q"))
levels(freMPL34$VehMaxSpeed) <- factor(c("1-140 km/h","1-140 km/h",
                                         "140-150 km/h","150-160 km/h",
                                         "160-170 km/h","170-180 km/h",
                                         "180-190 km/h","190-200 km/h",
                                         "200-220 km/h","220+ km/h "))

freMPL34 <- freMPL34[(freMPL34$VehEnergy != "eletric") 
                     & (freMPL34$VehEnergy != "GPL"),]
freMPL34$VehEnergy <- droplevels(freMPL34$VehEnergy)
freMPL34 <- freMPL34[(freMPL34$VehEnergy != "eletric") 
                     & (freMPL34$VehEnergy != "GPL"),]
freMPL34$VehEnergy <- droplevels(freMPL34$VehEnergy)
freMPL34$VehEngine <- droplevels(freMPL34$VehEngine)
freMPL34$LicAge <- floor(freMPL34$LicAge/12)

n <- NROW(freMPL34) ; p <- round(0.8*n)
index.app <- sample(1:n, p)
freMPL34.app <- freMPL34[index.app, ]
freMPL34.test <- freMPL34[-index.app, ]


freMPL34.app.reg <- model.matrix(ClaimInd ~ LicAge + VehAge + 
                                   Gender + MariStat + SocioCateg + VehUsage + 
                                   DrivAge + HasKmLimit + DeducType + BonusMalus +
                                   VehBody + VehPrice + VehEngine + VehEnergy +
                                   VehMaxSpeed + VehClass + RiskVar + Garage,
                                   data=freMPL34.app)

freMPL34.test.reg <- model.matrix(ClaimInd ~ LicAge + VehAge + 
                                    Gender + MariStat + SocioCateg + VehUsage +
                                    DrivAge + HasKmLimit + DeducType + BonusMalus +
                                    VehBody + VehPrice + VehEngine + VehEnergy +
                                    VehMaxSpeed + VehClass + RiskVar + Garage,
                                    data=freMPL34.test)

colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg)
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
 =="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.app.reg)[ colnames(freMPL34.app.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"


colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.test.reg)[ colnames(freMPL34.test.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"

#modèle GAM:

GLM.ClaimInd <- gam(ClaimInd ~ s(LicAge)+ s(RiskVar) + s(BonusMalus) + VehAge1 +
                      VehAge10 + VehAge2 + VehAge3 + VehAge4 + VehAge5 + VehAge6 +
                      VehAge8 + GenderMale + MariStatOther + SocioCategCSP2 + 
                      SocioCategCSP3 + SocioCategCSP4 + SocioCategCSP5 + 
                      SocioCategCSP6 + SocioCategCSP7 + SocioCategCSP9 + 
                      VehUsagePrivate_trip_to_office + VehUsageProfessional + 
                      VehUsageProfessional_run + DrivAge + HasKmLimit + 
                      DeducTypeNormal + DeducTypePartially_refunded + 
                      DeducTypeProportional + DeducTypeRefunded  + 
                      VehBodycabriolet + VehBodycoupe + VehBodymicrovan + 
                      VehBodyother_microvan + VehBodysedan + 
                      VehBodysport_utility_vehicle + VehBodystation_wagon +
                      VehBodyvan + VehPriceD + VehPriceE + VehPriceF + VehPriceG +
                      VehPriceH + VehPriceI + VehPriceJ + VehPriceK + VehPriceL + 
                      VehPriceM + VehPriceN + VehPriceO + VehPriceP + VehPriceQ +
                      VehEnginedirect_injection_overpowered + VehEngineinjection +
                      VehEngineinjection_overpowered + VehEnergyregular +
                      VehMaxSpeed140_150_km_h + VehMaxSpeed150_160_km_h + 
                      VehMaxSpeed160_170_km_h + VehMaxSpeed170_180_km_h + 
                      VehMaxSpeed180_190_km_h + VehMaxSpeed190_200_km_h +
                      VehMaxSpeed200_220_km_h + VehMaxSpeed220_km_h  + VehClassA +
                      VehClassB + VehClassH + VehClassM1 + VehClassM2 +
                      GarageNone + GaragePrivate_garage,
        family=binomial(link = 'logit'),
        data=cbind.data.frame(ClaimInd=freMPL34.app$ClaimInd, freMPL34.app.reg),
        method="REML")


# Le GAM selectionne :

GAM.claimInd <- gam(formula = ClaimInd ~ s(LicAge, k=34) + BonusMalus + VehAge10 +
                      VehAge6 + VehAge8 + SocioCategCSP6 + VehUsageProfessional +
                      VehUsageProfessional_run + DeducTypePartially_refunded +
                      DeducTypeRefunded + VehBodystation_wagon + VehPriceO +
                      VehPriceP + VehPriceQ + VehMaxSpeed140_150_km_h + 
                      VehMaxSpeed150_160_km_h + VehMaxSpeed160_170_km_h + 
                      VehMaxSpeed170_180_km_h + VehMaxSpeed190_200_km_h + 
                      VehMaxSpeed200_220_km_h + VehMaxSpeed220_km_h + VehClassA + 
                      GaragePrivate_garage, 
      family = binomial("logit"), 
      data = cbind.data.frame(ClaimInd = freMPL34.app$ClaimInd, freMPL34.app.reg),
      method="REML")

AIC(GAM.claimInd)

# Comparaison GLM et GAM:

#> AIC(GLM.claimInd) > AIC(GAM.claimInd)

# => on remarque le GAM est plus adapte au modele que le GLM.

plot(GAM.claimInd, pages=1)

# => Remarque: le modele fit bien.

# verification de la qualite des modeles 

summary(GAM.claimInd)

par(mfrow=c(2,2))
gam.check(GAM.claimInd)

#Vérification des résidus:

binnedplot(fitted(GAM.claimInd), residuals(GAM.claimInd, type="response"))

#Proba d'avoir ClaimInd=1

p1 <- length(freMPL34[freMPL34$ClaimInd==1,]$ClaimInd)
p2 <- length(freMPL34$ClaimInd)

proba_ind_1 <- p1 / p2

#Prediction du modele

predict.GAM.claimInd.test <- predict(GAM.claimInd, newdata 
                        = as.data.frame(freMPL34.test.reg), type = "response")
predict.GAM.claimInd.test.bis=as.factor(ifelse(predict.GAM.claimInd.test
                                               > proba_ind_1,1,0))

predict.GAM.claimInd.app <- predict(GAM.claimInd, newdata 
                          = as.data.frame(freMPL34.app.reg), type = "response")
predict.GAM.claimInd.app.bis=as.factor(ifelse(predict.GAM.claimInd.app
                                              > proba_ind_1,1,0))

#Matrice de confusion sur l'echantillon test

confusionMatrix(data = predict.GAM.claimInd.test.bis, reference 
                =as.factor(freMPL34.test$ClaimInd))

#Matrice de confusion sur l'echantillon d'apprentissage

confusionMatrix(data = predict.GAM.claimInd.app.bis, reference 
                =as.factor(freMPL34.app$ClaimInd))

#Erreur sur l'echantillon d'apprentissage

mean((predict.GAM.claimInd.app - freMPL34.app[,"ClaimInd"])**2)

#Erreur sur l'echantillon test

mean((predict.GAM.claimInd.test - freMPL34.test[,"ClaimInd"])**2)

# Conclusion:

# => L'erreur est petite, Accuracy acceptable donc on peut accepter le modele 
#    (modele GAM est mieux adapte que GLM) 

########################   GAM ClaimAmount    #################

freMPL34.app.sinistre <- subset

freMPL34.test.sinistre <- subset(freMPL34.test, ClaimAmount > 0)

freMPL34.app.sinistre.reg <- model.matrix(ClaimAmount ~ LicAge + VehAge + 
                                            Gender + MariStat + SocioCateg + 
                                            VehUsage + DrivAge + HasKmLimit + 
                                            DeducType + BonusMalus + VehBody + 
                                            VehPrice + VehEngine + VehEnergy + 
                                            VehMaxSpeed + VehClass + RiskVar + 
                                            Garage, data=freMPL34.app.sinistre)

freMPL34.test.sinistre.reg <- model.matrix(ClaimAmount ~ LicAge + VehAge + 
                                             Gender + MariStat + SocioCateg +
                                             VehUsage + DrivAge + HasKmLimit +
                                             DeducType + BonusMalus + VehBody + 
                                             VehPrice + VehEngine + VehEnergy +
                                             VehMaxSpeed + VehClass + RiskVar +
                                             Garage, data=freMPL34.test.sinistre)

colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.app.sinistre.reg)[ colnames(freMPL34.app.sinistre.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"

colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehAge10+"] <- "VehAge10"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehAge6-7"] <- "VehAge6"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehAge8-9"] <- "VehAge8"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehUsagePrivate+trip to office"] <- "VehUsagePrivate_trip_to_office"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehUsageProfessional run"] <- "VehUsageProfessional_run"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="DeducTypePartially refunded"] <- "DeducTypePartially_refunded"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehBodyother microvan"] <- "VehBodyother_microvan"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehBodysport utility vehicle"] <- "VehBodysport_utility_vehicle"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehBodystation wagon"] <- "VehBodystation_wagon"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehEnginedirect injection overpowered"] <- "VehEnginedirect_injection_overpowered"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehEngineinjection overpowered"] <- "VehEngineinjection_overpowered"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed140-150 km/h"] <- "VehMaxSpeed140_150_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed150-160 km/h"] <- "VehMaxSpeed150_160_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed160-170 km/h"] <- "VehMaxSpeed160_170_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed170-180 km/h"] <- "VehMaxSpeed170_180_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed180-190 km/h"] <- "VehMaxSpeed180_190_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed190-200 km/h"] <- "VehMaxSpeed190_200_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed200-220 km/h"] <- "VehMaxSpeed200_220_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="VehMaxSpeed220+ km/h "] <- "VehMaxSpeed220_km_h"
colnames(freMPL34.test.sinistre.reg)[ colnames(freMPL34.test.sinistre.reg) 
=="GaragePrivate garage"] <- "GaragePrivate_garage"

#Le modele ClaimAmount avec le modele gamma

GAM.claimAmount <- gam(ClaimAmount ~  s(LicAge) + s(BonusMalus) + HasKmLimit +
                         s(RiskVar) + VehAge1 + VehAge10 + VehAge2 + VehAge3 +
                         VehAge4 + VehAge5 + VehAge6 + VehAge8 + DeducTypeNormal +
                         DeducTypePartially_refunded+ DeducTypeProportional +
                         DeducTypeRefunded + VehPriceD + VehPriceE + VehPriceF +
                         VehPriceG + VehPriceH + VehPriceI + VehPriceJ +
                         VehPriceK + VehPriceL + VehPriceM + VehPriceN +
                         VehPriceO + VehPriceP + VehPriceQ + 
                         VehEnginedirect_injection_overpowered + VehEngineinjection +
                         VehEngineinjection_overpowered + VehEnergyregular +
                         VehMaxSpeed140_150_km_h + VehMaxSpeed150_160_km_h +
                         VehMaxSpeed160_170_km_h + VehMaxSpeed170_180_km_h +
                         VehMaxSpeed180_190_km_h + VehMaxSpeed190_200_km_h +
                         VehMaxSpeed200_220_km_h + VehMaxSpeed220_km_h + VehClassA +
                         VehClassB + VehClassH + VehClassM1 + VehClassM2 +
                         GarageNone + GaragePrivate_garage +
                         VehUsagePrivate_trip_to_office + VehUsageProfessional_run +
                         VehBodycabriolet, VehBodymicrovan + VehBodycoupe +
                         SocioCategCSP2 + SocioCategCSP4 + SocioCategCSP5 +
                         SocioCategCSP7 + SocioCategCSP9 , 
                       family=Gamma(link = 'log') ,
          data= cbind.data.frame(ClaimAmount = freMPL34.app.sinistre$ClaimAmount,
         freMPL34.app.sinistre.reg), method = "REML" )

#Selection par AIC (Forward, Backward, Stepwise)

GAM.claimAmount <- stepAIC(GAM.claimAmount, trace=TRUE, direction=c("both"))
AIC(GAM.claimAmount)

GAM.claimAmount <- stepAIC(GAM.claimAmount, trace=TRUE, direction=c("backward"))
AIC(GAM.claimAmount)

GAM.claimAmount <- stepAIC(GAM.claimAmount, trace=TRUE, direction=c("forward"))
AIC(GAM.claimAmount)

#le GLM selectionne 


GAM.claimAmount <- gam(ClaimAmount ~ s(LicAge, k=62)+ s(BonusMalus) + HasKmLimit + 
                         s(RiskVar) + VehAge3 + VehAge4 + VehAge6 + VehAge8 +
                         DeducTypePartially_refunded +  VehPriceE + VehPriceJ +
                         VehPriceM + VehPriceN + VehPriceQ + VehEnergyregular +
                         VehMaxSpeed140_150_km_h + VehMaxSpeed160_170_km_h + 
                         VehMaxSpeed170_180_km_h + VehMaxSpeed180_190_km_h +  
                         VehMaxSpeed190_200_km_h + VehMaxSpeed200_220_km_h +  
                         VehClassA + VehClassM1 + GarageNone + 
                         VehUsagePrivate_trip_to_office + VehBodycabriolet,
        family = Gamma(link = "log"), 
        data = cbind.data.frame(ClaimAmount = freMPL34.app.sinistre$ClaimAmount, 
                                               freMPL34.app.sinistre.reg),
                       weights = VehBodymicrovan + VehBodycoupe + 
                         SocioCategCSP2 + SocioCategCSP4 +SocioCategCSP5 +
                         SocioCategCSP7 + SocioCategCSP9,
        method = "REML")


# Comparaison GLM et GAM:

#> AIC(GLM.claiAmount) > AIC(GAM.claimAmount)

# => on remarque le GAM est plus adapte au modele que le GLM.

plot(GAM.claimAmount, pages=1)

# => Remarque: le modele fit bien.

# verification de la qualite des modeles 


summary(GAM.claimAmount)

par(mfrow=c(2,2))
gam.check(GAM.claimAmount)

#Vérification des résidus:

binnedplot(fitted(GAM.claimAmount), residuals(GAM.claimAmount, type="response"))


#Prediction du modele

predict.GAM.claimAmount.app <- predict(GAM.claimAmount, newdata 
                  = as.data.frame(freMPL34.app.sinistre.reg), type = "response")

predict.GAM.claimAmount.test <- predict(GAM.claimAmount, newdata 
                = as.data.frame(freMPL34.test.sinistre.reg), type = "response")

#Graphe de prediction

freMPL34.app.sinistre$prediction <- predict.GAM.claimAmount.app

freMPL34.test.sinistre$prediction <- predict.GAM.claimAmount.test

ggplot(freMPL34.test.sinistre, aes(x = prediction, y=ClaimAmount))  
+ geom_point(color = "darkgreen", size = 3, alpha = 0.3) + geom_abline(color="blue")

ggplot(freMPL34.app.sinistre, aes(x = prediction, y=ClaimAmount))  
+ geom_point(color = "darkgreen", size = 3, alpha = 0.3) + geom_abline(color="blue")

freMPL34.test.sinistre <- subset(freMPL34.test,ClaimAmount > 700 & ClaimAmount < 6000)

plot.new() 
par(mar=c(4,4,3,5),main = "") 
plot(freMPL34.test.sinistre$ClaimAmount,col = "blue",,axes=F,xlab="",ylab="")
axis(2, col="blue",col.axis="blue") 
mtext("ClaimAmount",side=2,line=2.5,col="blue") 
par(new = T) 
plot(predict.GAM.claimAmount.test,col = "red",,axes=F,xlab="",ylab="")
axis( 4 ,col="red",col.axis="red") 
mtext("Prédiction",side=4,line=2.5,col="red") 
axis( 1 ,col="black",col.axis="black") 
mtext("",side=1,line=2.5,col="black")

# Valeur absolue de l'ecart de prediction entre GAM et GLM

freMPL34.test.sinistre$Abs_ecart_GAM_GLM_prediction <- abs(predict.GAM.claimAmount.test 
                                                        - predict.GLM.claimAmount.test)

boxplot(freMPL34.test.sinistre$Abs_ecart_GAM_GLM_prediction)

hist(freMPL34.test.sinistre$Abs_ecart_GAM_GLM_prediction,breaks=20,col="black",
     density=5,xlab="abs(predict.GAM-predict.GLM)",ylab="Fréquences",
     main="Histogramme de l'écart de prédiction entre GAM et GLM",ylim=c(0,60),
     xlim=c(0,485),tck=0.01)


# Conclusion:

# =>On peut accepter le modele (modele GAM est mieux adapte que GLM) 

#Calcul Prime Pure

#On predit tous les sinistres 

predict.GAM.claimAmount.test <- predict(GAM.claimAmount, newdata 
                          = as.data.frame(freMPL34.test.reg), type = "response")

predict.GAM.claimInd.app <- predict(GAM.claimInd, newdata 
                          = as.data.frame(freMPL34.app.reg), type = "response")
predict.GAM.claimAmount.app <- predict(GAM.claimAmount, newdata 
                          = as.data.frame(freMPL34.app.reg), type = "response")
PrimeP_Individus_GAM_app<-predict.GAM.claimAmount.app*predict.GAM.claimInd.app
PrimeP_Unique_GAM_app<-mean(PrimeP_Individus_GAM_app)
PrimeP_Unique_GAM_test<-mean(PrimeP_Individus_GAM_test)
PrimeP_Individus_GAM_test<-predict.GAM.claimAmount.test*predict.GAM.claimInd.test

PrimeP_Unique_GAM_test
PrimeP_Unique_GLM_test

freMPL34.test$prime_GAM <- PrimeP_Individus_GAM_test
freMPL34.test$ClaimAmountPredit_GAM<-predict.GAM.claimAmount.test
freMPL34.test$ClaimIndPredit_GAM<-predict.GAM.claimInd.test




# /         /           /           /
#
# Observation visuel des primes
# Et Analyse par simulation de la prime avec chargement
#
# /         /           /           /


#Histogramme Primes  pures
hist(PrimeP_Individus_GAM_test,breaks=15,col="red",density=5,xlab="Prime Pure",
ylab="Frequences",main="Histogramme Prime Pure",ylim=c(0,5000),xlim=c(0,250),
tck=0.01)
hist(PrimeP_Individus_GLM_test,breaks=50,col="blue",density=5,xlab="Prime Pure",
ylab="Frequences",main="Histogramme Prime Pure",ylim=c(0,5000),xlim=c(0,250),
tck=0.01)

#Simulation de la Prime avec Chargement

raggsum <- function(mydata)
{
  n <- NROW(mydata)
  i <- sample.int(n, replace=TRUE)
  sum(mydata[i, ]$ClaimAmount)
}
raggsum(freMPL34.test)

ptfaggsimu <- replicate(10^3, raggsum(freMPL34.test))

proba_GLM={}
for(i in 1:200) {
  proba_GLM[i]<-sum(ptfaggsimu<(1+i/1000)*sum(PrimeP_Individus_GLM_test))/1000
}
summary(proba_GLM)

summary(proba_GLM<0.99)
summary(proba_GLM<0.95)
summary(proba_GLM<0.90)

#Donne sur notre analyse les valeurs ci dessous
#Elles vont un peu changer a chaque simulation
#Le quantile de 0.99 est donc 1.151 pour les GLM
#Le quantile de 0.99 est donc 1.092 pour les GLM
#Le quantile de 0.99 est donc 1.057 pour les GLM

#Avec les GAM

proba_GAM={}
for(i in 1:200) {
  proba_GAM[i]<-sum(ptfaggsimu<(1+i/1000)*sum(PrimeP_Individus_GAM_test))/1000
}
summary(proba_GAM)

summary(proba_GAM<0.99)
summary(proba_GAM<0.95)
summary(proba_GAM<0.90)

#Le quantile de 0.99 est donc 1.142 pour les GAM
#Le quantile de 0.95 est donc 1.084 pour les GAM
#Le quantile de 0.90 est donc 1.049 pour les GAM

#plot des probas de non ruine GAM

plot((1:200)/1000,proba_GAM,type='l',ylab='probabilite',xlab='Taux de chargement')

#plot des probas de non ruine GLM

plot((1:200)/1000,proba_GLM,type='l',ylab='probabilite',xlab='Taux de chargement')


#Histogramme des  somme des ClaimAmount
#Avec en valeurs de gauche a droite (GLM):
#prime pure / moyenne somme claimamount / Prime commerciale avec chargement 0.057/
#Prime commerciale avec chargement 0.092 / Prime commerciale avec chargement 0.151
#Histogramme sur les GLM

base<-data.frame(1:1000)
base$ptfaggsimu<-ptfaggsimu
base$num<-1:length(ptfaggsimu)

p1<- ggplot(base, aes(x=ptfaggsimu)) 
p1+geom_histogram(aes(y=..density..),color="black", fill="white")
+geom_density(alpha=.2, fill="lightblue") 
+geom_vline(aes(xintercept=sum(PrimeP_Individus_GLM_test)),
            color="blue", linetype="dashed", size=1) 
+geom_vline(aes(xintercept=1.057*sum(PrimeP_Individus_GLM_test)),color="red",
            linetype="dashed", size=1)
+geom_vline(aes(xintercept=1.092*sum(PrimeP_Individus_GLM_test)),color="green",
      linetype="dashed", size=1)+geom_vline(aes(xintercept=mean(ptfaggsimu)),
            color="yellow", size=1)
+geom_vline(aes(xintercept=1.151*sum(freMPL34.test$prime_GLM)),color="brown",
            linetype="dashed", size=1)

#Histogramme sur les GAM
#on met les valeurs des quantiles de GAM 

p2<- ggplot(base, aes(x=ptfaggsimu)) 
p2+geom_histogram(aes(y=..density..),color="black", fill="white")
+geom_density(alpha=.2, fill="lightblue") 
+geom_vline(aes(xintercept=sum(PrimeP_Individus_GAM_test)),color="blue",
            linetype="dashed", size=1) 
+geom_vline(aes(xintercept=1.049*sum(PrimeP_Individus_GAM_test)),color="red",
linetype="dashed", size=1)
+geom_vline(aes(xintercept=1.084*sum(PrimeP_Individus_GAM_test)),color="green",
linetype="dashed", size=1)+geom_vline(aes(xintercept=mean(ptfaggsimu)),
color="yellow", size=1)
+geom_vline(aes(xintercept=1.142*sum(freMPL34.test$prime_GAM)),color="brown",
            linetype="dashed", size=1)

#On peut observer les deux en même temps

plot.new() 
par(mar=c(4,4,3,5))
plot((1:200)/1000,proba_GLM,type='l',ylab='',xlab='',col='red')
par(new = T)
plot((1:200)/1000,proba_GAM,type='l',ylab='',xlab='',col='blue')


#On transforme en facteurs pour pouvoir faire des boxplots avec ggplot
#Boxplots avec ClaimAmount

freMPL34.test$CA_GAM<-predict.GAM.claimAmount.test
freMPL34.test$CA_GLM<-predict.GAM.claimAmount.test

freMPL34.test$prime<-PrimeP_Individus_GLM_test
freMPL34.test$primeGAM<-PrimeP_Individus_GAM_test
freMPL34.test$VehAge<-as.factor(freMPL34.test$VehAge)
freMPL34.test$DrivAge<-as.factor(freMPL34.test$DrivAge)
freMPL34.test$LicAge<-as.factor(freMPL34.test$LicAge)
freMPL34.test$SocioCateg<-as.factor(freMPL34.test$SocioCateg)
freMPL34.test$CA_GAM<-predict.GAM.claimAmount.test
freMPL34.test$CA_GLM<-predict.GLM.claimAmount.test

p <- ggplot(freMPL34.test, aes(x=SocioCateg, y=CA_GAM)) + 
  geom_boxplot()
p2 <- ggplot(freMPL34.test, aes(x=VehAge, y=CA_GAM)) + 
  geom_boxplot()
p3 <- ggplot(freMPL34.test, aes(x=DrivAge, y=CA_GAM)) + 
  geom_boxplot()
p4 <- ggplot(freMPL34.test, aes(x=LicAge, y=CA_GAM)) + 
  geom_boxplot()

p4


q <- ggplot(freMPL34.test, aes(x=SocioCateg, y=CA_GLM)) + 
  geom_boxplot()
q2 <- ggplot(freMPL34.test, aes(x=VehAge, y=CA_GLM)) + 
  geom_boxplot()
q3 <- ggplot(freMPL34.test, aes(x=DrivAge, y=CA_GLM)) + 
  geom_boxplot()
q4 <- ggplot(freMPL34.test, aes(x=LicAge, y=CA_GLM)) + 
  geom_boxplot()


#Boxplots avec la prime pure 

p <- ggplot(freMPL34.test, aes(x=SocioCateg, y=prime)) + 
  geom_boxplot()
p2 <- ggplot(freMPL34.test, aes(x=VehAge, y=prime)) + 
  geom_boxplot()
p3 <- ggplot(freMPL34.test, aes(x=DrivAge, y=prime)) + 
  geom_boxplot()
p4 <- ggplot(freMPL34.test, aes(x=LicAge, y=prime)) + 
  geom_boxplot()

p <- ggplot(freMPL34.test, aes(x=SocioCateg, y=prime)) + 
  geom_boxplot()
p2 <- ggplot(freMPL34.test, aes(x=VehAge, y=prime)) + 
  geom_boxplot()
p3 <- ggplot(freMPL34.test, aes(x=DrivAge, y=prime)) + 
  geom_boxplot()
p4 <- ggplot(freMPL34.test, aes(x=LicAge, y=prime)) + 
  geom_boxplot()

q <- ggplot(freMPL34.test, aes(x=SocioCateg, y=primeGAM)) + 
  geom_boxplot()
q2 <- ggplot(freMPL34.test, aes(x=VehAge, y=primeGAM)) + 
  geom_boxplot()
q3 <- ggplot(freMPL34.test, aes(x=DrivAge, y=primeGAM)) + 
  geom_boxplot()
q4 <- ggplot(freMPL34.test, aes(x=LicAge, y=primeGAM)) + 
  geom_boxplot()
p
# On trace des histogrammes sur les primes avec chargement
# Et des boxplots de l'ecart de ces primes

freMPL34.test$pc_GAM <- 1.142*PrimeP_Individus_GAM_test
freMPL34.test$pc_GLM <- 1.151*PrimeP_Individus_GLM_test

freMPL34.test$ClaimAmountPredit_GAM<-predict.GAM.claimAmount.test
freMPL34.test$ClaimIndPredit_GAM<-predict.GAM.claimInd.test

plot.new() 
par(mar=c(4,4,3,5))
hist(1.142*PrimeP_Individus_GAM_test,breaks=30,col="red",density=30,
     xlab="Primes (GLM= bleu / GAM =rouge)",
     ylab="Frequences",main="Histogramme Primes avec Chargement",ylim=c(0,5000),
     xlim=c(0,250),tck=0.01)
par(new=T)
hist(1.151*PrimeP_Individus_GLM_test,breaks=50,col="blue",density=30,
     xlab="Primes (GLM= bleu / GAM =rouge)",
     ylab="Frequences",main="Histogramme Primes avec Chargement",ylim=c(0,5000),
     xlim=c(0,250),tck=0.01)

sum(1.151*PrimeP_Individus_GLM_test)-sum(1.151*PrimeP_Individus_GAM_test)


summary(freMPL34.test$pc_GAM)
summary(freMPL34.test$pc_GLM)

summary(freMPL34.test$pc_GAM>150)
summary(freMPL34.test$pc_GLM>150)

summary(freMPL34.test$pc_GAM<50)
summary(freMPL34.test$pc_GLM<50)

freMPL34.test$ecart<-abs(freMPL34.test$pc_GLM-freMPL34.test$pc_GAM)

#dans notre base test on voulait observer un grand ecart superieur a 833
#summary(freMPL34.test[freMPL34.test$ecart>833,])

p <- ggplot(freMPL34.test, aes(x=SocioCateg, y=ecart)) + 
  geom_boxplot()
p2 <- ggplot(freMPL34.test, aes(x=VehAge, y=ecart)) + 
  geom_boxplot()
p3 <- ggplot(freMPL34.test, aes(x=DrivAge, y=ecart)) + 
  geom_boxplot()
p4 <- ggplot(freMPL34.test, aes(x=LicAge, y=ecart)) + 
  geom_boxplot()
p5 <- ggplot(freMPL34.test, aes(x=VehPrice, y=ecart)) + 
  geom_boxplot()
p6 <- ggplot(freMPL34.test, aes(x=MariStat, y=ecart)) + 
  geom_boxplot()

p5
p6
