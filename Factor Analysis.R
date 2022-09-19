install.packages(GPArotation )
install.packages(corpcor)
install.packages(psych)

#Load Libary
library("corpcor")
library("GPArotation")
library("psych")
library("IDPmisc")

# Load in Data

#Data Wrangling

financialWB1 <- financialWB[, 8:17]

#Absence of Mult#icollinearity

financialWBmatrix <- cor(financialWB1)

View(round(financialWBmatrix, 2))

#Bartlett's Test

cortest.bartlett(financialWB1)

#Check your Determinants

det(financialWBmatrix)
#Initial Pass to Determine Approximate Number of Factors

pcModel1 <- principal(financialWB1, nfactors = 10, rotate = "none")
pcModel1


#Examine the Scree Plot

plot(pcModel1$values, type="b")


#Second Pass to Test the Suspected Number of Factors

pcModel2 <- principal(financialWB1, nfactors = 2, rotate = "none")

#
##Examining Residuals to Determine Model Fit


residuals <- factor.residuals(financialWBmatrix, pcModel2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
largeResid <- abs(residuals) > .05

sum(largeResid)

sum(largeResid/nrow(residuals))

#Oblique Rotation

pcModel3 <- principal(financialWB1, nfactors = 2, rotate = "oblimin")
pcModel3

print.psych(pcModel3, cut = .3, sort=TRUE)

#Orthogonal Rotation
pcModel4 <- principal(financialWB1, nfactors = 2, rotate = "varimax")
print.psych(pcModel4, cut=.3, sort=TRUE)

pcModel1 <- principal(financialWB1, nfactors = 10, rotate = "none")
pcModel1

#Examine the Scree Plot

plot(pcModel1$values, type="b")

#Second Pass to Test the Suspected Number of Factors

pcModel2 <- principal(financialWB1, nfactors = 2, rotate = "none")
#Examining Residuals to Determine Model Fit

residuals <- factor.residuals(financialWBmatrix, pcModel2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
