
# Title:"Predicción de los salarios de jugadores de la NBA"
## Gonzalo Carretero Ursúa

library(tidyverse)
library(MASS)
library(rmarkdown)
library(carData)
library(car)
library(gvlma)
### Dataset de la NBA.
datos <- read.csv('nba.csv', sep = ',', header = T )

### Observaciones de las variables.
names(datos)
dim(datos)
length(datos)
summary(datos)
datos <- na.omit(datos)
View(datos)

### Recodificación de las variables categóricas.

unique(datos$NBA_Country)
unique(datos$Tm)
Countries <- c('China', 'Georgia', 'USA', 'Canada', 'Spain',
            'France', 'Czech Republic', 'Russia', 'South Sudan',
            'Switzerland', 'New Zealand', 'Haiti', 'Democratic Re_',
            'Tunisia', 'Brazil', 'Germany', 'Australia',
            'Cameroon', 'Israel', 'Turkey', 'United Kingdo...',
            'Montenegro', 'Serbia', 'Argentina', 'Bosnia',
            'Lithuania', 'Croatia', 'Italy', 'Poland',
            'Dominican Rep...', 'Finland', 'Latvia', 'Bosnia & Herz...',
            'Sweden', 'Ukraine', 'Austria', 'Puerto Rico',
            'Senegal', 'Slovenia', 'Greece', 'Democratic Re...',
            'Mali', 'Bahamas', 'Egypt')
datos$Paises <- match(datos$NBA_Country, Countries)

Teams <- c('HOU', 'GSW', 'SAC', 'CHI', 'POR',
            'DAL', 'BOS', 'MEM', 'DEN',
            'TOT', 'LAC', 'ORL', 'MIA',
            'IND', 'LAL', 'MIN', 'PHO',
            'ATL', 'CLE', 'NYK', 'CHO',
            'MIL', 'SAS', 'UTA', 'NOP',
            'WAS', 'PHI', 'BRK', 'OKC',
            'DET', 'TOR')
datos$Equipos <- match(datos$Tm, Teams)

attach(datos)

### Modelo linear con todas las variables del dataset.

NBA <- as.data.frame(datos[c("Player", "Equipos", "Salary","NBA_DraftNumber","Age","G",
                              "MP","PER","TS.","X3PAr","FTr","ORB.","DRB.","TRB.",
                              "AST.","STL.","BLK.","TOV.","USG.","OWS","DWS","WS",
                              "WS.48","OBPM","DBPM","BPM","VORP")])  

Model1 <- lm(Salary ~ NBA_DraftNumber+Age+G+Paises+Equipos+
             MP+PER+TS.+X3PAr+FTr+ORB.+DRB.+TRB.+
             AST.+STL.+BLK.+TOV.+USG.+OWS+DWS+WS+
             WS.48+OBPM+DBPM+BPM+VORP, data = datos)

summary(Model1)
par(mfrow = c(2,2))
plot(Model1)
par(mfrow = c(1,1))

coefficients(Model1)
confint(Model1)
fitted(Model1)
residuals(Model1)
anova(Model1)
vcov(Model1)

### Reducimos variables. Escogemos el modelo con AIC menor.

stepAIC(Model1, trace = TRUE, direction = 'backward')

NewModel <- lm(formula = Salary ~ NBA_DraftNumber + Age + G + Equipos + MP + 
                PER + X3PAr + ORB. + TRB. + USG. + WS + OBPM, data = NBA)
summary(NewModel)

coefficients(NewModel)
confint(NewModel)
fitted(NewModel)
residuals(NewModel)
vcov(NewModel)
library(car)

#### Normalidad
qqPlot(NewModel)

#### Linealidad
crPlots(NewModel)

#### Homocedasticidad
ncvTest(NewModel)
spreadLevelPlot(NewModel)

#### Multicolinealidad
vif(NewModel)

#### Validación del modelo 
gvmodel <- gvlma(NewModel)
summary(gvmodel)

### Outliers
outlierTest(NewModel)

## PREDICCIÓN 
### Generación de una muestra. Escogemos 10 jugadores al azar para probar nuestro modelo. 
set.seed(1234)
n <- 10
muestra <- sample(1:nrow(NBA),size = n, replace = FALSE)
NBA_10 <- NBA[muestra,]
NBA_10

### Predicción de los salarios de los jugadores de la muestra la muestra.

predict_muestra <- predict(NewModel, newdata = NBA_10)
predict_muestra

