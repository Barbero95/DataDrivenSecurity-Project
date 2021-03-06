library(ggplot2)
library(sjmisc)
library(dplyr)
library(visNetwork)

#library(maps)
#library(mapdata)
#library(ggplot2)
#library(ggrepel)
#library(tidyverse)

# Guardamos la información en un nuevo dataframe llamado mapa_mundo

#options(scipen = 999) # para evitar la anotación científica

# Graficamos indicando

#mapa_mundo <- map_data("world")
#incidents_map <- data.frame(
#  long = c(-95.712891),
#  lat = c(37.09024),
#  group = c("USA"),
#  count = 5
#)

#mapa_mundo %>%
#  ggplot() +
#  geom_polygon(aes( x= long, y = lat, group = group),
#               fill = "grey80",
#               color = "white") +
#  geom_point(data= incidents_map,
#             aes(x=long, y = lat, size = count),
#             stroke = F) +
#  scale_size_continuous(name = "Count") +
#  ggtitle( "Incidentes")

library(rworldmap)

theCountries <- c("DEU", "COD", "BFA")
# These are the ISO3 names of the countries you'd like to plot in red

## Create multiple color codes, with Burkina Faso in its own group
malDF <- data.frame(country = c("DEU", "COD", "BFA"),
                    malaria = c(1, 1, 2))

## Re-merge
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")

## Specify the colourPalette argument
mapCountryData(malMap, nameColumnToPlot="malaria", catMethod = "categorical",
               missingCountryCol = gray(.8), colourPalette = c("red", "blue"))
# And this will plot it, with the trick that the color palette's first
# color is red







ids <- as.character(unique(incidents$id))

tecCar <- ids[which(ids %in% raw_car$carnet$edges$to)]

kExists <- ids[which(ids %in% mitre.data$mitrenet$edges$from)]

to <- ids[which(tecCar %in% mitre.data$mitrenet$edges$from)]

from <- ids[which(tecCar %in% mitre.data$mitrenet$edges$from)]

rowsInterFrom <- which(mitre.data$mitrenet$edges$from %in% tecCar)

rowsInterTo <- which(mitre.data$mitrenet$edges$to %in% tecCar)

k <- mitre.data$mitrenet$edges[rowsInterFrom,]
k2 <-  mitre.data$mitrenet$edges[rowsInterTo,]

table(k$to)
table(k2$to)

table(k$from)
table(k2$from)

DataExplorer::plot_correlation(k2)
sort(table(k2$from)) #Podemos ver que el grupo G0045 tiene 4 y podemos decir que está detras de más ataques que los demás, tenemos más casos de 3

k2Plot <- filter(k2, startsWith(as.character(k2$from), "G"))
length(k2Plot$from)

DataExplorer::plot_bar(k2Plot$to)
k2Plot <- k2Plot %>% mutate_if(is.character, as.factor)
sort(summary(k2Plot$from))
hist(summary(k2Plot$from)) #es la tabla pero en histograma vemos que hay muchos grupos que salen una sola vez, etc y uno que sale cuatro veces
sort(table(k2Plot$from))
k2Plot2 <- table(k2Plot$from)[which(table(k2Plot$from) > 2)]
k2PlotDf <- data.frame(k2Plot2)
#Dos graficas que generan lo mismo pero se ven distintas
qplot(x=Var1, y=Freq, data = k2PlotDf)
qplot(x=from, data=k2Plot[which(k2Plot$from %in% k2PlotDf$Var1),])
k3 <- data.frame(to = k2$to, from = k2$from)

qplot(x = from, y=to, data = k2)

k3 <- k3 %>% mutate_if(is.character, as.factor)

summary(k3$to) #vemos que la tecnica T1105 tiene 245 casos
qplot(x = to, data = k3)
k4 <- filter(k3, k3$to %in% c("T1105"))
k4 <- k4 %>% mutate_if(is.character, as.factor)
sort(table(k4$to)) #comprovamos que solo tenmeos rows de T1105
sort(table(k4$from)) #aquí vemos que grupos y demás están con esta tecnica
#podemos hacer esto para todos las demás tecnicas
k5 <- filter(k4, startsWith(as.character(k4$from), "G")) #Extraemos todos los que son groups
sort(table(k5$to)) #Aqu'i podemos ver que de las 245 casos, 41 son de grupos de ataques info guay para grafica
sort(table(as.character(k5$from))) #Es para ver exactamente cuales son, hago el as.character para quitar todos los factors de las tablas anteriores (los G que tengan 0, S, M, etc)

qplot(x = from, data = k3) # locura de grafica no usar

#{
#  "T100": ["S1", "S2"],
#  "T200": ["S3", "S4"]
#}

test <- list("T100" = c("S1", "S2"), "T200" = c(), "T300" = c("S1", "S2"))
test["T100"]
sapply(test, length)

################################

mitrenet <- raw_attck$attcknet
mitrenet2 <- mitre.data$mitrenet

inod <- unique(plyr::ldply(
  ids, function(x) mitre::getNodeNeighbors(node = x, mitrenet = mitrenet)[["nodes"]]
))

iedg <- unique(plyr::ldply(
  ids, function(x) mitre::getNodeNeighbors(node = x, mitrenet = mitrenet)[["edges"]]
))

neighborhood <- list(nodes = inod, edges = iedg)

#nodes = NULL, edges = NULL, dot = NULL, gephi = NULL,
#width = NULL, height = NULL, main = NULL, submain = NULL,
#footer = NULL, background = "rgba(0, 0, 0, 0)"
myg <- visNetwork::visNetwork(nodes = inod, edges = iedg, height = "500px", width = "100%", main="Esquema") %>% visPhysics(enabled = FALSE)  %>%visConfigure(enabled = TRUE)
myg

inod2 <- unique(plyr::ldply(
  tecCar, function(x) mitre::getNodeNeighbors(node = x, mitrenet = mitrenet)[["nodes"]]
))

iedg2 <- unique(plyr::ldply(
  tecCar, function(x) mitre::getNodeNeighbors(node = x, mitrenet = mitrenet)[["edges"]]
))

myg2 <- visNetwork::visNetwork(nodes = inod2, edges = iedg2, height = "500px", width = "100%", main="Esquema") %>% visPhysics(enabled = FALSE)  %>%visConfigure(enabled = TRUE)
myg2

inod3 <- unique(plyr::ldply(
  tecCar, function(x) mitre::getNodeNeighbors(node = x, mitrenet = mitrenet2)[["nodes"]]
))

iedg3 <- unique(plyr::ldply(
  tecCar, function(x) mitre::getNodeNeighbors(node = x, mitrenet = mitrenet2)[["edges"]]
))

myg3 <- visNetwork::visNetwork(nodes = inod3, edges = iedg3, height = "500px", width = "100%", main="Esquema") %>% visPhysics(enabled = FALSE)  %>%visConfigure(enabled = TRUE)
myg3


#Pasamos a mirar las organizaciones en la tabla inicidents quedandonos de esta tabla con las rows que contengan uno de los 28 ids que existen en attack
inciFilterAttackIDs <- incidents[which(incidents$id %in% ids),]
plot_correlation(inciFilterAttackIDs)
ggplot(inciFilterAttackIDs, aes(x = first_event_ts, y = industry, fill = industry)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none")

inciFilterAttackIDs <- inciFilterAttackIDs %>% mutate(duration = round(as.numeric(containment_ts - first_event_ts)/3600, 2))

inciFilterAttackIDs %>%
  arrange(desc(first_event_ts)) %>%
  # mutate(country = factor(country, country)) %>%
  ggplot(aes(x=first_event_ts, y=industry, size=duration, fill=tactic)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 20), name="Duration") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Industry") +
  xlab("Incidents") +
  theme(legend.position = "none")

#Pasamos a mirar las organizaciones en la tabla inicidents quedandonos de esta tabla con las rows que contengan uno de los 5 ids que existen en car
inciFilterCarIDs <- incidents[which(incidents$id %in% tecCar),]
plot_correlation(inciFilterCarIDs) #Ojo esta correlación muy interesante! ya salen en la correlación las tecnicas y a parte qu ehace refernecia a tacticas también a las industrias
ggplot(inciFilterCarIDs, aes(x = first_event_ts, y = industry, fill = industry)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none")

inciFilterCarIDs <- inciFilterCarIDs %>% mutate(duration = round(as.numeric(containment_ts - first_event_ts)/3600, 2))

inciFilterCarIDs %>%
  arrange(desc(first_event_ts)) %>%
  # mutate(country = factor(country, country)) %>%
  ggplot(aes(x=first_event_ts, y=industry, size=duration, fill=tactic)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 20), name="Duration") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Industry") +
  xlab("Incidents") +
  theme(legend.position = "none")

################################

groups <- raw_attck$groups[which(raw_attck$groups$mitreid %in% k2PlotDf$Var1),]

fg <- function(x) {
  paste(paste("Tecnica: ", x["mitreid"]), paste("Name: ", x["name"]), paste("Description: ", x["description"]), sep=",")
}

res <- apply(groups, 1, fg)

groups2 <- lapply(groups, function(g) {
  print(g)
})


filter(k2, from %in% c("G0045"))
filter(k2, str_contains(from, "G"))

library("lattice")

# Dummy data
x <- seq(1,10, length.out=20)
y <- seq(1,10, length.out=20)
data <- expand.grid(X=k2$to, Y=k2$from)
data$Z <- runif(400, 0, 5)

## Try it out
levelplot(Z ~ X*Y, data=data  ,xlab="X", main="")



################################


tecniquesInCar <- lapply(ids, function(i){
  if(i %in% dfCoverage$tecnique) i
  else "-"
})
library(ggplot2)
tecniquesInCarDF <- data.frame(tecniquesInCar = factor(tecniquesInCar, levels = unique(tecniquesInCar)))
qplot(x = tecniquesInCar, data = tecniquesInCarDF)

tecniquesInAttack <- lapply(ids, function(i){
  if(i %in% raw_attck$techniques[["mitreid"]]) i
  else "-"
})
tecniquesInAttackDF <- data.frame(tecniquesInAttack = factor(tecniquesInAttack, levels = unique(tecniquesInAttack)))
qplot(x = tecniquesInAttack, data = tecniquesInAttackDF)



k <- dplyr::select(incidents, org, mitre_attack)

i <<- 0
incidentsFiltered <- plyr::ldply(k$mitre_attack, function(y) {
  i <<- i + 1
  data.frame(
    org = k$org[[i]],
    tactic = y$tactic,
    technique = y$technique,
    id = y$id
  )
})

k <- "TA0003/TA0004"
print("TA0003" %in% strsplit(k, "/")[[1]])

ids <- unique(incidents$id)
print(ids)
techniques <- unique(incidents$technique)
print(techniques)

dataFrame <- data.frame(
  id = unique(incidents$id),
  technique = unique(incidents$technique)
)

no <- c("T1192", "T1072", "T1197", "T1204", "T1086", "T1064", "T1077", "T1171", "T1074", "T1054", "T1219", "T1060", "T1114", "T1117", "T1027", "T1496", "T1008", "T1085", "T1110", "T1189", "T1089", "T1035", "T1050")
si <- c("T1078", "T1053", "T1047", "T1036", "T1105")

incidentsFiltered <- incidentsFiltered %>% mutate_if(is.character, as.factor)
summaryID <- summary(incidentsFiltered$id)
print(summaryID)

siMap <- plyr::ldply(si, function(y) {
  data.frame(
    id = y,
    count = 0
  )
})

countNo <<- 1

library(stringr)
lapply(no, function(x){
  countNo <<- countNo + as.integer(summaryID[[x]])
})

dfNO <- data.frame(
  id = "Others",
  count = countNo
)

dfSI <- plyr::ldply(si, function(y) {
  temp <- data.frame(
    id = y,
    count = summaryID[[y]]
  )
  temp
})


paste(c("1", "2"), collapse = "-")
strsplit("1-2", "-")[[1]]

dfIDs <- rbind(dfSI, dfNO)

library(ggplot2)
qplot(x = id, y = count, data = dfIDs)
qplot(x = id, data = incidents)

ggplot(data = incidents) + geom_histogram(aes(x=id))
g + geom_count()
g + labs(title=
             "Engine Capacity vs Mileage") + xlab("Engine Capacity (l)") + ylab("Mileage")
