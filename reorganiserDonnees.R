library(data.table)
library(reshape2)
library(dplyr)

population <- fread("D:/Mes Documents/DATA/elections/indicateurs municipales/OK_population.csv", header=TRUE, sep = ",", encoding = "UTF-8")
colnames(population)[4:14] <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

popReorg <- melt(data = population, 
                  na.rm = FALSE, 
                  id.vars = "code_insee", 
                  measure.vars = c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                  variable.name = "annee",
                  value.name = "population")
popReorg$annee <- as.character(popReorg$annee)

age <- fread("D:/Mes Documents/DATA/elections/indicateurs municipales/OK_population_tranche_age.csv", header = TRUE, sep = ",", encoding = "UTF-8")
age <- replace(age, age == 0, NA)
age <- replace(age, age == "", NA)

ageFiltre <- select(age, insee = 2, "annee", 13:18)
ageFiltre$annee <- as.character(ageFiltre$annee)

PopEtAge <- left_join(popReorg, ageFiltre, by = c("code_insee" = "insee", "annee"))


# as.list(distinct(ageFiltre, ageFiltre$annee))


#séparer une colonne double en conservant les emplacements
separate()

#gifski
#gganimate
#fonction créer jpeg

## From geometry to lat long
st_x = function(x) st_coordinates(x)[,2]
st_y = function(x) st_coordinates(x)[,1]
SF%>%mutate(centre=st_centroid(geometry),
            latitude=st_x(centre),
            longitude=st_y(centre))

### MAP + files

add_rrps <- function(name) {
  rpps <- read.csv(name, skip = 4)
  rpps <- rpps[-1, ] #enelver première ligne
  annee <-
    paste("20", substr(name, nchar(name) - 5, nchar(name) - 4), sep = "") #peut se faire en regex
  rpps <-
    rpps %>% tidyr::pivot_longer(-c(X, AGE)) %>% rename(
      territoire = X,
      specialite = AGE,
      tranche = name,
      nombre = value
    ) %>% separate(
      territoire,
      c("code", "territoire"),
      "(?<=([0-9]|2A|2B)) - (?=[A-Z])", #entre chiffres ou 2A-2B et lettres
      extra = "merge",
      fill = "left"
    ) %>% mutate(annee = annee)
  rpps$nombre <- as.numeric(rpps$nombre)
  rpps
}

rpps<-c("rpps-medecins13.csv","rpps-medecins14.csv","rpps-medecins15.csv","rpps-medecins16.csv","rpps-medecins17.csv","rpps-medecins18.csv") %>%
  map_dfr(add_rrps)

pivot_longer()
pivot_wider()

rpps %>%
  filter(territoire == "FRANCE ENTIERE") %>%
  inner_join(.,cat_age) %>% #tibble table de correspondance pour moins de 55 / + de 55
  group_by(annee,categorie) %>%
  filter(specialite == "Ensemble des spécialités d'exercice") %>%
  summarize(somme = sum(nombre,na.rm=TRUE)) %>%
  pivot_wider(names_from = categorie,values_from = somme) %>%
  mutate(taux = plus55 / ensemble * 100)