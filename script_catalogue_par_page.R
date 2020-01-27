#charger les packages utilisés
install.packages("pdftools")
library("pdftools")

#message informatif avant le choix du fichier
print("Choisir l'emplacement du fichier à analyser. Les fichiers de résultats seront enregistrés au même endroit")

# charge le catalogue à extraire
pdf_file <- file.choose()

#identifier le répertoire où se trouve le script
emplacement <- dirname(pdf_file)

#définir le répertoire de travail du dossier
setwd(emplacement)

#attribue les données par page à la variable motscatalogue
motscatalogue <- pdf_data(pdf_file)

#export pour chaque page dans le dossier du fichier choisi
for (i in 1:length(motscatalogue)) {
  write.csv(motscatalogue[[i]], file = paste("motscatalogue", i, ".csv"))
}

#message de fin
print("Traitement terminé. Les fichiers ont été enregistrés.")
