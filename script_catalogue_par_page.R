#charger les packages utilis�s
install.packages("pdftools")
library("pdftools")

#message informatif avant le choix du fichier
print("Choisir l'emplacement du fichier � analyser. Les fichiers de r�sultats seront enregistr�s au m�me endroit")

# charge le catalogue � extraire
pdf_file <- file.choose()

#identifier le r�pertoire o� se trouve le script
emplacement <- dirname(pdf_file)

#d�finir le r�pertoire de travail du dossier
setwd(emplacement)

#attribue les donn�es par page � la variable motscatalogue
motscatalogue <- pdf_data(pdf_file)

#export pour chaque page dans le dossier du fichier choisi
for (i in 1:length(motscatalogue)) {
  write.csv(motscatalogue[[i]], file = paste("motscatalogue", i, ".csv"))
}

#message de fin
print("Traitement termin�. Les fichiers ont �t� enregistr�s.")
