README - Projet Trafic Aérien NYC 2013
Projet réalisé dans le cadre du module R du 21 NOV 2025
Groupe 1 : Hoda, Soumaya, Souhir, Chaimae (MIA26.2)
BD : Analyse du traffic aérien NYC 2013


1. Présentation du projet
Ce projet a été réalisé dans le cadre du module IA/Data. L’objectif est d’analyser le trafic aérien à New York en 2013 à partir du dataset nycflights13. Le travail comprend une application Shiny, une carte des destinations, un modèle prédictif, un notebook d’analyse et l’export du modèle IA. Ce dataset a été choisi car on avait pas tout d’abord celui du professeur donc on a trouvé une alternative.


2. Structure du dépôt GitHub
projet_traffic_aerien/
- app.R : Code complet de l’application Shiny
- notebook_analysis.Rmd : Analyse, nettoyage et modélisation
- model_lm.rds : Modèle IA (régression linéaire)
- README.md : Ce fichier
- data/ : Données supplémentaires si besoin

3. Lancer l’application Shiny
Option 1 : Lancer en local (app.R)
Installer les packages nécessaires puis exécuter shiny::runApp("app.R").
Option 2: Lancer en ligne
Lien de l’application : https://hodafedendinge.shinyapps.io/webapp_finale/

4. Notebook d’analyse
Le notebook contient l’analyse exploratoire, le nettoyage, le préprocessing, la modélisation, l’évaluation du modèle et l’export .rds.

5. Modèle IA utilisé
Régression linéaire utilisant le retard au départ, la distance et le temps de vol.
Chargement : model_lm <- readRDS("model_lm.rds")

6. Trello du projet
Lien : https://trello.com/b/ZZgtyxdW/projet-ia-data-trafic-aérien-nyc-2013-r

7. Autres documents
Fichier zip
Dataset csv
PPT soutenance
Recording soutenance
Notebook
