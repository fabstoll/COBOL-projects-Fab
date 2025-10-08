# COBOL-projects-Fab
Some exercices and little projects

* You can download them and test them.
* Constructive criticism is welcome.

Remboursement-secu :
  - DECOMPTE.txt : Fichier source contenant les remboursements quotidiens avec une date exprimée en quantième.
  - Conversion_dates.cbl : Programme appelé par la clause CALL dans le programme principal. Il convertit la date au format quantième en une date au format jj/mm/aaaa.
  - Remboursement_secu_mensuel.cbl : Programme principal. Il calcule la somme mensuelle des remboursements et exporte le résultat dans le fichier RESULTAT.txt.

MàJ_Fichier : 
Ce programme met à jour le statut des lignes contrats en fonction du statut des lignes têtes.
 - FICENT.txt : Fichier d’entrée contenant les contrats et têtes avant la mise à jour.
 - Tableau_COPY : Tableau des données du fichier d’entrée, organisé sous forme de structures avec des clauses REDEFINES. Ce tableau est inclus dans le programme COMPLETER-FICHIER.cbl via la clause

Statistique télé :
 - FICENT.txt : Fichier d’entrée contenant des données statistiques structurées comme suit. Exemple : l’enregistrement "3330003" indique 3 téléspectateurs devant France 3 le mercredi entre 20h et 21h (tranche horaire n°3).
                 - Les chaînes	: TF1, France 2, France 3, CANAL +, ARTE et M6 
                 - Les jours	: 1 pour lundi, 2 pour mardi, etc. 
                 - Les tranches horaires	: 1 pour 18-19 heure, 2 pour 19-20, etc.
 - STATISTIC-TELE.cbl : Programme permettant d’effectuer diverses analyses statistiques sur les données.
   

Fusion de fichiers :

Ce programme fusionne deux fichiers (FIC1.txt et FIC2.txt) selon la consigne suivante :
Conserver :
 - les enregistrements du fichier 1 absents du fichier 2
 - les enregistrements du fichier 2 absents du fichier 1
 - un enregistrement si le contrat est présent dans les 2 fichiers.
Les deux fichiers sont triés sur la référence de contrat.
Pour chaque contrat fusionné, il faut renseigner le maximum d’informations disponibles (code situation, code intermédiaire, etc ...).

 
