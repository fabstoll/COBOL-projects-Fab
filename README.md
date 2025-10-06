# COBOL-projects-Fab
Some exercices and little projects

* You can download them and test them.
* Constructive criticism is welcome.

Remboursement-secu :
  - DECOMPTE.txt : Fichier source avec les remboursements quotidiens et la date en quantième.
  - Conversion_dates.cbl : Programme appelé par la clause CALL par le main Program et qui permet de transformer la date quantième en date jj/mm/aaaa.
  - Remboursement_secu_mensuel.cbl : Main Program - Il permet de calculer la somme mensuel des remboursements et d'exporter le résultat dans un fichier RESULTAT.txt.

MàJ_Fichier : Il s'agit d'une mis à jour du statut des lignes 'contrats' en fonction du statut des lignes 'têtes'.
 - FICENT.txt : Fichier d'entrée des contrats et têtes avant MàJ.
 - Tableau_COPY : Tableau des données du fichier d'entrée organisé en tableau, avec des structures de données de type REDEFINES. Ce tableau est 'appelé' dans le programme 'COMPLETER-FICHIER.cbl' avec la clause COPY.

Statistique télé :
 - FICENT.txt : Fichier d'entrée avec des données statistiques organisée ainsi : Par exemple l'enregistrement "3330003" correspond à un 3 téléspectateurs devant France 3 le mercredi de 20 à 21 heures.
                 - Les chaînes	: TF1, France 2, France 3, CANAL +, ARTE et M6 
                 - Les jours	: 1 pour lundi, 2 pour mardi, etc. 
                 - Les franches horaires	: 1 pour 18-19 heure, 2 pour 19-20, etc.
 - STATISTIC-TELE.cbl : Un programme permettant différentes interprétations statistiques des données.


Fusion de fichiers :

Il s'agit de fusionner 2 fichiers (FIC1.txt et FIC2.txt).
La consigne est la suivante : Vous devez fusionner ces 2 fichiers en conservant :
    • les enregistrements du fichier 1 absents du fichier 2
    • les enregistrements du fichier 2 absents du fichier 1
    • un enregistrement si le contrat est présent à la fois dans le fichier 1 et dans le fichier 2.
Les deux fichiers sont triés sur la référence de contrat.
On renseignera pour chaque contrat le plus d'informations disponibles (code situation, code intermédiaire).

 
