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
