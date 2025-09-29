       IDENTIFICATION DIVISION.
       PROGRAM-ID. Remboursement_secu_mensuel.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
           SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
           OBJECT-COMPUTER. JVM.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT LISTE ASSIGN TO 'DECOMPTE.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-Fst.
               
           SELECT SORTIE ASSIGN TO 'RESULTAT.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-FstSortie.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD LISTE.
       01 E-LISTE.
           05 E-LISTE-CODE                 PIC X(2).
           05 E-LISTE-FILLER1              PIC X(3).
           05 E-LISTE-NUMPOL               PIC X(12).
           05 E-LISTE-FILLER2              PIC X.
           05 E-LISTE-DATESOIN             PIC 9(7).
           05 E-LISTE-FILLER3              PIC X.
           05 E-LISTE-MTREMB               PIC 9(7)V99. 
           
       FD SORTIE.
       01 S-DONSORT                        PIC X(80).
           
       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.
       
       01 L-Pgm                    PIC X(20) VALUE 'REMBOURSEMENT SECU'.
      *-- file status fichier
       01 L-Fst                    PIC 99.
       01 L-FstSortie              PIC 99.
       
       01 L-FinFic                 PIC X.
           88 L-FinFic-OK VALUE 'O'.
       
       01 L-Nbr.
           05 L-NbrEnrLus          PIC 9(5).
           05 L-NbrEnrEcrits       PIC 9(5).
      
      *-- Données calculs
       01 E-JOURQ                  PIC 9(3).
       01 E-ANNEEQ                 PIC 9(4).
       01 E-MOISQ                  PIC 9(2).
       01 E-MONTANT-NUM            PIC X(15).
       01 WS-MOIS-TXT              PIC XX.
       01 WS-ANNEE-TXT             PIC XXXX.
       01 WS-MNT-TXT               PIC X(12).
       01 RESTE                    PIC 9.
       01 QUOTIENT                 PIC 9.

       01 DONSORT.
           05 MOIS                         PIC 99.
           05 FILLER                       PIC X(5) VALUE '   * '.
           05 ANNEE                        PIC 9999.
           05 FILLER                       PIC X(4) VALUE '  * '.
           05 MNT-REMB                     PIC ZBZZZBZZ9,99.

       *> Variables pour recevoir les résultats du sous-programme
       01 W-DATE.
           05 W-ANNEE                  PIC 9(4).
           05 W-MOIS                   PIC 9(2).
           05 W-JOUR                   PIC 9(2).
           
      *-- Tableau des valeurs par jour
       01 TABLE-MONTANTS.
           05 T-ANNEES OCCURS 5 INDEXED BY IDX-ANNEE. *> Pour 2003 à 2007
               10 T-MOIS OCCURS 12 INDEXED BY IDX-MOIS.
                   15 T-MONTANT PIC 9(7)V99 VALUE ZERO.
                   
      *-- Données de sortie
       
           
       PROCEDURE DIVISION.

      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *    SQUELETTE.
      *+++++++++++++++++ 
           PERFORM INIT 
           *> Lecture initiale
           PERFORM LECTURE-FICHIER 
           
           PERFORM UNTIL L-FinFic-OK 
               PERFORM TRAITEMENT 
               PERFORM LECTURE-FICHIER
           END-PERFORM 
           
           PERFORM AFFICHAGE-DONNEES
           PERFORM FIN-TRT
           .

      *----------------------------------------------------------------
       INIT.
      *-----
           DISPLAY '*************************************************'
           DISPLAY ' DEBUT PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
               MOVE LOW-VALUE TO L-FinFic
               INITIALIZE L-Nbr
               OPEN INPUT LISTE.
               OPEN OUTPUT SORTIE.
      * controle que l'ouverture du fichier c'est bien faite
           IF L-Fst NOT = ZERO
               DISPLAY 'Erreur ouverture fichier FS =' L-Fst '>'
               PERFORM ERREUR
           END-IF
           
           IF L-FstSortie NOT = 0
               DISPLAY 'Erreur ouverture fichier sortie FS=' L-FstSortie
               PERFORM ERREUR
           END-IF
           .

      *----------------------------------------------------------------
       LECTURE-FICHIER.
      *-----------
           READ LISTE
               AT END
                   SET L-FinFic-OK TO TRUE
               NOT AT END
                   IF L-Fst NOT = ZERO 
                       DISPLAY 'Erreur lecture fichier FS =' L-Fst '>'
                       PERFORM ERREUR
                   END-IF
                   ADD 1 TO L-NbrEnrLus
           END-READ.
      
      *----------------------------------------------------------------
       ECRITURE-FICHIER.
      *-----------
           MOVE MOIS TO WS-MOIS-TXT
           MOVE ANNEE TO WS-ANNEE-TXT
           MOVE MNT-REMB TO WS-MNT-TXT
           MOVE SPACES TO S-DONSORT
           
           STRING
               WS-MOIS-TXT                         DELIMITED BY SIZE
               '   * '                             DELIMITED BY SIZE
               WS-ANNEE-TXT                        DELIMITED BY SIZE
               '  * '                              DELIMITED BY SIZE
               WS-MNT-TXT                          DELIMITED BY SIZE
           INTO S-DONSORT
           END-STRING
           
           WRITE S-DONSORT
           IF L-FstSortie NOT = ZERO
               DISPLAY "Erreur écriture fichier FS=" L-FstSortie
               PERFORM ERREUR
           ELSE
               ADD 1 TO L-NbrEnrEcrits
           END-IF
           .
                   
      *++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       TRAITEMENT.
      *+++ Le coeur du traitement
           PERFORM APPEL-SS-PROG
           PERFORM CHARGEMENT-MONTANT-TABLEAU
           .
           
      *----------------------------------------------------------------
       APPEL-SS-PROG.
      *-- Appel du ss-prog de test annee bissextile et transformation date
           CALL 'Conversion_dates'
           USING E-LISTE-DATESOIN
               W-ANNEE
               W-MOIS
               W-JOUR
           END-CALL
           .
           
           IF W-ANNEE < 2003 OR W-ANNEE > 2007
               DISPLAY 'Annee hors plage 2003-2007 =' W-ANNEE '>'
               PERFORM ERREUR
           END-IF
      *-- Transformation année en index 1...5
           COMPUTE E-ANNEEQ = W-ANNEE - 2002
           SET IDX-ANNEE TO E-ANNEEQ
           SET IDX-MOIS TO W-MOIS
           MOVE W-JOUR TO E-JOURQ
           .
      *----------------------------------------------------------------
       CHARGEMENT-MONTANT-TABLEAU.
      *-- Sommes mensuelles des remboursements
           ADD E-LISTE-MTREMB TO T-MONTANT (IDX-ANNEE IDX-MOIS)
           .

      *----------------------------------------------------------------
       AFFICHAGE-DONNEES.
      *-- Affichage des données à exporter

           MOVE SPACES TO S-DONSORT
           MOVE 'Mois * Année * Remboursements' TO S-DONSORT.
     
           *> Boucles sur les années et le mois           
           PERFORM VARYING IDX-ANNEE FROM 1 BY 1 UNTIL IDX-ANNEE > 5
               PERFORM VARYING IDX-MOIS FROM 1 BY 1 UNTIL IDX-MOIS > 12
                 *> Affichage du titre toutes les 10 lignes
                   DIVIDE L-NbrEnrEcrits BY 10 GIVING QUOTIENT 
                   REMAINDER RESTE
                   IF RESTE = 0
                       MOVE 'Mois * Année * Remboursements' 
                       TO S-DONSORT
                       DISPLAY S-DONSORT
                       WRITE S-DONSORT
                   END-IF
                 *> Récupérer le montant  
                   MOVE T-MONTANT (IDX-ANNEE IDX-MOIS) TO MNT-REMB
                 *> LES MOIS  
                   MOVE IDX-MOIS TO MOIS
                 *> LES ANNEES  
                   COMPUTE ANNEE = IDX-ANNEE + 2002
           *> ECRITURE-FICHIER
                   PERFORM ECRITURE-FICHIER

           *> AFFICHAGE
                   DISPLAY S-DONSORT

               END-PERFORM
           END-PERFORM
           .
      
      *----------------------------------------------------------------
       FIN-TRT.
      *---- 
           CLOSE LISTE
           CLOSE SORTIE
           DISPLAY 'Nbre enregs lus =' L-NbrEnrLus '>'
           DISPLAY 'Nbre enregs écrits =' L-NbrEnrEcrits '>'
           DISPLAY '*************************************************'
           DISPLAY ' FIN PROGRAMME ' L-Pgm
           DISPLAY '*************************************************' 
           GOBACK. 
       
       ERREUR.
           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.

