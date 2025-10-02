       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLETER-FICHIER.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       OBJECT-COMPUTER. JVM.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FICENT  ASSIGN  TO 'FICENT.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst-Ent.

       SELECT FICSOR ASSIGN TO 'FICSOR.txt'
                     ORGANIZATION IS LINE SEQUENTIAL
                     FILE STATUS IS L-Fst-Sor.

       DATA DIVISION.

       FILE SECTION.

       FD  FICENT.
       01 E-FICENT                                 PIC X(45).

       FD  FICSOR.
       01 E-FICSOR                                 PIC X(45).

       WORKING-STORAGE SECTION.

       01 L-Pgm                    PIC X(20) VALUE 'COMPLÉTER-FICHIER'.
      *-- Variables de comptage
       01 CPT1                                    PIC 9.

      *-- file status fichier
       01 L-Fst-Ent                                   PIC 99.
       01 L-Fst-Sor                                   PIC 99.

       01 L-FinFic                                PIC X.
           88 L-FinFic-OK                         VALUE 'O'.

       01 L-Nbr.
           05 L-NbrEnrLus                         PIC 9(5).
       
       *> Mémorisation des lignes contrat
       01 W-MEMLIN-CTR                            PIC 99.

       COPY Tableau_COPY.

       PROCEDURE DIVISION.

       SQUELETTE.

           PERFORM INIT
           *> Boucle de lecture - chargement du tableau
           PERFORM UNTIL L-FinFic-OK
               PERFORM LECTURE-FICHIER-CHARGE-TABLEAU
           END-PERFORM

           PERFORM TRAITEMENT

           PERFORM FIN-TRT
           .


      *----------------------------------------------------------------------------
       INIT.
      *-----
           DISPLAY '*************************************************'
           DISPLAY '      DEBUT PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'

           MOVE LOW-VALUE                              TO L-FinFic
           INITIALIZE L-Nbr

           OPEN INPUT FICENT OUTPUT FICSOR
           *> controle que l'ouverture du fichier c'est bien faite
           IF L-Fst-Ent NOT = ZERO 
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst-Ent '>'
              PERFORM ERREUR
           END-IF
           IF L-Fst-Sor NOT = ZERO
                DISPLAY 'Erreur ouverture fichier FS =' L-Fst-Sor '>'
                PERFORM ERREUR
           END-IF
           .
      *----------------------------------------------------------------------------
       LECTURE-FICHIER-CHARGE-TABLEAU.
      *-----------
           INITIALIZE L-Fst-Ent
           INITIALIZE L-Fst-Sor

           READ FICENT
               AT END
               SET L-FinFic-OK                  TO TRUE

               NOT AT END
                   IF L-Fst-Ent NOT = ZERO
                       DISPLAY 'Err lect fichier =' L-Fst-Ent '>'
                       PERFORM ERREUR
                   END-IF

      *D       DISPLAY 'E-FICENT =' E-FICENT '>'
                       ADD 1                 TO L-NbrEnrLus
                       MOVE E-FICENT         TO W-FicEnt(L-NbrEnrLus)
           END-READ
           .
      *----------------------------------------------------------------------------
       TRAITEMENT.
      *-----------
           PERFORM ATTRIBUTION-CODE-CONTRAT
           PERFORM ECRITURE-FICHIER-SORTIE
           .
       
      *----------------------------------------------------------------------------
       ATTRIBUTION-CODE-CONTRAT.
      *----
       SET IDX-TAB                           TO 1
       MOVE 0 TO W-MEMLIN-CTR
       
       PERFORM VARYING IDX-TAB FROM 1 BY 1 UNTIL IDX-TAB > L-NbrEnrLus
       

           *> Détéction lignes Contrats
           IF W-FicEnt-TypEnr-Ctr(IDX-TAB)
               MOVE IDX-TAB TO W-MEMLIN-CTR
               *> forçage statut contrat à 'C'.
               SET W-FicEnt-DonCtr-Sit-Crs(IDX-TAB)  TO TRUE
               DISPLAY 'REPÈRE LIGNE C : ' W-MEMLIN-CTR
           ELSE 
              *> Détéction statutlignes Têtes ET modification statut 
              *> contrat, si Itc dans la ligne  -> contrat mémorisée I
              IF W-FicEnt-DonTet-Sit-Itc(IDX-TAB)
                  SET W-FicEnt-DonCtr-Sit-Itc(W-MEMLIN-CTR) TO TRUE
              END-IF
           END-IF
       END-PERFORM
       .        

      *----------------------------------------------------------------------------
       ECRITURE-FICHIER-SORTIE.
      *----
       SET IDX-TAB                           TO 1
       
       PERFORM VARYING IDX-TAB FROM 1 BY 1 UNTIL IDX-TAB > L-NbrEnrLus
           MOVE W-FICENT(IDX-TAB) TO E-FICSOR
           DISPLAY E-FICSOR
           WRITE E-FICSOR
       END-PERFORM
       .

      *----------------------------------------------------------------------------
       FIN-TRT.
      *----
           CLOSE FICENT
           Display 'Nbre enregs lus =' L-NbrEnrLus '>'
           DISPLAY '*************************************************'
           DISPLAY '      FIN   PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
           GOBACK.

      *----------------------------------------------------------------------------
       ERREUR.
      *----
           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.
