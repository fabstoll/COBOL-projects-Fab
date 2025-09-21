       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATITIC-TELE.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       OBJECT-COMPUTER. JVM.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DONNEE-STAT  ASSIGN  TO 'FICENT.txt'
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS L-Fst.

       DATA DIVISION.
       FILE SECTION.

       FD DONNEE-STAT.
       01 F-FICENT.
           05 F-Chaine                           PIC 9.
           05 F-Jour                             PIC 9.
           05 F-Tranche                          PIC 9.
           05 F-NbrSpe                           PIC 9(4).

       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.

       01 L-Pgm                        PIC X(20) VALUE 'STATISTIC-TELE'.

       01 L-Fst                                   PIC 99.

       01 L-FinFic                                PIC X.
           88 L-FinFic-OK                         VALUE 'O'.

       01 L-Nbr.
           05 L-NbrEnrLus                         PIC 9(5).

       01 TABLETELE.
           05 CHAINE OCCURS 6.
               10 JOUR OCCURS 7.
                   15 TRANCHE OCCURS 7.
                       20 NB-SPECTATEURS PIC 9(5).

       01 DONNEES-TEMP.
           05 W-CHAINE                                 PIC 9.
           05 W-JOUR                                   PIC 9.
           05 W-TRANCHE                                PIC 9.
           05 W-NBRSPECTEMP                            PIC ZZBZZ9.
       
       01 W-NBSPECTOT                                  PIC 9(9).
       01 W-NBSPEC-SPE                                 PIC 9(9).
       01 W-PRCSPEC                                    PIC 9(2)V99.
       01 W-PRCSPEC3                                   PIC 9(2)V99.


      */Variable de comptage pour les boucles 
       01 CPT1                                     PIC 9.
       01 CPT2                                     PIC 9.
       01 CPT3                                     PIC 9.

       PROCEDURE DIVISION.

       
      */Initialisation du programme
           PERFORM INIT
           .

      */PERFORM LECTURE-FICHIER
      
           PERFORM UNTIL L-FinFic-OK
                PERFORM LECTURE-FICHIER
                PERFORM TRAITEMENT-ENREG
           END-PERFORM
           .
           PERFORM TRAITEMENT.
           PERFORM FIN-TRT
           .
           STOP RUN.

       INIT.

           DISPLAY '*************************************************'.
           DISPLAY '      DEBUT PROGRAMME ' L-Pgm.
           DISPLAY '*************************************************'.

           MOVE LOW-VALUE TO L-FinFic.
           INITIALIZE L-Nbr,TABLETELE, W-NBRSPECTEMP, CPT1
           MOVE ZERO TO CPT1
           MOVE ZERO TO L-NbrEnrLus.

           OPEN INPUT DONNEE-STAT.

           IF L-Fst NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FS =' L-Fst '>'
              PERFORM ERREUR
           END-IF
           .
           
       LECTURE-FICHIER.

           READ DONNEE-STAT
           AT END
             SET L-FinFic-OK TO TRUE

           NOT AT END
              IF L-Fst NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst '>'
                 PERFORM ERREUR
              END-IF

           ADD 1 TO L-NbrEnrLus
           
           END-READ
           .

       TRAITEMENT-ENREG.
           ADD F-NbrSpe TO NB-SPECTATEURS(F-Chaine, F-Jour, F-Tranche).

       TRAITEMENT.

           PERFORM DISPLAY1.
              DISPLAY ' '.

           PERFORM DISPLAY2.
              DISPLAY ' '.

           PERFORM DISPLAY3.
              DISPLAY ' '.

           PERFORM DISPLAY4.

       DISPLAY1.
           MOVE ZERO TO CPT1.
           MOVE ZERO TO W-NBRSPECTEMP.
           DISPLAY '*************************************************'.
           DISPLAY '                       Q1                        '.
           DISPLAY '*************************************************'.
           DISPLAY "Nombre de spectateurs de la chaine 6 pour la "
           "tranche 23-24 heures : ".
           PERFORM VARYING CPT1 FROM 1 BY 1 UNTIL CPT1 > 7
              MOVE NB-SPECTATEURS(6, CPT1, 6) TO W-NBRSPECTEMP
              DISPLAY "jour " CPT1 " : " W-NBRSPECTEMP
           END-PERFORM
           .

       DISPLAY2.
           MOVE ZERO TO CPT2
           DISPLAY '*************************************************'.
           DISPLAY '                       Q2                        '.
           DISPLAY '*************************************************'.
           PERFORM VARYING CPT2 FROM 1 BY 1 UNTIL CPT2 > 6
              IF NB-SPECTATEURS(CPT2, 2, 3) > 0
                  ADD NB-SPECTATEURS(CPT2, 2, 3) TO W-NBSPECTOT
              ELSE
                  DISPLAY "jour " CPT2 " : Aucun spectateur"
              END-IF
           END-PERFORM
           .

           COMPUTE W-PRCSPEC ROUNDED = (NB-SPECTATEURS(2, 2, 3)*100) 
                  / W-NBSPECTOT.
           DISPLAY "Pourcentage de spectateurs de la chaine 2 pour la "
           "tranche horaire de 20-21 heures, le mardi : " W-PRCSPEC "%".

       DISPLAY3.
           DISPLAY '*************************************************'.
           DISPLAY '                       Q3                        '.
           DISPLAY '*************************************************'.
           MOVE ZERO TO CPT1.
           MOVE ZERO TO CPT2.
           MOVE ZERO TO CPT3.
           MOVE ZERO TO W-NBSPECTOT.
           MOVE ZERO TO W-PRCSPEC.
           MOVE ZERO TO W-NBRSPECTEMP.

      *Calcul du nombre total de spectateurs tous les jours,
      *toutes les tranches, toutes les chaines

           PERFORM VARYING CPT1 FROM 1 BY 1 UNTIL CPT1 > 6
                PERFORM VARYING CPT2 FROM 1 BY 1 UNTIL CPT2 > 7
                    PERFORM VARYING CPT3 FROM 1 BY 1 UNTIL CPT3 > 7
                     IF NB-SPECTATEURS(CPT1, CPT2, CPT3) > 0
                        ADD NB-SPECTATEURS(CPT1, CPT2, CPT3) 
                        TO W-NBSPECTOT
                     END-IF
                    END-PERFORM
                END-PERFORM
           END-PERFORM
      
      *calcul du nombre de spectateurs de la chaine 4,
      *tous les jours et toutes les tranches.
           .
           MOVE ZERO TO CPT2.
           MOVE ZERO TO CPT3.
           PERFORM VARYING CPT2 FROM 1 BY 1 UNTIL CPT2 > 7
                PERFORM VARYING CPT3 FROM 1 BY 1 UNTIL CPT3 > 7
                    IF NB-SPECTATEURS(4, CPT2, CPT3) > 0
                      ADD NB-SPECTATEURS(4, CPT2, CPT3) TO W-NBSPEC-SPE
                    END-IF
                END-PERFORM
           END-PERFORM
           .
      
      *Calcul du pourcentage

           COMPUTE W-PRCSPEC ROUNDED = (W-NBSPEC-SPE*100) / W-NBSPECTOT.

           DISPLAY "Pourcentage de spectateurs de la chaine 4 pour "
           "tous les jours de la semaine et toutes les tranches : " 
           W-PRCSPEC "%".

       DISPLAY4.

           DISPLAY '*************************************************'.
           DISPLAY '                       Q4                        '.
           DISPLAY '*************************************************'.
      
      *Afficher le jour, la chaîne et la tranche horaire où
      *le nombre de spectateurs a été maximum.

           MOVE ZERO TO CPT1.
           MOVE ZERO TO CPT2.
           MOVE ZERO TO CPT3.
           MOVE ZERO TO W-NBSPEC-SPE.

      *On prend toutes les valeurs du tableau une à une.
      *Dès qu'une valeurs dépasse celle déjà stockée dans W-NBSPEC-SPE
      *on la remplace avec la nouvelle valeur. A la fin, il restera
      *la valeur la plus élevée.

           PERFORM VARYING CPT1 FROM 1 BY 1 UNTIL CPT1 > 6
                PERFORM VARYING CPT2 FROM 1 BY 1 UNTIL CPT2 > 7
                    PERFORM VARYING CPT3 FROM 1 BY 1 UNTIL CPT3 > 7
                        
                        IF NB-SPECTATEURS(CPT1, CPT2, CPT3) > 
                        W-NBSPEC-SPE
                        
                          MOVE CPT1 TO W-CHAINE
                          MOVE CPT2 TO W-JOUR
                          MOVE CPT3 TO W-TRANCHE
                          MOVE NB-SPECTATEURS(CPT1, CPT2, CPT3)
                          TO W-NBSPEC-SPE
                        
                        END-IF
                    END-PERFORM
                END-PERFORM
           END-PERFORM
           .
           DISPLAY "Jour : " W-JOUR ", chaine : " W-CHAINE 
           ", tranche : " W-TRANCHE ", spectateur max : " W-NBSPEC-SPE.

       FIN-TRT.

           CLOSE DONNEE-STAT
           Display 'Nbre enregs lus =' L-NbrEnrLus '>'
           DISPLAY '*************************************************'
           DISPLAY '      FIN   PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
           GOBACK.

       ERREUR.

           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.
