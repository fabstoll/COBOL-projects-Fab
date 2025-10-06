       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUSION-FICHIER.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       OBJECT-COMPUTER. JVM.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT FICENT1  ASSIGN  TO 'FIC1.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst1.

       SELECT FICENT2  ASSIGN  TO 'FIC2.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-Fst2.

       SELECT FICSOR  ASSIGN  TO 'FICSOR.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS L-FstSOR.

       DATA DIVISION.

       FILE SECTION.

       FD  FICENT1.
       01 E-FICENT1.
           05 E-RefCtr1                            PIC X(9).
           05 FILLER                               PIC X.
           05 E-CodeSit1                           PIC X.

       FD  FICENT2.
       01 E-FICENT2.
           05 E-RefCtr2                            PIC X(9).
           05 FILLER                               PIC X.
           05 E-CodeInt2                           PIC X(7).

       FD  FICSOR.
       01 E-FICSOR.
           05 E-RefCtrSor                          PIC X(9).
           05 FILLER                               PIC X.
           05 E-CodeSitSor                         PIC X.
           05 FILLER                               PIC X.
           05 E-CodeIntSor                         PIC X(7).

       WORKING-STORAGE SECTION.
       LOCAL-STORAGE SECTION.

       01 L-Pgm                     PIC X(20) VALUE 'FUSION-FICHIER'.
      *-- Variables de comptage
       01 CPT1                                    PIC 9.

      *-- file status fichier
       01 L-Fst1                                   PIC 99.
       01 L-Fst2                                   PIC 99.
       01 L-FstSOR                                 PIC 99.


       01 L-FinFic1                                PIC X.
           88 L-FinFic1-OK                         VALUE 'O'.
       01 L-FinFic2                                PIC X.
           88 L-FinFic2-OK                         VALUE 'O'.

       01 L-NbrLUS.
           05 L-NbrEnrLus1                         PIC 9(5).
           05 L-NbrEnrLus2                         PIC 9(5).

       01 L-NbrECR                                 PIC 9(5).

       PROCEDURE DIVISION.

       SQUELETTE.

           PERFORM INIT
           *> Lecture initiale
           PERFORM LECTURE-FICHIER1
           PERFORM LECTURE-FICHIER2
           *> Boucle de traitement
           *>(tant que les 2 fichiers ne sont pas finis)
           PERFORM UNTIL L-FinFic1-OK OR L-FinFic2-OK
                PERFORM TRAITEMENT
           END-PERFORM
           *> Vidage du fichier qui n'est pas terminé
           PERFORM VIDAGE-FICHIER-NON-FINI

           PERFORM FIN-TRT
           .

      *----------------------------------------------------------------
       INIT.
      *-----
           DISPLAY '*************************************************'
           DISPLAY '      DEBUT PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'

           MOVE LOW-VALUE                      TO L-FinFic1 L-FinFic2
           INITIALIZE L-NbrLUS

           OPEN INPUT FICENT1 FICENT2
           OPEN OUTPUT FICSOR
           *> controle que l'ouverture des fichiers s'est bien faite
           IF L-Fst1 NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FIC1 =' L-Fst1 '>'
              PERFORM ERREUR
           END-IF
           .
           IF L-Fst2 NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FIC2 =' L-Fst2 '>'
              PERFORM ERREUR
           END-IF
           .
           IF L-FstSOR NOT = ZERO
              DISPLAY 'Erreur ouverture fichier FICSOR =' L-FstSOR '>'
              PERFORM ERREUR
           END-IF
           .
      *----------------------------------------------------------------
       LECTURE-FICHIER1.
      *-----------
           *>DISPLAY 'je suis dans LECTURE-FICHIER1'.

           READ FICENT1
           AT END
             SET L-FinFic1-OK                  TO TRUE
           NOT AT END
              IF L-Fst1 NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst1 '>'
                 PERFORM ERREUR
              END-IF

              ADD 1                            TO L-NbrEnrLus1
           END-READ
           .
      *----------------------------------------------------------------
       LECTURE-FICHIER2.
      *-----------
           *>DISPLAY 'je suis dans LECTURE-FICHIER2'.
           READ FICENT2
           AT END
             SET L-FinFic2-OK                  TO TRUE
           NOT AT END
              IF L-Fst2 NOT = ZERO
                 DISPLAY 'Erreur lecture fichier FS =' L-Fst2 '>'
                 PERFORM ERREUR
              END-IF

              ADD 1                            TO L-NbrEnrLus2
           END-READ
           .
      
      *----------------------------------------------------------------------------
       ECRITURE-FICHIER.
      *----
           *>DISPLAY 'je suis dans ECRITURE-FICHIER'.
           WRITE E-FICSOR
               IF L-FstSOR NOT = ZERO
                  DISPLAY 'Erreur ecriture fichier FS =' L-FstSOR '>'
                  PERFORM ERREUR
               END-IF
           

           ADD 1                          TO L-NbrECR
           DISPLAY E-FICSOR
           .
      *----------------------------------------------------------------------------
       TRAITEMENT.
      *----
      *DISPLAY 'je suis dans TRAITEMENT'.
       EVALUATE TRUE
           WHEN E-RefCtr1 < E-RefCtr2
               MOVE SPACES TO E-FICSOR
               MOVE E-RefCtr1           TO E-RefCtrSor
               MOVE E-CodeSit1          TO E-CodeSitSor
               MOVE SPACES              TO E-CodeIntSor
               
               PERFORM LECTURE-FICHIER1
               

           WHEN E-RefCtr1 > E-RefCtr2
               MOVE SPACES TO E-FICSOR
               MOVE E-RefCtr2           TO E-RefCtrSor 
               MOVE SPACES              TO E-CodeSitSor
               MOVE E-CodeInt2          TO E-CodeIntSor
               
               PERFORM LECTURE-FICHIER2
               

           WHEN E-RefCtr1 = E-RefCtr2
               MOVE SPACES TO E-FICSOR
               MOVE E-RefCtr1           TO E-RefCtrSor
               MOVE E-CodeSit1          TO E-CodeSitSor
               MOVE E-CodeInt2          TO E-CodeIntSor
               
               PERFORM LECTURE-FICHIER1
               PERFORM LECTURE-FICHIER2
               
           END-EVALUATE
           .
       PERFORM ECRITURE-FICHIER
       .
      *----------------------------------------------------------------------------
       VIDAGE-FICHIER-NON-FINI.
      *----
      *DISPLAY 'je suis dans VIDAGE-FICHIER-NON-FINI'.
       EVALUATE TRUE
           WHEN L-FinFic1-OK
               MOVE SPACES TO E-FICSOR
               MOVE E-RefCtr2     TO E-RefCtrSor
               MOVE SPACES        TO E-CodeSitSor
               MOVE E-CodeInt2    TO E-CodeIntSor
               PERFORM ECRITURE-FICHIER
               PERFORM LECTURE-FICHIER2
               IF L-FinFic2-OK THEN
                   EXIT PARAGRAPH
               END-IF

           WHEN L-FinFic2-OK
               MOVE SPACES TO E-FICSOR
               MOVE E-RefCtr1     TO E-RefCtrSor
               MOVE E-CodeSit1    TO E-CodeSitSor
               MOVE SPACES        TO E-CodeIntSor
               PERFORM ECRITURE-FICHIER
               DISPLAY E-FICSOR
               PERFORM LECTURE-FICHIER1
               IF L-FinFic1-OK THEN
                   EXIT PARAGRAPH
               END-IF
       END-EVALUATE
       .
       PERFORM ECRITURE-FICHIER
       .
      *----------------------------------------------------------------------------
       FIN-TRT.
      *----
           CLOSE FICENT1 FICENT2 FICSOR
           Display 'Nbre enregs lus FICENT1 =' L-NbrEnrLus1 '>'    
           Display 'Nbre enregs lus FICENT2 =' L-NbrEnrLus2 '>'
           Display 'Nbre enregs ecrits FICSOR =' L-NbrECR '>'
           DISPLAY '*************************************************'
           DISPLAY '      FIN   PROGRAMME ' L-Pgm
           DISPLAY '*************************************************'
           GOBACK
           .
      *----------------------------------------------------------------------------
       ERREUR.
      *----
           DISPLAY 'Fin anormale'
           PERFORM FIN-TRT.
