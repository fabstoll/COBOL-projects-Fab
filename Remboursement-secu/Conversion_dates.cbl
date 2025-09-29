       IDENTIFICATION DIVISION.
       PROGRAM-ID. Conversion_dates.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 E-JOURQ                                 PIC 9(3).
       01 ANNEEBISSEX-ON                          PIC 9 VALUE 0.

       LINKAGE SECTION.
       01 L-DATE-SOIN     PIC 9(7).
       01 L-ANNEE         PIC 9(4).
       01 L-MOIS          PIC 9(2).
       01 L-JOUR          PIC 9(2).
       
       PROCEDURE DIVISION USING
           L-DATE-SOIN
           L-ANNEE
           L-MOIS
           L-JOUR.

      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *TRAITEMENT.
      *+++++++++++++++++++++++
       
       PERFORM VERIF-BISSEX      
       PERFORM TRANSFO-DATE
       GOBACK
       .
              
      *----------------------------------------------------------------------------
       VERIF-BISSEX.
      *-- Verification si l'année est bisextile ou non
       
       MOVE L-DATE-SOIN(1:4) TO L-ANNEE.
       MOVE L-DATE-SOIN(5:3) TO E-JOURQ.

       IF ((FUNCTION MOD (L-ANNEE 4) = 0
           AND FUNCTION MOD (L-ANNEE 100) NOT = 0)
           OR (FUNCTION MOD (L-ANNEE 400) = 0))
           MOVE 1 TO ANNEEBISSEX-ON
       ELSE
           MOVE 0 TO ANNEEBISSEX-ON
       END-IF
       .
      *----------------------------------------------------------------------------
       TRANSFO-DATE.
      *-- Transformation de la date de soin de QUANTIÈME en mois et année

       IF ANNEEBISSEX-ON = 0
           EVALUATE TRUE
                  WHEN E-JOURQ <= 31
                        MOVE 01 TO L-MOIS
                        MOVE E-JOURQ TO L-JOUR
                  WHEN E-JOURQ <= 59
                        MOVE 02 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 31
                  WHEN E-JOURQ <= 90
                        MOVE 03 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 59
                  WHEN E-JOURQ <= 120
                        MOVE 04 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 90
                  WHEN E-JOURQ <= 151
                        MOVE 05 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 120
                  WHEN E-JOURQ <= 181
                        MOVE 06 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 151
                  WHEN E-JOURQ <= 212
                        MOVE 07 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 181
                  WHEN E-JOURQ <= 243
                        MOVE 08 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 212
                  WHEN E-JOURQ <= 273
                        MOVE 09 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 243
                  WHEN E-JOURQ <= 304
                        MOVE 10 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 273
                  WHEN E-JOURQ <= 334
                        MOVE 11 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 304
                  WHEN OTHER
                        MOVE 12 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 334
           END-EVALUATE

       ELSE
           EVALUATE TRUE
                  WHEN E-JOURQ <= 31
                        MOVE 01 TO L-MOIS
                        MOVE E-JOURQ TO L-JOUR
                  WHEN E-JOURQ <= 60
                        MOVE 02 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 31
                  WHEN E-JOURQ <= 91
                        MOVE 03 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 60
                  WHEN E-JOURQ <= 121
                        MOVE 04 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 91
                  WHEN E-JOURQ <= 152
                        MOVE 05 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 121
                  WHEN E-JOURQ <= 182
                        MOVE 06 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 152
                  WHEN E-JOURQ <= 213
                        MOVE 07 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 182
                  WHEN E-JOURQ <= 244
                        MOVE 08 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 213
                  WHEN E-JOURQ <= 274
                        MOVE 09 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 244
                  WHEN E-JOURQ <= 305
                        MOVE 10 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 274
                  WHEN E-JOURQ <= 335
                        MOVE 11 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 305
                  WHEN OTHER
                        MOVE 12 TO L-MOIS
                        COMPUTE L-JOUR = E-JOURQ - 335
           END-EVALUATE
       END-IF
       .
       
