       IDENTIFICATION DIVISION.
       PROGRAM-ID. JPENDU.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MOT      PIC X(20).
       01 WS-CHN      PIC X(20) VALUE SPACES.
       01 WS-LETTRES  PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
       01 WS-TIRETS   PIC X(26) VALUE ALL "_".
       01 WS-CHAR     PIC X.
       01 I           PIC 99    VALUE ZERO.
       01 ESSAI       PIC 99    VALUE ZERO.
       01 LONGUEUR    PIC 99    VALUE ZERO.

       PROCEDURE DIVISION.

      *SQUELETTE
       
       PERFORM CHOIX-MOT
       PERFORM RECHERCHE-LETTRES
       PERFORM FIN-DU-JEU
       .
       STOP RUN
       .
      * ----------------------------------------------------
       CHOIX-MOT.
      * -- Saisie du mot à trouver + stockage dans WS-CHN du nb de tiret
      *    correspondant au nb de lettre du mot.

       DISPLAY "Avec quel mot souhaitez-vous jouer ?".
       ACCEPT WS-MOT.

       MOVE FUNCTION LENGTH(WS-MOT) TO LONGUEUR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LONGUEUR
               IF WS-MOT(I:1) NOT = SPACE
                   MOVE "_" TO WS-CHN(I:1)
               ELSE
                   MOVE SPACE TO WS-CHN(I:1)
               END-IF
           END-PERFORM
           .

      * ----------------------------------------------------
       RECHERCHE-LETTRES.
      * -- Début du jeu : on propose une lettre

       INITIALIZE WS-CHAR.
       DISPLAY WS-CHN.
       PERFORM VARYING ESSAI FROM 1 BY 1 UNTIL WS-MOT = WS-CHN
           DISPLAY "Essai n° " ESSAI
           DISPLAY "Proposez une lettre : "
           ACCEPT WS-CHAR
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LONGUEUR
              IF WS-MOT(I:1) = WS-CHAR
                MOVE WS-CHAR TO WS-CHN(I:1)
                DISPLAY "Bravo, vous avez trouvé une lettre !"
              END-IF
           END-PERFORM
           DISPLAY WS-CHN
       END-PERFORM
       .
      
      * ----------------------------------------------------
       FIN-DU-JEU.
      * ----------------------------------------------------

       DISPLAY "Bravo, vous avez trouvé le mot !".
       STOP RUN.
       END PROGRAM JPENDU.
