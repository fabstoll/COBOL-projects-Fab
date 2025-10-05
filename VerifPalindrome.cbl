       PROGRAM-ID. PALINDROME.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WS-CHAINE PIC X(15).
       01 WS-POINTEUR PIC 99 value 1.
       01 WS-CHAINE-DEPLACEE PIC X(15).
       01 WS-CHAINE-SANITIZED PIC X(15).
       01 WS-CHAINE-INVERSEE PIC X(15).

       PROCEDURE DIVISION.

           PERFORM SAISIE-MOT
           PERFORM GESTION-CASSE
           PERFORM INVERSION
           PERFORM DEPLACEMENT-MOT
           PERFORM AFFICHAGE-VERIF
           .

           STOP RUN.

      *--------------------------------------------- 
       SAISIE-MOT.
      *---------------------------------------------

       DISPLAY "Saisir un mot (max 15 caractères) : ".
       ACCEPT WS-CHAINE.

      *--------------------------------------------- 
       GESTION-CASSE.
      *--- mettre toutes les letttres en minuscule

       MOVE FUNCTION LOWER-CASE(WS-CHAINE) TO WS-CHAINE-SANITIZED.

      *--------------------------------------------- 
       INVERSION.
      *--- Inversion du mot saisi 

       MOVE FUNCTION REVERSE(WS-CHAINE-SANITIZED) TO WS-CHAINE-INVERSEE.

      *--------------------------------------------- 
       DEPLACEMENT-MOT.
      *--- Comme l'inversion a également pris en compte les espaces,
      *    il faut déplacer le mot en début d'intervalle.
   
       MOVE FUNCTION TRIM(WS-CHAINE-INVERSEE LEADING)
           TO WS-CHAINE-DEPLACEE
       .

      *--------------------------------------------- 
       AFFICHAGE-VERIF.
      *----Affichage des variables + vérification si palindrome

           DISPLAY "    Chaîne d'origine : " WS-CHAINE.
           DISPLAY "Chaîne en minuscules : " WS-CHAINE-SANITIZED.
           DISPLAY "     Chaîne inversée : " WS-CHAINE-INVERSEE.
           DISPLAY "          Mot décalé : " WS-CHAINE-DEPLACEE.
           

           IF WS-CHAINE-SANITIZED = WS-CHAINE-DEPLACEE
               DISPLAY "La chaîne est un palindrome"
           ELSE
               DISPLAY "nix palindrome"
           END-IF
           .

