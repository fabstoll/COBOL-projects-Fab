       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. HELLO-WORLD.
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
         01 WS-CHAINE                PIC X(15).
         01 WS-CHAINE-DEPLACEE       PIC X(15).
         01 WS-CHAINE-SANITIZED      PIC X(16).
         01 WS-CHAINE-INVERSEE       PIC X(16).
         
         01 CPT                      PIC 9(2).
         01 POSLET                   PIC 9(2).
       
       PROCEDURE DIVISION.
         PERFORM SAISIE-MOT
         PERFORM GESTION-CASSE
         PERFORM INVERSION
         PERFORM DEPLACEMENT-MOT
         PERFORM AFFICHAGE-VERIF
         STOP RUN
         .
       
       SAISIE-MOT.
       
         DISPLAY 'Saisir un mot (max 15 caractères) : '.
         ACCEPT WS-CHAINE
         .
       
       GESTION-CASSE.
       
         MOVE FUNCTION LOWER-CASE(WS-CHAINE) TO WS-CHAINE-SANITIZED
         .
       
       INVERSION.
       
         PERFORM VARYING CPT FROM 1 BY 1 UNTIL CPT>=15
           COMPUTE POSLET = (16 - CPT)
           MOVE WS-CHAINE-SANITIZED(CPT:1) TO
            WS-CHAINE-INVERSEE(POSLET:1)
         END-PERFORM
         .
       
       DEPLACEMENT-MOT.
          
         MOVE FUNCTION TRIM(WS-CHAINE-INVERSEE LEADING)
         TO WS-CHAINE-DEPLACEE
         .
       
       AFFICHAGE-VERIF.
       
         DISPLAY '    Chaîne d''origine : ' WS-CHAINE.
         DISPLAY 'Chaîne en minuscules : ' WS-CHAINE-SANITIZED.
         DISPLAY '     Chaîne inversée : ' WS-CHAINE-INVERSEE.
         DISPLAY '          Mot décalé : ' WS-CHAINE-DEPLACEE.
                  
       
         IF WS-CHAINE-SANITIZED = WS-CHAINE-DEPLACEE
           DISPLAY 'La chaîne est un palindrome'
         ELSE
           DISPLAY 'nix palindrome'
         END-IF
         .
       
       STOP RUN.
       