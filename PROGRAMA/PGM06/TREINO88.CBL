       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TREINO88.
       AUTHOR.         RICKOSHI.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   13/10/2025.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.
      * LE TODOS OS REGISTROS DO ARQUIVO CADENT E
      * CRITICA MEDIANTE AS CONDICOES A SEGUIR:
      *
      * NOME – DEVERA SER DIFERENTE DE BRANCOS
      * ESTADO – DEVERA SER DIFERENTE DE BRANCO SENDO ACEITO
      * APENAS OS ESTADOS BRASILEIROS (AC, AL, AP, AM, BA, CE, DF,
      * ES, GO, MA, MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO,
      * RR, SC, SP, SE, TO)
      *
      * SE O REGISTRO FOR CONSIDERADO INVALIDO SEGUNDO AS CONDICOES
      * DE CRITICA, GRAVA NO ARQUIVO DE SAIDA CADRUIM.
      * SE O REGISTRO ESTIVER CORRETO, GRAVA NO ARQUIVO CADOK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
           SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
           SELECT CADENT ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADOK ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADRUIM ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADENT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADENT.DAT".
       01 REG-ENT.
           02 COD-ENT    PIC 9(02).
           02 NOME-ENT   PIC X(30).
               88 NOME-INVALIDO  VALUE SPACES.
           02 ESTD-ENT   PIC X(02).
               88 ESTD-VALIDO    VALUE "AC" "AL" "AP" "AM" "BA"
                                       "CE" "DF" "ES" "GO" "MA"
                                       "MT" "MS" "MG" "PA" "PB"
                                       "PR" "PE" "PI" "RJ" "RN"
                                       "RS" "RO" "RR" "SC" "SP"
                                       "SE" "TO".

       FD CADOK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADOK.DAT".
       01 REG-OK     PIC X(34).

       FD CADRUIM
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADRUIM.DAT".
       01 REG-RUIM   PIC X(34).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".
       77 CH-ERRO    PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
       PGM-TREINO88.
           PERFORM INICIO.
           PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.

       INICIO.
           OPEN INPUT CADENT
               OUTPUT CADOK
                      CADRUIM.
           PERFORM LEITURA.

       LEITURA.
           READ CADENT
               AT END
                   MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM CONSISTENCIA.
           PERFORM LEITURA.

       CONSISTENCIA.
           MOVE "NAO" TO CH-ERRO.
           PERFORM VALIDA-NOME.
           PERFORM VALIDA-ESTADO.
           IF CH-ERRO EQUAL "SIM"
               PERFORM GRAVA-RUIM
           ELSE
               PERFORM GRAVA-OK.

       VALIDA-NOME.
           IF NOME-INVALIDO
               MOVE "SIM" TO CH-ERRO
           ELSE
               NEXT SENTENCE.

       VALIDA-ESTADO.
           IF NOT ESTD-VALIDO
               MOVE "SIM" TO CH-ERRO
           ELSE
               NEXT SENTENCE.

       GRAVA-RUIM.
           WRITE REG-RUIM FROM REG-ENT.

       GRAVA-OK.
           WRITE REG-OK FROM REG-ENT.

       FIM.
           CLOSE CADENT
                 CADOK
                 CADRUIM.
