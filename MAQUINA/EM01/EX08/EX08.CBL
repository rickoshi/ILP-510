       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX08.
       AUTHOR.         RICKOSHI.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   06/10/2025.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.
      *    LE TODOS OS REGISTROS DE CADFUN, CALCULA PARA TODOS OS
      *    REGISTROS DE ENTRADA, O REAJUSTE CONFORME TABELA ABAIXO,
      *    DE ACORDO COM A FAIXA SALARIAL E GRAVA OS REGISTROS DE 
      *    SAÍDA EM CADSAI, CONFORME LAYOUT.
      *
      *    SALARIO BRUTO (VALOR EM R$)      PERCENTUAL REAJUSTE
      *           ATÉ 1.000,00                      12
      *     DE 1.000,01 ATÉ 2.000,00                11
      *         ACIMA DE 2.000,00                   10

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
           SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFUN ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADFUN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFUN.DAT".
       01 REG-ENT.
           02 COD-ENT        PIC 9(05).
           02 NOME-ENT       PIC X(20).
           02 SALBRUT-ENT    PIC 9(05)V99.

       FD CADSAI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".
       01 REG-SAI.
           02 COD-SAI        PIC 9(05).
           02 NOME-SAI       PIC X(20).
           02 SALREAJ-SAI    PIC 9(05)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ        PIC X(03) VALUE "NAO".
       77 WS-SALREAJ     PIC 9(05)V99.

       PROCEDURE DIVISION.
       EM01-EX08.
           PERFORM INICIO.
           PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADFUN
               OUTPUT CADSAI.
           PERFORM LEITURA.

       LEITURA.
           READ CADFUN
               AT END
                   MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM CALCULA-SALARIO.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       CALCULA-SALARIO.
           IF SALBRUT-ENT <= 1000,00
               COMPUTE WS-SALREAJ = SALBRUT-ENT * 1,12
           ELSE
               IF SALBRUT-ENT <= 2000,00
                   COMPUTE WS-SALREAJ = SALBRUT-ENT * 1,11
               ELSE
                   COMPUTE WS-SALREAJ = SALBRUT-ENT * 1,10.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE WS-SALREAJ TO SALREAJ-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADFUN
                 CADSAI.
