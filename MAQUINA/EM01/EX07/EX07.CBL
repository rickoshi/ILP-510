       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX07.
       AUTHOR.         RICKOSHI.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   06/10/2025.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.        LE TODOS OS REGISTROS DE CADALU, SELECIONA PARA
      *                GRAVAÇÃO NO ARQUIVO DE SAÍDA CADATU SOMENTE OS
      *                REGISTROS DO SEXO FEMININO (SEXO=F), CALCULA
      *                MEDIA = (NOTA1 + NOTA2 + NOTA3 + NOTA4) / 4 DOS
      *                REGISTROS SELECIONADOS E GRAVA OS REGISTROS EM
      *                CADATU CONFORME LAYOUT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
           SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".
       01 REG-ENT.
           02 NUM-ENT      PIC 9(05).
           02 NOME-ENT     PIC X(20).
           02 NOTA1-ENT    PIC 9(02)V99.
           02 NOTA2-ENT    PIC 9(02)V99.
           02 NOTA3-ENT    PIC 9(02)V99.
           02 NOTA4-ENT    PIC 9(02)V99.
           02 SEXO-ENT     PIC X(01).

       FD CADATU
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
       01 REG-SAI.
           02 NUM-SAI      PIC 9(05).
           02 NOME-SAI     PIC X(20).
           02 MEDIA-SAI    PIC 9(02)V99.
           02 SEXO-SAI     PIC X(01).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".
       77 WS-MEDIA   PIC 9(02)V99.

       PROCEDURE DIVISION.
       EM01-EX07.
           PERFORM INICIO.
           PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADALU
               OUTPUT CADATU.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
               AT END
                   MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM SELECAO.
           PERFORM LEITURA.

       SELECAO.
           IF SEXO-ENT = "F" OR SEXO-ENT = "f"
               PERFORM CALCULA-MEDIA
               PERFORM GRAVACAO.

       CALCULA-MEDIA.
           COMPUTE WS-MEDIA = (NOTA1-ENT + NOTA2-ENT +
               NOTA3-ENT + NOTA4-ENT) / 4.

       GRAVACAO.
           MOVE NUM-ENT TO NUM-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE WS-MEDIA TO MEDIA-SAI.
           MOVE SEXO-ENT TO SEXO-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.
