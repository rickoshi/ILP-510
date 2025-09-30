       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DECISAO.
       AUTHOR.         RICKOSHI.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   30/09/2025.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.        LE OS REGISTROS DO ARQUIVO DE
      *                ENTRADA CAD-ENT E GRAVA NO
      *                ARQUIVO DE SAIDA CAD-SAI APENAS
      *                OS REGISTROS DO SEXO FEMININO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
           SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAD-ENT ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CAD-SAI ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CAD-ENT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CAD-ENT.DAT".
       01 REG-ENT.
           02 COD-ENT    PIC 9(04).
           02 NOME-ENT   PIC X(30).
           02 SEXO-ENT   PIC X(01).

       FD CAD-SAI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CAD-SAI.DAT".
       01 REG-SAI.
           02 COD-SAI    PIC 9(04).
           02 NOME-SAI   PIC X(30).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
       PGM-DECISAO.
           PERFORM INICIO.
           PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CAD-ENT
               OUTPUT CAD-SAI.
           PERFORM LEITURA.

       LEITURA.
           READ CAD-ENT
               AT END
                   MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM SELECAO.
           PERFORM LEITURA.

       SELECAO.
           IF SEXO-ENT = "F" OR SEXO-ENT EQUAL "f"
               PERFORM GRAVACAO.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CAD-ENT
                 CAD-SAI.
