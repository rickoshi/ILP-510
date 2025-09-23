       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX00.
       AUTHOR.         RICKOSHI.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   23/09/2025.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.        LE OS REGISTROS DO ARQUIVO DE ENTRADA
      *                CAD-ENT(CODIGO, NOME E SEXO) E GRAVA NO
      *                ARQUIVO DE SAIDA CAD-SAI(CODIGO E NOME).

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
           02 SEXO-ENT   PIC A(01).

       FD CAD-SAI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CAD-SAI.DAT".
       01 REG-SAI.
           02 COD-SAI    PIC 9(04).
           02 NOME-SAI   PIC X(30).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
       EXEMPLO.
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
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CAD-ENT
                 CAD-SAI.

      *FIM DO PROGRAMA
