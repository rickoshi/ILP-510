       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX02.
       AUTHOR.         RICKOSHI.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   03/10/2025.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.        LE TODOS OS REGISTROS DE CADALU E GRAVA
      *                OS REGISTROS EM CADATU CONFORME LAYOUT.

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
           02 SEXO-ENT     PIC X(01).
           02 DATA-NASC-ENT.
               03 DD       PIC 9(02).
               03 MM       PIC 9(02).
               03 AAAA     PIC 9(04).

       FD CADATU
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".
       01 REG-SAI.
           02 NUM-SAI      PIC 9(05).
           02 NOME-SAI     PIC X(20).
           02 DATA-NASC-SAI.
               03 DD       PIC 9(02).
               03 MM       PIC 9(02).
               03 AAAA     PIC 9(04).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ   PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
       EM01-EX02.
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
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE NUM-ENT TO NUM-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE DATA-NASC-ENT TO DATA-NASC-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.
