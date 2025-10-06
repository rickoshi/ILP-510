       IDENTIFICATION DIVISION.
       PROGRAM-ID.     EX05.
       AUTHOR.         RICKOSHI.
       INSTALLATION.   FATEC-SP.
       DATE-WRITTEN.   05/10/2025.
       DATE-COMPILED.
       SECURITY.       APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.        LE TODOS OS REGISTROS DE CADALU, CALCULA
      *                MEDIA = (NOTA1 + NOTA2) / 2 E SELECIONA
      *                PARA GRAVAÇÃO NO ARQUIVO DE SAÍDA “CADAPR”
      *                SOMENTE OS REGISTROS QUE TIVEREM
      *                MÉDIA >= 7,0 (SETE) E FALTAS <= 18.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
           SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADAPR ASSIGN TO DISK
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
           02 FALTAS-ENT   PIC 9(02).

       FD CADAPR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADAPR.DAT".
       01 REG-SAI.
           02 NUM-SAI      PIC 9(05).
           02 NOME-SAI     PIC X(20).
           02 MEDIA-SAI    PIC 9(02)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ    PIC X(03) VALUE "NAO".
       77 WS-MEDIA   PIC 9(02)V99.

       PROCEDURE DIVISION.
       EM01-EX05.
           PERFORM INICIO.
           PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADALU
               OUTPUT CADAPR.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
               AT END
                   MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM SELECAO.
           PERFORM LEITURA.

       SELECAO.
           COMPUTE WS-MEDIA = (NOTA1-ENT + NOTA2-ENT) / 2.
           IF WS-MEDIA >= 7 AND FALTAS-ENT <= 18
               PERFORM GRAVACAO.

       GRAVACAO.
           MOVE NUM-ENT TO NUM-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE WS-MEDIA TO MEDIA-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADAPR.
