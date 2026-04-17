       IDENTIFICATION DIVISION.
       PROGRAM-ID. MD5Example.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       File-Control.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STRING-VALUE    PIC X(08) VALUE 'ABCDEFGH'.
       01 CMD-LINE        PIC X(100).
       01 RETURN-VALUE    PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE 'echo ' TO CMD-LINE
           STRING STRING-VALUE DELIMITED BY SPACE
                  ' | md5sum' DELIMITED BY SPACE
                  INTO CMD-LINE
           DISPLAY 'Running command: ' CMD-LINE
           CALL 'SYSTEM' USING CMD-LINE

           DISPLAY 'MD5 Hash calculated, reading file...'

           OPEN INPUT MD5-FILE
           READ MD5-FILE INTO MD5-LINE
           DISPLAY 'MD5 Hash is: ' MD5-LINE
           CLOSE MD5-FILE

           STOP RUN.







