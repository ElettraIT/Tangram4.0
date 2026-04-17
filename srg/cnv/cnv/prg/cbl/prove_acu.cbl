
_____* NON FUNZIA 21/11/24


       Identification Division.
       PROGRAM-ID. HexConversion.

       Data Division.
       Working-Storage Section.

       01 input-string         pic X(3) VALUE 'ABC'.
       01 hex-string           pic X(6)            .
       01 ascii-value.
          05 filler         occurs 3 pic 9(03)  .
       01 hex-value.
          05 filler         occurs 3 pic x(02)  .
       01 index                pic 9(1) VALUE 1    .

       Procedure Division                                             .
       BEGIN.
           PERFORM Convert-To-Hex
           DISPLAY 'Hexadecimal string: ' hex-string
           STOP RUN.

       Convert-To-Hex.
           PERFORM VARYING index FROM 1 BY 1 UNTIL index > 3
               MOVE FUNCTION NUMVAL (FUNCTION NUMVAL-C (input-string(index:1))) TO ascii-value(index)
               MOVE FUNCTION NUMVAL (ascii-value(index)) TO hex-value(index)
               STRING hex-value(index) DELIMITED BY SPACE
                      INTO hex-string
               END-STRING
           END-PERFORM.
