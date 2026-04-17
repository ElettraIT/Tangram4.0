       Identification Division.
       Program-Id.                                 Base64toString      .
      *================================================================*
      *                                                                *
      * CALL "Base64toString" USING Base64String GIVING OctetString    *
      *
      * where:
      *        Base64String is the Base64 encoded message 
      *        OctetString is a area into which the decoded message 
      *                will be stored
      *
      * This program uses Base64 MIME
      * encoding as specified in RFC 2045                              *
      *                                                                *
      *================================================================*

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

       01  binary (2).
           02  i                       pic 9(4).
           02  j                       pic 9(4).
           02  k                       pic 9(4).

       01  message-length              pic 9(6) binary.
       01  output-length               pic 9(6) binary.
       01  required-output-length      pic 9(6) binary.

       01  base64-quartet.
           03  base64-char             pic x occurs 4.

       01  constructed-octets.
           03  base64-translated       pic 9(3) binary(1) occurs 4.
       01  redefines constructed-octets.
           03  octets-4.
               05                      pic x.
               05  octets-3.
                   07                  pic x.
                   07  octets-2.
                       09              pic x.
                       09  octets-1    pic x.

       01  Base64-Alphabet             pic x(64) value
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                   "abcdefghijklmnopqrstuvwxyz" &
                   "0123456789" &
                   "+/".

       01  ARGUMENT-DESCRIPTION        BINARY(2).
           02  ARGUMENT-TYPE           PIC 9(2).
           02  ARGUMENT-LENGTH         PIC 9(8) BINARY(4).
           02  ARGUMENT-DIGIT-COUNT    PIC 9(2).
           02  ARGUMENT-SCALE          PIC S9(2).

       01  C-CARG-SUCCESS              PIC X.
           88  IS-C-CARG-SUCCESS       VALUE "Y".
       
       linkage section.
       01  Base64String.
           02  pic x occurs 1 to 10000 
                     depending on message-length.

       01  OctetString.
           02  pic x occurs 1 to 10000 
                     depending on output-length.



       procedure division using Base64String giving OctetString.
       a.
           CALL "C$CARG" USING C-CARG-SUCCESS,
                               Base64String
                               Argument-Description.
           if not is-c-carg-success
               Display "C$CARG failure on Base64String"
               Stop Run
           end-if.
           move Argument-length to message-length.
           divide message-length by 4 
                   giving i
                   remainder j
           if j not = 0
               Display "Base64 message length not multiple of 4"
               Stop Run
           end-if.

           CALL "C$CARG" USING C-CARG-SUCCESS,
                               OctetString
                               Argument-Description.
           if not is-c-carg-success
               Display "C$CARG failure on OctetString"
               Stop Run
           end-if.
           move Argument-length to output-length.
           multiply i by 3 giving required-output-length.
           if output-length < required-output-length
               Display "Base64 message length too large for output"
               Stop Run
           end-if.


           move all X"00" to OctetString
           move 1 to k  *> output cursor
           perform varying i from 1 by 4 
                     until i > message-length
               move Base64String (i:4) to base64-quartet
               perform varying j from 1 by 1
                         until j > count of base64-char
                   move 0 to base64-translated (j)
                   inspect base64-alphabet
                       tallying base64-translated (j)
                       for characters before initial base64-char (j)
                   if  base64-translated (j) >=
                       length of base64-alphabet
                     move 0 to base64-translated (j)
                   end-if
               end-perform
               call "C$LogicalShiftLeft" using octets-1, 2
               call "C$LogicalShiftLeft" using octets-2, 2
               call "C$LogicalShiftLeft" using octets-3, 2
               call "C$LogicalShiftLeft" using octets-4, 2
               move constructed-octets to OctetString (k:3) 
               add 3 to k
           end-perform.
           exit program.
