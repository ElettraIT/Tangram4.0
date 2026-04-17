       identification division.
       program-id.  Base64MessageDigest.
      *
      * CALL "Base64MessageDigest" USING Message GIVING Base64-Digest
      *
      * where:
      *        Message is the message for which a digest is required
      *        Base64-Digest is a 28-character area into which the
      *                message digest, encoded in Base64, will be stored
      *
      * This program uses C$SecureHash to create a 20-byte SHA-1
      * message digest, which is then encoded using Base64 MIME
      * encoding as specified in RFC 2045 (http://www.ietf.org/rfc/rfc2045.txt)
       
       data division.
       working-storage section.

       01  binary(2).
           02  i                       pic 9(4).
           02  j                       pic 9(4).
           02  k                       pic 9(4).

       01  message-length              pic 9(6) binary.

       01  message-digest              pic x(20).
       01  redefines message-digest.
           03  md-triplet              pic x(3) occurs 6.
           03  md-doublet              pic x(2).


       01  Base64-Alphabet             pic x(64) value
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                   "abcdefghijklmnopqrstuvwxyz" &
                   "0123456789" &
                   "+/".

       01  constructed-base64-md.
           03  b64-triplet             occurs 7.
               05  b64-4octets.
                   07                      pic x.
                   07  b64-3octets.
                       09                  pic x.
                       09  b64-2octets.
                           11              pic x.
                           11  b64-1octet  pic x.
       01  redefines constructed-base64-md.
           03                          occurs 7.
               05  b64-octet           pic 999 binary(1) occurs 4.

       01  ARGUMENT-DESCRIPTION        BINARY(2).
           02  ARGUMENT-TYPE           PIC 9(2).
           02  ARGUMENT-LENGTH         PIC 9(8) BINARY(4).
           02  ARGUMENT-DIGIT-COUNT    PIC 9(2).
           02  ARGUMENT-SCALE          PIC S9(2).

       01  C-CARG-SUCCESS              PIC X.
           88  IS-C-CARG-SUCCESS       VALUE "Y".
       
       linkage section.
       01  the-message.
           02  pic x occurs 1 to 10000 
                     depending on message-length.

       01  Base64-Digest               pic x(28).


       procedure division using the-message giving Base64-Digest.
       a.
           CALL "C$CARG" USING C-CARG-SUCCESS,
                               the-message
                               Argument-Description.
           if not is-c-carg-success
               move all "=" to Base64-Digest
               exit program
           end-if.
           move Argument-length to message-length.

           CALL "C$SecureHash" USING the-message, message-length
                               GIVING message-digest.

           move all X"00" to constructed-base64-md
           perform varying i from 1 by 1 
                     until i > count of md-triplet
               move md-triplet (i) to b64-3octets (i)
               call "C$LogicalShiftLeft"  using b64-4octets (i), 6
               call "C$LogicalShiftRight" using b64-3octets (i), 2
               call "C$LogicalShiftRight" using b64-2octets (i), 2
               call "C$LogicalShiftRight" using b64-1octet  (i), 2
               perform varying j from 1 by 1
                         until j > count of b64-octet
                   add b64-octet (i, j), 1 giving k
                   move Base64-Alphabet (k:1) 
                     to b64-octet (i, j) (1:1)     
               end-perform
           end-perform.
           move md-doublet to b64-3octets (7) (1:2) 
           call "C$LogicalShiftLeft"  using b64-4octets (7), 6
           call "C$LogicalShiftRight" using b64-3octets (7), 2
           call "C$LogicalShiftRight" using b64-2octets (7), 2
           call "C$LogicalShiftRight" using b64-1octet  (7), 2
           perform varying j from 1 by 1
                     until j = count of b64-octet
               add b64-octet (7, j), 1 giving k
               move Base64-Alphabet (k:1) 
                 to b64-octet (7, j) (1:1)     
           end-perform.
           move "=" to b64-octet (7, 4) (1:1).
           move constructed-base64-md to Base64-Digest.
           exit program.
            
