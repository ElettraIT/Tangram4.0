       >>imp margin-r end
       identification division.
       program-id.  Base64MessageDigest.
      *
      * CALL "StringToBase64" USING Message GIVING Base64-Message
      *
      * where:
      *        Message is the message to be converted to base64
      *        Base64-Messgae is an area into which the
      *                message, encoded in Base64, will be stored
      *
      * Programming note:  StringToBase64 does not initialize the 
      *                    output message.  It is left to the calling
      *                    program to initialize the output message
      *                    if desired.  Conveniently, the space character
      *                    is not in the output alphabet.
      *
      * This program uses Base64 MIME encoding as specified in 
      * RFC 2045 (http://www.ietf.org/rfc/rfc2045.txt)
       
       data division.
       working-storage section.

       01  binary(2).
           02  i                       pic 9(4).
           02  j                       pic 9(4).
           02  k                       pic 9(4).

       01  message-length              pic 9(6) binary.
       01  base64-message-length       pic 9(6) binary.
       01  triplet-count               pic 9(6) binary.
       01  trailing-count              pic 9(6) binary.



       01  Base64-Alphabet             pic x(64) value
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                   "abcdefghijklmnopqrstuvwxyz" &
                   "0123456789" &
                   "+/".

       01  b64-triplet.
           05  b64-4octets.
               07                      pic x.
               07  b64-3octets.
                   09                  pic x.
                   09  b64-2octets.
                       11              pic x.
                       11  b64-1octet  pic x.
       01  redefines b64-triplet.
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

       01  Base64-Message.
           02  pic x occurs 1 to 10000 
                     depending on base64-message-length.


       procedure division using the-message giving Base64-Message.
       a.
           CALL "C$CARG" USING C-CARG-SUCCESS,
                               the-message
                               Argument-Description.
           if not is-c-carg-success
               move all "=" to Base64-Message
               exit program
           end-if.
           move Argument-length to message-length.
           divide message-length by 3
               giving triplet-count
               remainder trailing-count.
           multiply triplet-count by 4 giving base64-message-length.
           if trailing-count not = 0 add 4 to base64-message-length.

           perform varying i from 1 by 1 
                     until i > triplet-count
               move all x"00" to b64-4octets
               move the-message ((i * 3) - 2:3) to  b64-3octets
               call "C$LogicalShiftLeft"  using b64-4octets, 6
               call "C$LogicalShiftRight" using b64-3octets, 2
               call "C$LogicalShiftRight" using b64-2octets, 2
               call "C$LogicalShiftRight" using b64-1octet,  2
               perform varying j from 1 by 1
                         until j > count of b64-octet
                   add b64-octet (j), 1 giving k
                   move Base64-Alphabet (k:1) 
                     to b64-octet (j) (1:1)     
               end-perform
               move b64-4octets to Base64-Message ((i * 4) - 3:4)
           end-perform.
           move all x"00" to b64-4octets
           evaluate trailing-count
           when 2
               move the-message ((triplet-count * 3) + 1:2) to  b64-3octets(1:2)
           when 1
               move the-message ((triplet-count * 3) + 1:1) to  b64-3octets(1:1)
           when other
               continue
           end-evaluate
           call "C$LogicalShiftLeft"  using b64-4octets, 6
           call "C$LogicalShiftRight" using b64-3octets, 2
           call "C$LogicalShiftRight" using b64-2octets, 2
           call "C$LogicalShiftRight" using b64-1octet,  2
           perform varying j from 1 by 1
                     until j = count of b64-octet
               add b64-octet (j), 1 giving k
               move Base64-Alphabet (k:1) 
                 to b64-octet (j) (1:1)     
           end-perform.
           evaluate trailing-count
           when 1
               move "==" to  b64-2octets
           when 2
               move "=" to  b64-1octet
           when other
               continue
           end-evaluate
           move b64-4octets to Base64-Message ((triplet-count * 4) + 1:4)
           exit program.
            
