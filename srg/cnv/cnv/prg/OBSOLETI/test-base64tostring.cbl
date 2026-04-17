       identification division.
       program-id.  test-base64-decode.

       data division.
       working-storage section.
       
       01  base64-message      pic x(48) value
           "Tm93IGlzIHRoZSB0aW1lIHRvIGRlY29kZSBhIG1lc3NhZ2U=".       

       01  clear-message       pic x(36).  

       procedure division.
       a.
           call "Base64toString" using base64-message 
                                 giving clear-message.
           display clear-message.
           
       b.
           exit. 
