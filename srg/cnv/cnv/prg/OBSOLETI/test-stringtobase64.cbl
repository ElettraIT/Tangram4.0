       >>imp margin-r end
       identification division.
       program-id.  test-stringtobase64.

       data division.
       working-storage section.
       
       01  base64-message      pic x(400).
       
       01  i pic 9(6) binary.       

       01  clear-message       pic x(36).  

       procedure division.
       a.
           move spaces to base64-message.
           call "StringToBase64" using  "Aladdin:open sesame"
                                 giving base64-message.
           move 0 to i.
           inspect base64-message 
               tallying i for characters before initial space
           display base64-message (1:i)
           call "Base64toString" using base64-message (1:i) 
                                 giving clear-message.
           display clear-message.
           
       b.
           exit.
