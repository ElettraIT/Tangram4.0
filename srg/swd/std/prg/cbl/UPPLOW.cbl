
           ________ OBSOLETO: vedi all-str-alf-upp ..... _____




      *    *===========================================================*
      *    * Work-area per conversioni in uppercase/lowercase          *
      *    *-----------------------------------------------------------*
       01  w-upp-low.
      *        *-------------------------------------------------------*
      *        * Stringa da convertire e convertita                    *
      *        *-------------------------------------------------------*
           05  w-upp-low-str.
               10  w-upp-low-chr
                               occurs 80  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Caratteri in uppercase                                *
      *        *-------------------------------------------------------*
           05  w-upp-low-upp.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-low-upr redefines
               w-upp-low-upp.
               10  w-upp-low-upc
                               occurs 26  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Caratteri in lowercase                                *
      *        *-------------------------------------------------------*
           05  w-upp-low-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-upp-low-lor redefines
               w-upp-low-low.
               10  w-upp-low-loc
                               occurs 26  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore dei trailing spaces                         *
      *        *-------------------------------------------------------*
           05  w-upp-low-cts              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Lunghezza della stringa da convertire                 *
      *        *-------------------------------------------------------*
           05  w-upp-low-lun              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Indice su carattere da convertire                     *
      *        *-------------------------------------------------------*
           05  w-upp-low-inx              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Indice su tabella caratteri                           *
      *        *-------------------------------------------------------*
           05  w-upp-low-itc              pic  9(02)                  .





      *    *===========================================================*
      *    * Routine per la conversione da lowercase a uppercase       *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-upp-low-str = Valore della stringa da conver-  *
      *    *                          tire, max 80 caratteri           *
      *    *                                                           *
      *    * Output : w-upp-low-str = Valore della stringa in upperca- *
      *    *                          se                               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cvs-low-upp-000.
      *              *-------------------------------------------------*
      *              * Conteggio dei trailing spaces                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-upp-low-cts          .
           inspect   w-upp-low-str    tallying w-upp-low-cts
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Calcolo numero caratteri da convertire          *
      *              *-------------------------------------------------*
           move      80                   to   w-upp-low-lun          .
           subtract  w-upp-low-cts        from w-upp-low-lun          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice su carattere da conver- *
      *              * tire                                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-upp-low-inx          .
       cvs-low-upp-100.
      *              *-------------------------------------------------*
      *              * Incremento indice su carattere da convertire    *
      *              *-------------------------------------------------*
           add       1                    to   w-upp-low-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : ad uscita                 *
      *              *-------------------------------------------------*
           if        w-upp-low-inx        >    w-upp-low-lun
                     go to cvs-low-upp-999.
      *              *-------------------------------------------------*
      *              * Conversione da lowercase a uppercase            *
      *              *-------------------------------------------------*
           move      zero                 to   w-upp-low-itc          .
           inspect   w-upp-low-low    tallying w-upp-low-itc
                     for characters     before
                                       initial w-upp-low-chr
                                              (w-upp-low-inx)         .
           if        w-upp-low-itc        <    26
                     add   1              to   w-upp-low-itc
                     move  w-upp-low-upc
                          (w-upp-low-itc) to   w-upp-low-chr
                                              (w-upp-low-inx)         .
      *              *-------------------------------------------------*
      *              * Riciclo su carattere successivo                 *
      *              *-------------------------------------------------*
           go to     cvs-low-upp-100.
       cvs-low-upp-999.
           exit.

      *    *===========================================================*
      *    * Routine per la conversione da uppercase a lowercase       *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-upp-low-str = Valore della stringa da conver-  *
      *    *                          tire, max 80 caratteri           *
      *    *                                                           *
      *    * Output : w-upp-low-str = Valore della stringa in lowerca- *
      *    *                          se                               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cvs-upp-low-000.
      *              *-------------------------------------------------*
      *              * Conteggio dei trailing spaces                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-upp-low-cts          .
           inspect   w-upp-low-str    tallying w-upp-low-cts
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Calcolo numero caratteri da convertire          *
      *              *-------------------------------------------------*
           move      80                   to   w-upp-low-lun          .
           subtract  w-upp-low-cts        from w-upp-low-lun          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice su carattere da conver- *
      *              * tire                                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-upp-low-inx          .
       cvs-upp-low-100.
      *              *-------------------------------------------------*
      *              * Incremento indice su carattere da convertire    *
      *              *-------------------------------------------------*
           add       1                    to   w-upp-low-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : ad uscita                 *
      *              *-------------------------------------------------*
           if        w-upp-low-inx        >    w-upp-low-lun
                     go to cvs-upp-low-999.
      *              *-------------------------------------------------*
      *              * Conversione da uppercase a lowercase            *
      *              *-------------------------------------------------*
           move      zero                 to   w-upp-low-itc          .
           inspect   w-upp-low-upp    tallying w-upp-low-itc
                     for characters     before
                                       initial w-upp-low-chr
                                              (w-upp-low-inx)         .
           if        w-upp-low-itc        <    26
                     add   1              to   w-upp-low-itc
                     move  w-upp-low-loc
                          (w-upp-low-itc) to   w-upp-low-chr
                                              (w-upp-low-inx)         .
      *              *-------------------------------------------------*
      *              * Riciclo su carattere successivo                 *
      *              *-------------------------------------------------*
           go to     cvs-upp-low-100.
       cvs-upp-low-999.
           exit.

      *    *===========================================================*
      *    * Routine per la trasformazione in capitalize               *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-upp-low-str = Valore della stringa da conver-  *
      *    *                          tire, max 80 caratteri           *
      *    *                                                           *
      *    * Output : w-upp-low-str = Valore della stringa in lowerca- *
      *    *                          se                               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cvs-str-cpt-000.
           move      1                    to   w-upp-low-cts          .
       cvs-str-cpt-100.
           add       1                    to   w-upp-low-cts          .
           if        w-upp-low-cts        >    30
                     go to  cvs-str-cpt-999.
      *
           if        w-upp-low-str
                    (w-upp-low-cts)       =    spaces or
                     w-upp-low-str
                    (w-upp-low-cts)       =    "'"
                     add  1               to   w-upp-low-cts
                     go to cvs-str-cpt-100.
      *
           move      zero                 to   w-upp-low-inx          .
           inspect   w-upp-car        tallying w-upp-low-inx
                     for characters     before initial w-upp-low-str
                                                      (w-upp-low-cts) .
           if        w-upp-low-inx        <    26
                     add     1            to   w-upp-low-inx
                     move    w-upp-low-loc
                            (w-upp-low-inx)
                                          to   w-upp-low-str
                                              (w-upp-low-cts)   .
           go to     cvs-str-cpt-100.
       cvs-str-cpt-999.
           exit.

