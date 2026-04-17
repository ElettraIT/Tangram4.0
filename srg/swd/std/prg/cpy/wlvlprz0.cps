      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *                                                           *
      *    * Input  : w-lvl-prz-prz = Prezzo da sottoporre a legame    *
      *    *                                                           *
      *    *          w-lvl-prz-vlt = Valuta per il legame             *
      *    *                                                           *
      *    *          w-lvl-prz-tdc = Tipo di cambio valuta            *
      *    *                                                           *
      *    *          w-lvl-prz-cdc = Coefficiente di cambio attuale   *
      *    *                                                           *
      *    *          w-lvl-prz-ccr = Coefficiente di cambio di rife-  *
      *    *                          rimento                          *
      *    *                                                           *
      *    *          w-lvl-prz-plm = Percentuale di limitazione       *
      *    *                                                           *
      *    *          w-lvl-prz-tlm = Tipo di limitazione              *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-lvl-prz-prz = Prezzo determinato               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       lvl-prz-det-000.
      *              *-------------------------------------------------*
      *              * Controllo esistenza parametri                   *
      *              *-------------------------------------------------*
           if        w-lvl-prz-vlt        =    spaces or
                     w-lvl-prz-prz        =    zero   or
                     w-lvl-prz-ccr        =    zero   or
                     w-lvl-prz-cdc        =    zero
                     go to lvl-prz-det-999.
           if        w-lvl-prz-plm        =    zero and
                     w-lvl-prz-tlm        not  = spaces
                     go to lvl-prz-det-999.
           if       (w-lvl-prz-plm        not  = zero  ) and
                    (w-lvl-prz-tlm        not  = "T" and
                     w-lvl-prz-tlm        not  = "F" and
                     w-lvl-prz-tlm        not  = "M"   )
                     go to lvl-prz-det-999.
       lvl-prz-det-050.
      *              *-------------------------------------------------*
      *              * Determinazione valore scostamento rispetto al   *
      *              * cambio di riferimento                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che esista o meno la   *
      *                  * percentuale di tolleranza                   *
      *                  *---------------------------------------------*
           if        w-lvl-prz-plm        =    zero
                     go to lvl-prz-det-052
           else      go to lvl-prz-det-054.
       lvl-prz-det-052.
      *                  *---------------------------------------------*
      *                  * Se percentuale di tolleranza a zero         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A calcolo nuovo prezzo con tipo di li-  *
      *                      * mitazione : Tolleranza                  *
      *                      *-----------------------------------------*
           go to     lvl-prz-det-200.
       lvl-prz-det-054.
      *                  *---------------------------------------------*
      *                  * Se percentuale di tolleranza esistente      *
      *                  *---------------------------------------------*
           multiply  w-lvl-prz-plm        by   w-lvl-prz-ccr
                                        giving w-lvl-prz-wps          .
           divide    100                  into w-lvl-prz-wps          .
           go to     lvl-prz-det-060.
       lvl-prz-det-060.
      *              *-------------------------------------------------*
      *              * Test se esiste uno scostamento in piu'          *
      *              *-------------------------------------------------*
           add       w-lvl-prz-wps        to   w-lvl-prz-ccr
                                        giving w-lvl-prz-wpc          .
           if        w-lvl-prz-cdc        >    w-lvl-prz-wpc
                     go to lvl-prz-det-100.
      *              *-------------------------------------------------*
      *              * Test se esiste uno scostamento in meno          *
      *              *-------------------------------------------------*
           subtract  w-lvl-prz-wps        from w-lvl-prz-ccr
                                        giving w-lvl-prz-wpc          .
           if        w-lvl-prz-cdc        <    w-lvl-prz-wpc
                     go to lvl-prz-det-100.
      *              *-------------------------------------------------*
      *              * Uscita con prezzo invariato                     *
      *              *-------------------------------------------------*
           go to     lvl-prz-det-999.
       lvl-prz-det-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo limitazione     *
      *              *-------------------------------------------------*
           if        w-lvl-prz-tlm        =    "T"
                     go to lvl-prz-det-200
           else if   w-lvl-prz-tlm        =    "F"
                     go to lvl-prz-det-400
           else if   w-lvl-prz-tlm        =    "M"
                     go to lvl-prz-det-600.
       lvl-prz-det-200.
      *              *=================================================*
      *              * Tipo limitazione : Tolleranza                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di cambio   *
      *                  *---------------------------------------------*
           if        w-lvl-prz-tdc        =    "/"
                     go to lvl-prz-det-220
           else      go to lvl-prz-det-240.
       lvl-prz-det-220.
      *                  *---------------------------------------------*
      *                  * Se divisore                                 *
      *                  *---------------------------------------------*
           multiply  w-lvl-prz-prz        by   w-lvl-prz-cdc
                                        giving w-lvl-prz-wpz          .
           divide    w-lvl-prz-ccr        into w-lvl-prz-wpz
                                               rounded                .
           move      w-lvl-prz-wpz        to   w-lvl-prz-prz          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     lvl-prz-det-999.
       lvl-prz-det-240.
      *                  *---------------------------------------------*
      *                  * Se moltiplicatore                           *
      *                  *---------------------------------------------*
           multiply  w-lvl-prz-prz        by   w-lvl-prz-ccr
                                        giving w-lvl-prz-wpz          .
           divide    w-lvl-prz-cdc        into w-lvl-prz-wpz
                                               rounded                .
           move      w-lvl-prz-wpz        to   w-lvl-prz-prz          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     lvl-prz-det-999.
       lvl-prz-det-400.
      *              *=================================================*
      *              * Tipo limitazione : Franchigia                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di cambio   *
      *                  *---------------------------------------------*
           if        w-lvl-prz-tdc        =    "/"
                     go to lvl-prz-det-420
           else      go to lvl-prz-det-440.
       lvl-prz-det-420.
      *                  *---------------------------------------------*
      *                  * Se divisore                                 *
      *                  *---------------------------------------------*
           multiply  w-lvl-prz-prz        by   w-lvl-prz-cdc
                                        giving w-lvl-prz-wpz          .
           divide    w-lvl-prz-wpc        into w-lvl-prz-wpz
                                               rounded                .
           move      w-lvl-prz-wpz        to   w-lvl-prz-prz          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     lvl-prz-det-999.
       lvl-prz-det-440.
      *                  *---------------------------------------------*
      *                  * Se moltiplicatore                           *
      *                  *---------------------------------------------*
           multiply  w-lvl-prz-prz        by   w-lvl-prz-wpc
                                        giving w-lvl-prz-wpz          .
           divide    w-lvl-prz-cdc        into w-lvl-prz-wpz
                                               rounded                .
           move      w-lvl-prz-wpz        to   w-lvl-prz-prz          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     lvl-prz-det-999.
       lvl-prz-det-600.
      *              *=================================================*
      *              * Tipo limitazione : Tetto massimo                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di cambio   *
      *                  *---------------------------------------------*
           if        w-lvl-prz-tdc        =    "/"
                     go to lvl-prz-det-620
           else      go to lvl-prz-det-640.
       lvl-prz-det-620.
      *                  *---------------------------------------------*
      *                  * Se divisore                                 *
      *                  *---------------------------------------------*
           multiply  w-lvl-prz-prz        by   w-lvl-prz-wpc
                                        giving w-lvl-prz-wpz          .
           divide    w-lvl-prz-ccr        into w-lvl-prz-wpz
                                               rounded                .
           move      w-lvl-prz-wpz        to   w-lvl-prz-prz          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     lvl-prz-det-999.
       lvl-prz-det-640.
      *                  *---------------------------------------------*
      *                  * Se moltiplicatore                           *
      *                  *---------------------------------------------*
           multiply  w-lvl-prz-prz        by   w-lvl-prz-ccr
                                        giving w-lvl-prz-wpz          .
           divide    w-lvl-prz-wpc        into w-lvl-prz-wpz
                                               rounded                .
           move      w-lvl-prz-wpz        to   w-lvl-prz-prz          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     lvl-prz-det-999.
       lvl-prz-det-999.
           exit.

