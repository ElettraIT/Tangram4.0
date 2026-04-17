      *    *===========================================================*
      *    * Routine di determinazione del primo giorno non festivo    *
      *    * rispetto ad una data. Si presume che sia stato prece-     *
      *    * dentemente determinato il giorno della settimana della    *
      *    * data in input                                             *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * In input  : w-det-dow-fst-dat = data completa             *
      *    *             w-det-dow-fst-num = giorno della settimana    *
      *    *                                                           *
      *    * In output : w-det-dow-fst-dnf = Data non festiva          *
      *    *                                                           *
      *    *       N.B.: Vengono considerati festivi i giorni :        *
      *    *                                                           *
      *    *             - sabato       (6)  (w-det-dow-fst-1gf)       *
      *    *             - domenica     (7)  (w-det-dow-fst-2gf)       *
      *    *                                                           *
      *    *             salvo diversa indicazione.                    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-dow-fst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-dow-fst-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dow-fst-dnf      .
      *              *-------------------------------------------------*
      *              * Se la data in input e' a zero : uscita          *
      *              *-------------------------------------------------*
           if        w-det-dow-fst-dat    =    zero
                     go to det-dow-fst-999.
      *              *-------------------------------------------------*
      *              * Se non si conosce il numero di giorno nell'am-  *
      *              * bito della settimana : uscita                   *
      *              *-------------------------------------------------*
           if        w-det-dow-fst-num    =    zero
                     go to det-dow-fst-999.
      *              *-------------------------------------------------*
      *              * Se il numero di giorno nell'ambito della setti- *
      *              * mana e' maggiore di sette : uscita              *
      *              *-------------------------------------------------*
           if        w-det-dow-fst-num    >    7
                     go to det-dow-fst-999.
       det-dow-fst-100.
      *              *-------------------------------------------------*
      *              * Data di entrata in data di uscita               *
      *              *-------------------------------------------------*
           move      w-det-dow-fst-dat    to   w-det-dow-fst-dnf      .
      *              *-------------------------------------------------*
      *              * Controllo data                                  *
      *              *-------------------------------------------------*
           move      "CD"                 to   s-ope                  .
           move      w-det-dow-fst-dnf    to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se data scorretta : uscita con errore       *
      *                  *---------------------------------------------*
           if        s-sts                not  = spaces
                     go to  det-dow-fst-900.
       det-dow-fst-200.
      *              *-------------------------------------------------*
      *              * Se non si tratta di giorno festivo : uscita con *
      *              * la stessa data in input                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto con il giorno festivo 1           *
      *                  *---------------------------------------------*
           if        w-det-dow-fst-num    =    w-det-dow-fst-1gf
                     go to det-dow-fst-300.
      *                  *---------------------------------------------*
      *                  * Confronto con il giorno festivo 2           *
      *                  *---------------------------------------------*
           if        w-det-dow-fst-num    =    w-det-dow-fst-2gf
                     go to det-dow-fst-300.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dow-fst-999.
       det-dow-fst-300.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento del giorno in funzione del tipo  *
      *                  * giorno                                      *
      *                  *                                             *
      *                  * N.B. : nel caso del sabato si comprende     *
      *                  *        anche la domenica, quindi lo sposta- *
      *                  *        mento in avanti e' di 2 giorni       *
      *                  *---------------------------------------------*
           if        w-det-dow-fst-num    =    6
                     add   2              to   s-gio
           else      add   1              to   s-gio                  .
      *                  *---------------------------------------------*
      *                  * Controllo della data                        *
      *                  *---------------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se la data e' corretta : nuovo valore       *
      *                  *---------------------------------------------*
           if        s-sts                =    spaces
                     go to det-dow-fst-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione del giorno ed incremento    *
      *                  * del mese                                    *
      *                  *---------------------------------------------*
           move      1                    to   s-gio                  .
           add       1                    to   s-mes                  .
      *                  *---------------------------------------------*
      *                  * Controllo mese                              *
      *                  *---------------------------------------------*
           if        s-mes                not  > 12
                     go to det-dow-fst-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione del mese ed incremento      *
      *                  * dell'anno                                   *
      *                  *---------------------------------------------*
           move      1                    to   s-mes                  .
           if        s-ann                <    99
                     add   1              to   s-ann
                     go to det-dow-fst-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione dell'anno ed incremento del *
      *                  * secolo                                      *
      *                  *---------------------------------------------*
           move      zero                 to   s-ann                  .
           add       1                    to   s-sec                  .
       det-dow-fst-400.
           move      s-dat                to   w-det-dow-fst-dnf      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dow-fst-800.
       det-dow-fst-600.
      *              *-------------------------------------------------*
      *              * Controllo che la data ottenuta non esista nella *
      *              * tabella delle date festive : da implementare    *
      *              *-------------------------------------------------*
       det-dow-fst-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-dow-fst-999.
       det-dow-fst-900.
      *              *-------------------------------------------------*
      *              * Uscita per data non determinabile               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-dow-fst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dow-fst-999.
       det-dow-fst-999.
           exit.

