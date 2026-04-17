      *    *===========================================================*
      *    * Emissione tag 'xml'                                       *
      *    *                                                           *
      *    * N.B.: normalizzare sempre 'w-sta-emi-xml-inx'             *
      *    *       all'inizio dell'utilizzo di questi moduli           *
      *    *                                                           *
      *    *       *** IMPORTANTE ***                                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       emi-tag-xml-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare prompt              *
      *              *-------------------------------------------------*
           move      w-sta-emi-xml-pmt    to   w-sta-emi-xml-pma      .
           move      w-sta-emi-xml-pmt    to   w-sta-emi-xml-pmc      .
       emi-tag-xml-010.
      *              *-------------------------------------------------*
      *              * Filtraggio preliminare valore per eliminare     *
      *              * eventuali caratteri incompatibili con XML       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sostituzione caratteri con sequenze HTML    *
      *                  *                                             *
      *                  * '&' = '&amp;'                               *
      *                  * '<' = '&lt;'                                *
      *                  * '>' = '&gt;'                                *
      *                  * '"' = '&quot;'                              *
      *                  *                                             *
      *                  * N.B.: DA IMPLEMENTARE ___                   *
      *                  *---------------------------------------------*
           inspect   w-sta-emi-xml-val
                                replacing all  "&"
                                          by   "E"                    .
      *                  *---------------------------------------------*
      *                  * Carattere '<'                               *
      *                  *---------------------------------------------*
           inspect   w-sta-emi-xml-val
                                replacing all  "<"
                                          by   "("                    .
      *                  *---------------------------------------------*
      *                  * Carattere '>'                               *
      *                  *---------------------------------------------*
           inspect   w-sta-emi-xml-val
                                replacing all  ">"
                                          by   ")"                    .
       emi-tag-xml-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-sta-emi-xml-ope    =    "A"
                     go to emi-tag-xml-100
           else if   w-sta-emi-xml-ope    =    "E"
                     go to emi-tag-xml-200
           else if   w-sta-emi-xml-ope    =    "C"
                     go to emi-tag-xml-300
           else if   w-sta-emi-xml-ope    =    "N"
                     go to emi-tag-xml-400
           else if   w-sta-emi-xml-ope    =    "I"
                     go to emi-tag-xml-500
           else if   w-sta-emi-xml-ope    =    "D"
                     go to emi-tag-xml-600
           else if   w-sta-emi-xml-ope    =    "V"
                     go to emi-tag-xml-700.
       emi-tag-xml-100.
      *              *=================================================*
      *              * Composizione tag xml di apertura                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<"                  to   w-all-str-cat (1)      .
           move      w-sta-emi-xml-pmt    to   w-all-str-cat (2)      .
           move      ">"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-sta-emi-xml-str      .
      *                  *---------------------------------------------*
      *                  * Ad eventuale indentazione                   *
      *                  *---------------------------------------------*
           go to     emi-tag-xml-800.
       emi-tag-xml-200.
      *              *=================================================*
      *              * Composizione tag xml con valore                 *
      *              *                                                 *
      *              * '<prompt> valore </prompt>'                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore presente                     *
      *                  *---------------------------------------------*
           if        w-sta-emi-xml-val    =    spaces
                     go to emi-tag-xml-900.
      *                  *---------------------------------------------*
      *                  * Test se prompt composito                    *
      *                  *---------------------------------------------*
           move      w-sta-emi-xml-pmt    to   w-all-str-alf          .
           move      " "                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           if        w-all-str-num        not  > 1
                     go to emi-tag-xml-320.
      *                  *---------------------------------------------*
      *                  * Scorporo prompt                             *
      *                  *---------------------------------------------*
           move      w-all-str-cat (1)    to   w-sta-emi-xml-pmc      .
       emi-tag-xml-320.
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      8000                 to   w-big-str-lun          .
           move      07                   to   w-big-str-num          .
           move      "<"                  to   w-big-str-cat (1)      .
           move      w-sta-emi-xml-pma    to   w-big-str-cat (2)      .
           move      ">"                  to   w-big-str-cat (3)      .
           move      w-sta-emi-xml-val    to   w-big-str-cat (4)      .
           move      "</"                 to   w-big-str-cat (5)      .
           move      w-sta-emi-xml-pmc    to   w-big-str-cat (6)      .
           move      ">"                  to   w-big-str-cat (7)      .
           perform   big-str-cat-000      thru big-str-cat-999        .
           move      w-big-str-alf        to   w-sta-emi-xml-str      .
      *                  *---------------------------------------------*
      *                  * Ad eventuale indentazione                   *
      *                  *---------------------------------------------*
           go to     emi-tag-xml-800.
       emi-tag-xml-300.
      *              *=================================================*
      *              * Composizione tag xml di chiusura                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "</"                 to   w-all-str-cat (1)      .
           move      w-sta-emi-xml-pmt    to   w-all-str-cat (2)      .
           move      ">"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-sta-emi-xml-str      .
      *                  *---------------------------------------------*
      *                  * Ad eventuale indentazione                   *
      *                  *---------------------------------------------*
           go to     emi-tag-xml-800.
       emi-tag-xml-400.
      *              *=================================================*
      *              * Editing preliminare per numeri                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore in uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      w-sta-emi-xml-car    to   v-car                  .
           move      w-sta-emi-xml-dec    to   v-dec                  .
           move      w-sta-emi-xml-sgn    to   v-sgn                  .
           move      w-sta-emi-xml-edm    to   v-edm                  .
           move      w-sta-emi-xml-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Formato virgole e decimali                  *
      *                  *---------------------------------------------*
           inspect   w-sta-emi-xml-val
                                replacing all  ","
                                          by   "."                    .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     emi-tag-xml-900.
       emi-tag-xml-500.
      *              *=================================================*
      *              * Editing preliminare per codici Iva              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing codice                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-sta-emi-xml-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * In campo di destinazione                    *
      *                  *---------------------------------------------*
           move      v-edt                to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Formato virgole e decimali                  *
      *                  *---------------------------------------------*
           inspect   w-sta-emi-xml-val
                                replacing all  ","
                                          by   "."                    .
      *                  *---------------------------------------------*
      *                  * Se zero                                     *
      *                  *---------------------------------------------*
           if        w-sta-emi-xml-num    =    zero
                     move  "00.00"        to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     emi-tag-xml-900.
       emi-tag-xml-600.
      *              *=================================================*
      *              * Editing preliminare per le date nel formato     *
      *              *                                                 *
      *              * - AAAA-MM-GG                                    *
      *              * - AAAAMMGG                                      *
      *              *                                                 *
      *              * N.B.: utilizzare 'w-sta-emi-xml-edm' come       *
      *              *       separatore o come indicatore di formato   *
      *              *      (quest'ultima opzione, da implementare)    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore in uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Preparazione data documento                 *
      *                  *---------------------------------------------*
           move      w-sta-emi-xml-dat    to   s-dat                  .
           move      s-saa                to   w-sta-emi-xml-saa      .
           move      s-mes                to   w-sta-emi-xml-mes      .
           move      s-gio                to   w-sta-emi-xml-gio      .
      *                  *---------------------------------------------*
      *                  * Editing anno                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-sta-emi-xml-saa    to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Editing mese                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-sta-emi-xml-mes    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (3)      .
      *                  *---------------------------------------------*
      *                  * Editing giorno                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-sta-emi-xml-gio    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (5)      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      w-sta-emi-xml-edm    to   w-all-str-cat (2)      .
           move      w-sta-emi-xml-edm    to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     emi-tag-xml-900.
       emi-tag-xml-700.
      *              *=================================================*
      *              * Editing preliminare per gli importi             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore in uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      w-sta-emi-xml-dec    to   v-dec                  .
           move      w-sta-emi-xml-sgn    to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-sta-emi-xml-imp    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Formato virgole e decimali                  *
      *                  *---------------------------------------------*
           inspect   w-sta-emi-xml-val
                                replacing all  ","
                                          by   "."                    .
      *                  *---------------------------------------------*
      *                  * Se zero                                     *
      *                  *---------------------------------------------*
           if        w-sta-emi-xml-imp    =    zero
                     move  "0.00"         to   w-sta-emi-xml-val      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     emi-tag-xml-900.
       emi-tag-xml-800.
      *              *=================================================*
      *              * Eventuale indentazione                          *
      *              *                                                 *
      *              * N.B.: normalizzare sempre 'w-sta-emi-xml-inx'   *
      *              *       all'inizio dell'utilizzo di questi moduli *
      *              *                                                 *
      *              *       *** IMPORTANTE ***                        *
      *              *                                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se indentazione negativa e indice gia' *
      *                  * a zero                                      *
      *                  *---------------------------------------------*
           if        w-sta-emi-xml-inx    =    zero and
                     w-sta-emi-xml-ind    =    "-"
                     go to emi-tag-xml-820.
      *                  *---------------------------------------------*
      *                  * Determinazione indice di indentazione       *
      *                  *---------------------------------------------*
           if        w-sta-emi-xml-ind    =    "+"
                     add      w-sta-emi-xml-tab
                                          to   w-sta-emi-xml-inx
           else if   w-sta-emi-xml-ind    =    "-"
                     subtract w-sta-emi-xml-tab
                                          from w-sta-emi-xml-inx      .
       emi-tag-xml-820.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza stringa            *
      *                  *---------------------------------------------*
           move      w-sta-emi-xml-str    to   w-big-str-alf          .
           perform   big-str-lun-000      thru big-str-lun-999        .
           move      w-big-str-lun        to   w-sta-emi-xml-lun      .
      *                  *---------------------------------------------*
      *                  * Indentazione                                *
      *                  *---------------------------------------------*
           move      spaces               to   w-big-str-alf          .
           move      w-sta-emi-xml-inx    to   w-sta-emi-xml-inw      .
           add       1                    to   w-sta-emi-xml-inw      .
           move      w-sta-emi-xml-str    to   w-big-str-alf
                                              (w-sta-emi-xml-inw :
                                               w-sta-emi-xml-lun)     .
           move      w-big-str-alf        to   w-sta-emi-xml-str      .
       emi-tag-xml-850.
      *              *=================================================*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           move      w-sta-emi-xml-str    to   j-rec                  .
      *              *-------------------------------------------------*
      *              * Scrittura record in file Line-Sequential        *
      *              *-------------------------------------------------*
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       emi-tag-xml-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-tag-xml-999.
       emi-tag-xml-999.
           exit.

