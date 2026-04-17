       Identification Division.
       Program-Id.                                 msqzkk             .
      *================================================================*
      *                                                                *
      * Modulo intermedio per 'Operazione su se' stesso' alla prima    *
      * Open di un file.                                               *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Open modulo                                        *
      *                                                                *
      *             Input  : s-ope = "OP"                              *
      *                                                                *
      *                      s-num = tipo filtro                       *
      *                              - 0 : nessun filtro               *
      *                              - 1 : filtro standard             *
      *                              - 2 : filtro programma            *
      *                                                                *
      *                      s-pat = filtro                            *
      *                              - se 0 : Spaces                   *
      *                              - se 1 : Nome filtro standard     *
      *                              - se 2 : Pathname oggetto         *
      *                                                                *
      *             Output : s-sts = Status                            *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put 0       Inizio put next record nel modulo                  *
      *                                                                *
      *             Input  : s-ope = "P0"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put 5       Blocco di 80 caratteri nel modulo                  *
      *                                                                *
      *             Input  : s-ope = "P5"                              *
      *                                                                *
      *                      s-alf = blocco di 80 caratteri            *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put 9       Fine put next record nel modulo                    *
      *                                                                *
      *             Input  : s-ope = "P9"                              *
      *                                                                *
      *             Output : s-sts = Status                            *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Get 5       Estrazione blocco di 80 caratteri dal modulo dopo  *
      *             esecuzione della funzione di filtro                *
      *                                                                *
      *             Input  : s-ope = "G5"                              *
      *                                                                *
      *                      s-num = indice su blocco                  *
      *                                                                *
      *             Output : s-alf = blocco di 80 caratteri            *
      *                                                                *
      *                      s-sts = Status                            *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - #      : No blocco, fine        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Close modulo                                       *
      *                                                                *
      *             Input  : s-ope = "CL"                              *
      *                                                                *
      *             Output : s-sts = Status                            *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - #      : Ko                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     d-K-b-Snc-PD .
       Object-Computer.     d-K-b-Snc-PD .

       Special-Names.       Decimal-Point is comma .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work area per il controllo                                *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Tipo di filtro richiesto                              *
      *        * - 0 : nessun filtro                                   *
      *        * - 1 : filtro standard                                 *
      *        * - 2 : filtro programma                                *
      *        *-------------------------------------------------------*
           05  w-cnt-tip-flt              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Nome filtro o pathname programma oggetto di filtro    *
      *        *-------------------------------------------------------*
           05  w-cnt-nop-flt              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per il record letto, passato da filtrare       *
      *        *-------------------------------------------------------*
           05  w-cnt-rec-let.
               10  filler occurs 5120     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice corrispondente al numero blocco da 80 caratte- *
      *        * ri in corso di trattamento per lettura                *
      *        *-------------------------------------------------------*
           05  w-cnt-inx-let              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per indice su blocco in lettura           *
      *        *-------------------------------------------------------*
           05  w-cnt-inx-lsv              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per il record scritto, ritornato filtrato      *
      *        *-------------------------------------------------------*
           05  w-cnt-rec-scr.
               10  filler occurs 5120     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice corrispondente al numero blocco da 80 caratte- *
      *        * ri in corso di trattamento per scrittura              *
      *        *-------------------------------------------------------*
           05  w-cnt-inx-scr              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per indice su blocco in scrittura         *
      *        *-------------------------------------------------------*
           05  w-cnt-inx-ssv              pic  9(04)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msqzff" *
      *    *-----------------------------------------------------------*
       01  x.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        * - OP : Open                                           *
      *        * - FF : Filtering                                      *
      *        * - CL : Close                                          *
      *        *-------------------------------------------------------*
           05  x-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di filtro richiesto                              *
      *        * - 0 : Nessun filtro                                   *
      *        * - 1 : Filtro standard                                 *
      *        * - 2 : Filtro programma                                *
      *        *-------------------------------------------------------*
           05  x-tpf                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Nome filtro o pathname programma oggetto di filtro    *
      *        *-------------------------------------------------------*
           05  x-nmf                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Status di uscita                                      *
      *        *-------------------------------------------------------*
           05  x-sts                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Area record da filtrare e filtrato                    *
      *        *-------------------------------------------------------*
           05  x-rec.
               10  x-chr      occurs 5120 pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using s                      .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        s-ope                =    "OP"
                     perform opn-opn-000  thru opn-opn-999
           else if   s-ope                =    "P0"
                     perform put-zer-000  thru put-zer-999
           else if   s-ope                =    "P5"
                     perform put-cin-000  thru put-cin-999
           else if   s-ope                =    "P9"
                     perform put-nov-000  thru put-nov-999
           else if   s-ope                =    "G5"
                     perform get-cin-000  thru get-cin-999
           else if   s-ope                =    "CL"
                     perform cls-cls-000  thru cls-cls-999            .
       main-999.
           exit      program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-opn-000.
      *              *-------------------------------------------------*
      *              * Salvataggio tipo di filtro richiesto            *
      *              *-------------------------------------------------*
           move      s-num                to   w-cnt-tip-flt          .
      *              *-------------------------------------------------*
      *              * Salvataggio nome filtro o pathname programma    *
      *              * oggetto di filtro                               *
      *              *-------------------------------------------------*
           move      s-pat                to   w-cnt-nop-flt          .
       opn-opn-200.
      *              *-------------------------------------------------*
      *              * Open modulo filtro conversione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun filtro : nessuna operazione       *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        =    zero
                     go to opn-opn-999.
      *                  *---------------------------------------------*
      *                  * Open modulo                        "msqzff" *
      *                  *---------------------------------------------*
           move      "OP"                 to   x-ope                  .
           move      w-cnt-tip-flt        to   x-tpf                  .
           move      w-cnt-nop-flt        to   x-nmf                  .
           call      "swd/mod/prg/obj/msqzff"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con status ad errore     *
      *                  *---------------------------------------------*
           if        x-sts                not  = spaces
                     move  "##"           to   s-sts
                     go to opn-opn-999.
       opn-opn-999.
           exit.

      *    *===========================================================*
      *    * Put 0 : Inizio put record nel modulo                      *
      *    *-----------------------------------------------------------*
       put-zer-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice corrispondente al nume- *
      *              * ro blocco da 80 caratteri in corso di tratta-   *
      *              * mento                                           *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-inx-scr          .
      *              *-------------------------------------------------*
      *              * Spaces in tutto il record da comporre a blocchi *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rec-scr          .
       put-zer-999.
           exit.

      *    *===========================================================*
      *    * Put 5 : Concatenamento blocco di 80 caratteri in record   *
      *    *         nel modulo                                        *
      *    *-----------------------------------------------------------*
       put-cin-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valore attuale indice               *
      *              *-------------------------------------------------*
           move      w-cnt-inx-scr        to   w-cnt-inx-ssv          .
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           string    s-alf      delimited by   size
                                          into w-cnt-rec-scr
                                  with pointer w-cnt-inx-ssv          .
      *              *-------------------------------------------------*
      *              * Incremento indice per concatenamento            *
      *              *-------------------------------------------------*
           add       80                   to   w-cnt-inx-scr          .
       put-cin-999.
           exit.

      *    *===========================================================*
      *    * Put 9 : Fine put record nel modulo                        *
      *    *-----------------------------------------------------------*
       put-nov-000.
      *              *-------------------------------------------------*
      *              * Spostamento area di scrittura in area di lettu- *
      *              * ra                                              *
      *              *-------------------------------------------------*
           move      w-cnt-rec-scr        to   w-cnt-rec-let          .
       put-nov-200.
      *              *-------------------------------------------------*
      *              * Esecuzione filtro per mezzo del modulo "msqzff" *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun filtro : nessuna operazione       *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        =    zero
                     go to put-nov-999.
      *                  *---------------------------------------------*
      *                  * Spostamento record in area di link          *
      *                  *---------------------------------------------*
           move      w-cnt-rec-let        to   x-rec                  .
      *                  *---------------------------------------------*
      *                  * Richiamo modulo per esecuzione              *
      *                  *---------------------------------------------*
           move      "FF"                 to   x-ope                  .
           call      "swd/mod/prg/obj/msqzff"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con status di errore     *
      *                  *---------------------------------------------*
           if        x-sts                not  = spaces
                     move  "##"           to   s-sts
                     go to put-nov-999.
      *                  *---------------------------------------------*
      *                  * Ripristino record da area di link           *
      *                  *---------------------------------------------*
           move      x-rec                to   w-cnt-rec-let          .
       put-nov-999.
           exit.

      *    *===========================================================*
      *    * Get 5 : Estrazione blocco di 80 caratteri dal modulo dopo *
      *    *         esecuzione funzione di filtro                     *
      *    *-----------------------------------------------------------*
       get-cin-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valore indice passato               *
      *              *-------------------------------------------------*
           move      s-num                to   w-cnt-inx-let          .
      *              *-------------------------------------------------*
      *              * Salvataggio valore attuale indice               *
      *              *-------------------------------------------------*
           move      w-cnt-inx-let        to   w-cnt-inx-lsv          .
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           move      spaces               to   s-alf                  .
           unstring  w-cnt-rec-let        into s-alf
                                  with pointer w-cnt-inx-lsv          .
      *              *-------------------------------------------------*
      *              * Se e' avvenuta estrazione si esce con status ad *
      *              * Ok, altrimenti si esce con status a Ko          *
      *              *-------------------------------------------------*
           if        w-cnt-inx-let        not  = w-cnt-inx-lsv
                     move  spaces         to   s-sts
           else      move  "##"           to   s-sts                  .
       get-cin-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-cls-000.
      *              *-------------------------------------------------*
      *              * Close modulo filtro conversione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessun filtro : nessuna operazione       *
      *                  *---------------------------------------------*
           if        w-cnt-tip-flt        =    zero
                     go to cls-cls-999.
      *                  *---------------------------------------------*
      *                  * Close modulo                       "msqzff" *
      *                  *---------------------------------------------*
           move      "CL"                 to   x-ope                  .
           call      "swd/mod/prg/obj/msqzff"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita con status ad errore     *
      *                  *---------------------------------------------*
           if        x-sts                not  = spaces
                     move  "##"           to   s-sts
                     go to cls-cls-999.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo               "msqzff" *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/msqzff"                         .
       cls-cls-999.
           exit.

