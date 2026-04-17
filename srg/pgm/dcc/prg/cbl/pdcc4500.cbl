       Identification Division.
       Program-Id.                                 pdcc4500           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    com                 *
      *                                   Fase:    dcc450              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/06/93    *
      *                       Ultima revisione:    NdK del 02/09/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Manutenzioni anagrafiche clienti            *
      *                                                                *
      *                    Main                                        *
      *                                                                *
      *                    N.B.: richiamato da 'pdcc9500' per la       *
      *                          generazione di 'set'                  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * La fase dcc450, nella sua globalita', esegue i tipi di manu-   *
      * tenzione elencati nella tabella seguente :                     *
      *                                                                *
      *                                                                *
      * Codice                                                         *
      *  tipo                                                          *
      * manut.       Descrizione per il tipo manutenzione      Modulo  *
      * ------  --------------------------------------------  -------- *
      *                                                                *
      * MANGEO  Localizzazione geografica                     pdcc450a *
      * MANNTI  Utenze varie ed interlocutori                 pdcc450b *
      * MANVEN  Condizioni di vendita e provv.                pdcc450c *
      * MANFAT  Modalita' di fatturazione                     pdcc450d *
      * MANCON  Modalita' di consegna                         pdcc450e *
      * MANPAG  Condizioni di pagamento                       pdcc450f *
      * MANSTA  Codici statistici                             pdcc450g *
      * MANGRA  Relativa al gruppo d'acquisto                 pdcc450h *
      * MANSDI  Codici Destinatario SDI e PEC                 pdcc450p *
      * MANEDI  Codici per gestione documenti EDI             pdcc450q *
      * MANTUS  Relativa allo status cliente                  pdcc450s *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

       Special-Names.       Decimal-Point is comma .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c. globali lette all'ini-  *
      *    * zio dell'esecuzione                                       *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabile 'bol300-sub-pgm'                            *
      *        *-------------------------------------------------------*
           05  w-ipc-sub-pgm.
      *            *---------------------------------------------------*
      *            * Valore della variabile                            *
      *            *---------------------------------------------------*
               10  w-ipc-sub-pgm-val.
      *                *-----------------------------------------------*
      *                * Dati relativi all'identificazione             *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ide.
      *                    *-------------------------------------------*
      *                    * Sistema applicativo                       *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-sap
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Area gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-arg
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Settore gestionale                        *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-set
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Fase gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-fas
                                          pic  x(06)                  .
      *                    *-------------------------------------------*
      *                    * Sigla interna del programma               *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-pro
                                          pic  x(10)                  .
      *                    *-------------------------------------------*
      *                    * Descrizione del programma                 *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-des
                                          pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Indicatore                                    *
      *                *  - Spaces : Nessun significato                *
      *                *  - CS     : Il programma e' eseguito come     *
      *                *             sottoprogramma per la creazione   *
      *                *             di un SET                         *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ind  pic  x(02)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pdcc4500       *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/pdcc4500.pgl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .
      *        *-------------------------------------------------------*
      *        * [zdf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzdf"                          .

      *    *===========================================================*
      *    * Work-area per visualizzazione pagina attuale              *
      *    *-----------------------------------------------------------*
       01  w-vis-pag.
      *        *-------------------------------------------------------*
      *        * Numero linea video per il numero pagina               *
      *        *-------------------------------------------------------*
           05  w-vis-pag-lin-000          pic  9(02)       value 06   .
      *        *-------------------------------------------------------*
      *        * Si/No pagina precedente alla pagina attuale           *
      *        *-------------------------------------------------------*
           05  w-vis-pag-snx-pre          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No pagina seguente alla pagina attuale             *
      *        *-------------------------------------------------------*
           05  w-vis-pag-snx-seg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice primo elemento visualizzato nella pagina at-   *
      *        * tuale                                                 *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-pel          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice ultimo elemento visualizzato nella pagina at-  *
      *        * tuale                                                 *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-uel          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice per scansione su elementi da visualizzare      *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-tbl          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione linea a video per elemento  *
      *        * in corso di trattamento                               *
      *        *-------------------------------------------------------*
           05  w-vis-pag-lin-acv          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione campi accettazione campi    *
      *    * chiave trattati dal main                                  *
      *    *-----------------------------------------------------------*
       01  w-key.
      *        *-------------------------------------------------------*
      *        * Codice numerico tipo manutenzione                     *
      *        *-------------------------------------------------------*
           05  w-key-num-man              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico tipo manutenzione                 *
      *        *-------------------------------------------------------*
           05  w-key-alf-man              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per codice tipo manutenzione              *
      *        *-------------------------------------------------------*
           05  w-key-des-man              pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Overlay per il tipo manutenzione                      *
      *        *-------------------------------------------------------*
           05  w-key-ovy-man              pic  x(10)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Lettura della variabile di i.p.c. di tipo glo-  *
      *              * bale 'dcc450-sub-pgm'                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   ipc-sub-pgm-000      thru ipc-sub-pgm-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione ciclo per la fase 'dcc450'           *
      *              *-------------------------------------------------*
           perform   fas-dcc-450-000      thru fas-dcc-450-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Lettura della variabile di i.p.c. 'bol300-sub-pgm'        *
      *    *-----------------------------------------------------------*
       ipc-sub-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni valore della variabile          *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura e cancellazione della variabile globale *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dcc450-sub-pgm"     to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se variabile non esistente : uscita             *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     go to ipc-sub-pgm-999.
      *              *-------------------------------------------------*
      *              * Se variabile di formato inaspettato : uscita    *
      *              *-------------------------------------------------*
           if        s-tip                not  = "A" or
                     s-car                not  = 80
                     go to ipc-sub-pgm-999.
       ipc-sub-pgm-200.
      *              *-------------------------------------------------*
      *              * Valore della variabile in area di bufferizza-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-300.
      *              *-------------------------------------------------*
      *              * Se indicatore di tipo non riconosciuto : uscita *
      *              *-------------------------------------------------*
           if        w-ipc-sub-pgm-ind    =    "CS"
                     go to ipc-sub-pgm-400
           else      go to ipc-sub-pgm-999.
       ipc-sub-pgm-400.
      *              *-------------------------------------------------*
      *              * Se indicatore di tipo "CS"                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assestamento area di identificazione del    *
      *                  * programma                                   *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-sap    to   i-ide-sap              .
           move      w-ipc-sub-pgm-arg    to   i-ide-arg              .
           move      w-ipc-sub-pgm-set    to   i-ide-set              .
           move      w-ipc-sub-pgm-fas    to   i-ide-fas              .
           move      w-ipc-sub-pgm-pro    to   i-ide-pro              .
           move      w-ipc-sub-pgm-des    to   i-ide-des              .
       ipc-sub-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "S"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm      .
      *              *-------------------------------------------------*
      *              * Flag di eventuale visualizzazione forzata       *
      *              *-------------------------------------------------*
           move      s-sts                to   w-cnt-mfu-vis-sgr      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Flag di pre-esecuzione                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
      *              *-------------------------------------------------*
      *              * Azzeramento tabella sottoprogrammi attivi       *
      *              *-------------------------------------------------*
           move      zero                 to   w-spg-ele-num          .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Personalizzazione relativa al tipo di ordi- *
      *                  * namento anagrafiche clienti                 *
      *                  *---------------------------------------------*
           perform   prs-tip-ord-000      thru prs-tip-ord-999        .
      *                  *---------------------------------------------*
      *                  * Numero livelli del piano dei conti          *
      *                  *---------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione archivio per fatturazione    *
      *                  *---------------------------------------------*
           perform   prs-ges-apf-000      thru prs-ges-apf-999        .
      *                  *---------------------------------------------*
      *                  * Spese per la fatturazione                   *
      *                  *---------------------------------------------*
           perform   prs-spe-fat-000      thru prs-spe-fat-999        .
      *                  *---------------------------------------------*
      *                  * Voci descrittive per la fatturazione        *
      *                  *---------------------------------------------*
           perform   prs-vde-fat-000      thru prs-vde-fat-999        .
      *                  *---------------------------------------------*
      *                  * Si/No bollettazione attiva                  *
      *                  *---------------------------------------------*
           perform   prs-bol-snx-000      thru prs-bol-snx-999        .
      *                  *---------------------------------------------*
      *                  * Tipo trattamento codice categoria per le    *
      *                  * dipendenze                                  *
      *                  *---------------------------------------------*
           perform   prs-trt-cat-000      thru prs-trt-cat-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione documenti via EDI            *
      *                  *---------------------------------------------*
           perform   prs-snx-edi-000      thru prs-snx-edi-999        .
       pre-exe-pgm-400.
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della tabella dei tipi in- *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           perform   loa-tbl-tmn-000      thru loa-tbl-tmn-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione per il tipo ordinamento   *
      *    *-----------------------------------------------------------*
       prs-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work-area                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-prs-tip-ord-tip      .
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione                   *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc/com/dcc450[tip-ord]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to prs-tip-ord-500.
      *                  *---------------------------------------------*
      *                  * Valore personalizzazione in work-area       *
      *                  *---------------------------------------------*
           move      s-alf                to   w-prs-tip-ord-tip      .
      *                  *---------------------------------------------*
      *                  * Controllo personalizzazione                 *
      *                  *---------------------------------------------*
           if        w-prs-tip-ord-tip    not  = "R" and
                     w-prs-tip-ord-tip    not  = "C" and
                     w-prs-tip-ord-tip    not  = "M"
                     move  spaces         to   w-prs-tip-ord-tip      .
       prs-tip-ord-500.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione letta                        *
      *                  *---------------------------------------------*
           if        w-prs-tip-ord-tip    =    spaces
                     go to prs-tip-ord-600
           else      go to prs-tip-ord-999.
       prs-tip-ord-600.
      *                  *---------------------------------------------*
      *                  * Se la personalizzazione non esprime nessuna *
      *                  * preferenza , l'ordinamento sara' per ragio- *
      *                  * ne sociale                                  *
      *                  *---------------------------------------------*
           move      "R"                  to   w-prs-tip-ord-tip      .
           go to     prs-tip-ord-999.
       prs-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al numero di li- *
      *    * velli del piano dei conti                                 *
      *    *-----------------------------------------------------------*
       prs-liv-pdc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-liv-pdc        not  = 2
                     move  3              to   w-prs-liv-pdc          .
       prs-liv-pdc-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione archivio per   *
      *    *                             fatturazione                  *
      *    *-----------------------------------------------------------*
       prs-ges-apf-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[ges-apf]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-ges-apf
           else      move  zero           to   w-prs-ges-apf          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-ges-apf        not  = 00 and
                     w-prs-ges-apf        not  = 11 and
                     w-prs-ges-apf        not  = 21 and
                     w-prs-ges-apf        not  = 99
                     move  00             to   w-prs-ges-apf          .
       prs-ges-apf-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alle spese per   *
      *    * la fatturazione                                           *
      *    *-----------------------------------------------------------*
       prs-spe-fat-000.
      *              *-------------------------------------------------*
      *              * Open tabella [zsf]                              *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
       prs-spe-fat-100.
      *              *-------------------------------------------------*
      *              * Numero di spese personalizzate caricate : zero  *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-spe-fat-nst      .
       prs-spe-fat-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura spese da 1 a 6 con carica- *
      *              * mento in tabella delle spese trovate            *
      *              *-------------------------------------------------*
       prs-spe-fat-250.
      *                  *---------------------------------------------*
      *                  * Indice 1..6 a zero                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-spe-fat-i01      .
       prs-spe-fat-300.
      *                  *---------------------------------------------*
      *                  * Incremento indice 1..6                      *
      *                  *---------------------------------------------*
           add       1                    to   w-prs-spe-fat-i01      .
      *                  *---------------------------------------------*
      *                  * Se oltre il max : a chiusura                *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-i01    >    6
                     go to prs-spe-fat-900.
       prs-spe-fat-400.
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura della tabella per la   *
      *                  * spesa corripsondente all'indice, e con co-  *
      *                  * dice lingua per Italia                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-prs-spe-fat-i01    to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prs-spe-fat-500
           else      go to prs-spe-fat-700.
       prs-spe-fat-500.
      *                  *---------------------------------------------*
      *                  * Se record spesa esistente                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo di correttezza, e se non e'   *
      *                      * superato si ricicla a spesa successiva  *
      *                      *-----------------------------------------*
           if        rf-zsf-tfu-spe       <    01 or
                     rf-zsf-tfu-spe       >    05
                     go to prs-spe-fat-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione spesa                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento numero di spese persona- *
      *                          * lizzate caricate                    *
      *                          *-------------------------------------*
           add       1                    to   w-prs-spe-fat-nst      .
      *                          *-------------------------------------*
      *                          * Numero spesa                        *
      *                          *-------------------------------------*
           move      w-prs-spe-fat-i01    to   w-prs-spe-fat-npt
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-prs-spe-fat-dve
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Tipo funzionamento spesa            *
      *                          *-------------------------------------*
           move      rf-zsf-tfu-spe       to   w-prs-spe-fat-tfs
                                              (w-prs-spe-fat-nst)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su spesa successiva             *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-700.
      *                  *---------------------------------------------*
      *                  * Se record spesa non esistente               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo a spesa successiva              *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-800.
      *                  *---------------------------------------------*
      *                  * Riciclo a spesa successiva                  *
      *                  *---------------------------------------------*
           go to     prs-spe-fat-300.
       prs-spe-fat-900.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea a video, diminuito  *
      *              * di uno, per la prima spesa                      *
      *              *-------------------------------------------------*
           if        w-prs-spe-fat-nst    =    zero
                     move  zero           to   w-prs-spe-fat-lv1
           else      move  07             to   w-prs-spe-fat-lv1      .
      *              *-------------------------------------------------*
      *              * Close tabella [zsf]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
       prs-spe-fat-999.
           exit.
           
      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alle voci de-    *
      *    * scrittive per la fatturazione                             *
      *    *-----------------------------------------------------------*
       prs-vde-fat-000.
      *              *-------------------------------------------------*
      *              * Open tabella [zdf]                              *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
       prs-vde-fat-100.
      *              *-------------------------------------------------*
      *              * Numero di voci personalizzate caricate : zero   *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-vde-fat-nvt      .
       prs-vde-fat-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura voci da 1 a 6 con carica-  *
      *              * mento in tabella delle voci trovate             *
      *              *-------------------------------------------------*
       prs-vde-fat-250.
      *                  *---------------------------------------------*
      *                  * Indice 1..6 a zero                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-vde-fat-i01      .
       prs-vde-fat-300.
      *                  *---------------------------------------------*
      *                  * Incremento indice 1..6                      *
      *                  *---------------------------------------------*
           add       1                    to   w-prs-vde-fat-i01      .
      *                  *---------------------------------------------*
      *                  * Se oltre il max : a chiusura                *
      *                  *---------------------------------------------*
           if        w-prs-vde-fat-i01    >    6
                     go to prs-vde-fat-900.
       prs-vde-fat-400.
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura della tabella per la   *
      *                  * voce corripsondente all'indice, e con co-   *
      *                  * dice lingua per Italia                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDEF    "         to   f-key                  .
           move      w-prs-vde-fat-i01    to   rf-zdf-num-def         .
           move      "I  "                to   rf-zdf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prs-vde-fat-500
           else      go to prs-vde-fat-700.
       prs-vde-fat-500.
      *                  *---------------------------------------------*
      *                  * Se record voce descrittiva esistente        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione voce                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento numero di voci persona-  *
      *                          * lizzate caricate                    *
      *                          *-------------------------------------*
           add       1                    to   w-prs-vde-fat-nvt      .
      *                          *-------------------------------------*
      *                          * Numero voce                         *
      *                          *-------------------------------------*
           move      w-prs-vde-fat-i01    to   w-prs-vde-fat-npt
                                              (w-prs-vde-fat-nvt)     .
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zdf-des-ves       to   w-prs-vde-fat-dve
                                              (w-prs-vde-fat-nvt)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su voce successiva              *
      *                      *-----------------------------------------*
           go to     prs-vde-fat-800.
       prs-vde-fat-700.
      *                  *---------------------------------------------*
      *                  * Se record voce descrittiva non esistente    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo a voce successiva               *
      *                      *-----------------------------------------*
           go to     prs-vde-fat-800.
       prs-vde-fat-800.
      *                  *---------------------------------------------*
      *                  * Riciclo a voce successiva                   *
      *                  *---------------------------------------------*
           go to     prs-vde-fat-300.
       prs-vde-fat-900.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea a video, diminuito  *
      *              * di uno, per la prima spesa                      *
      *              *-------------------------------------------------*
           if        w-prs-vde-fat-nvt    =    zero
                     move  zero           to   w-prs-vde-fat-lv1
                     go to prs-vde-fat-950.
           if        w-prs-spe-fat-lv1    =    zero
                     move  07             to   w-prs-vde-fat-lv1
           else      move  15             to   w-prs-vde-fat-lv1      .
       prs-vde-fat-950.
      *              *-------------------------------------------------*
      *              * Close tabella [zdf]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
       prs-vde-fat-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No bollettazione attiva    *
      *    *-----------------------------------------------------------*
       prs-bol-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/bol[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-bol-snx
           else      move  spaces         to   w-prs-bol-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-bol-snx        =    "S" or
                     w-prs-bol-snx        =    "N"
                     go to prs-bol-snx-999.
           move      "N"                  to   w-prs-bol-snx          .
       prs-bol-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo trattamento codice       *
      *    *                             categoria per le dipendenze   *
      *    *-----------------------------------------------------------*
       prs-trt-cat-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[trt-cat]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-trt-cat
           else      move  zero           to   w-prs-trt-cat          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-trt-cat        not  = 01
                     move  00             to   w-prs-trt-cat          .
       prs-trt-cat-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione documenti EDI  *
      *    *-----------------------------------------------------------*
       prs-snx-edi-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/azi[snx-edi]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-edi
           else      move  spaces         to   w-prs-snx-edi          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-edi        not  = "S" and
                     w-prs-snx-edi        not  = "N"
                     move  "N"            to   w-prs-snx-edi          .
       prs-snx-edi-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della tabella dei tipi manutenzione  *
      *    *-----------------------------------------------------------*
       loa-tbl-tmn-000.
      *              *-------------------------------------------------*
      *              * Azzeramento preliminare numero effettivo di e-  *
      *              * lementi in tabella                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-tmn-ele-num          .
       loa-tbl-tmn-100.
      *              *-------------------------------------------------*
      *              * Caricamento elementi in tabella con incremento  *
      *              * del numero effettivo di elementi in tabella     *
      *              *-------------------------------------------------*
           move      "Localizzazione geografica                         
      -              " MANGEO     pdcc450a  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Utenze varie ed interlocutori                     
      -              " MANNTI     pdcc450b  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Condizioni di vendita e provvigioni               
      -              " MANVEN     pdcc450c  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Modalita' di fatturazione                         
      -              " MANFAT     pdcc450d  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Modalita' di consegna                             
      -              " MANCON     pdcc450e  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Condizioni di pagamento                           
      -              " MANPAG     pdcc450f  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Codici statistici                                 
      -              " MANSTA     pdcc450g  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           if        w-prs-ges-apf        not  = 21 and
                     w-prs-ges-apf        not  = 99
                     go to loa-tbl-tmn-180.
           move      "Informazioni relative al gruppo d'acquisto        
      -              " MANGRA     pdcc450h  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
       loa-tbl-tmn-180.
           move      "Codici Destinatario SDI e PEC                     
      -              " MANSDI     pdcc450p  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           if        w-prs-snx-edi        not  = "S"
                     go to loa-tbl-tmn-190.
           move      "Codici per gestione documenti EDI                 
      -              " MANEDI     pdcc450q  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
       loa-tbl-tmn-190.
           move      "Status cliente                                    
      -              " MANTUS     pdcc450s  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
       loa-tbl-tmn-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione elementi residui                *
      *              *-------------------------------------------------*
           move      w-tmn-ele-num        to   w-tmn-ele-inx          .
       loa-tbl-tmn-225.
           add       1                    to   w-tmn-ele-inx          .
           if        w-tmn-ele-inx        >    w-tmn-ele-max
                     go to loa-tbl-tmn-300.
           move      zero                 to   w-tmn-num-tmn
                                              (w-tmn-ele-inx)         .
           move      spaces               to   w-tmn-alf-tmn
                                              (w-tmn-ele-inx)         .
           move      spaces               to   w-tmn-des-tmn
                                              (w-tmn-ele-inx)         .
           move      spaces               to   w-tmn-ovy-tmn
                                              (w-tmn-ele-inx)         .
           go to     loa-tbl-tmn-225.
       loa-tbl-tmn-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagine totali             *
      *              *-------------------------------------------------*
           move      w-tmn-ele-num        to   w-tmn-ele-npt          .
           add       w-tmn-ele-nep        to   w-tmn-ele-npt          .
           subtract  1                    from w-tmn-ele-npt          .
           divide    w-tmn-ele-nep        into w-tmn-ele-npt          .
       loa-tbl-tmn-400.
      *              *-------------------------------------------------*
      *              * Numero pagina attualmente visualizzata a : 1    *
      *              *-------------------------------------------------*
           move      1                    to   w-tmn-ele-pag          .
       loa-tbl-tmn-999.
           exit.

      *    *===========================================================*
      *    * Caricamento elemento in tabella tipi manutenzione         *
      *    *-----------------------------------------------------------*
       loa-tbl-ele-000.
      *              *-------------------------------------------------*
      *              * Incremento numero effettivo di elementi in ta-  *
      *              * bella, a meno di non essere gia' in saturazio-  *
      *              * ne della tabella                                *
      *              *-------------------------------------------------*
           if        w-tmn-ele-num        =    w-tmn-ele-max
                     go to loa-tbl-ele-999
           else      add   1              to   w-tmn-ele-num          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice numerico tipo manuten-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      w-tmn-ele-num        to   w-tmn-num-tmn
                                              (w-tmn-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice alfanumerico tipo manu-  *
      *              * tenzione                                        *
      *              *-------------------------------------------------*
           move      w-tmn-ele-wci-alf    to   w-tmn-alf-tmn
                                              (w-tmn-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione descrizione tipo manutenzione   *
      *              *-------------------------------------------------*
           move      w-tmn-ele-wci-des    to   w-tmn-des-tmn
                                              (w-tmn-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione overlay da richiamare per il    *
      *              * tipo manutenzione                               *
      *              *-------------------------------------------------*
           move      w-tmn-ele-wci-ovy    to   w-tmn-ovy-tmn
                                              (w-tmn-ele-num)         .
       loa-tbl-ele-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ciclo per la fase 'dcc450'                     *
      *    *-----------------------------------------------------------*
       fas-dcc-450-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dcc-450-025.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
       fas-dcc-450-050.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale valori di accettazione *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Codice numerico per il tipo manutenzione   *
      *                   *--------------------------------------------*
           move      zero                 to   w-key-num-man          .
      *                   *--------------------------------------------*
      *                   * Codice alfanumerico per il tipo manuten-   *
      *                   * zione                                      *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-alf-man          .
      *                   *--------------------------------------------*
      *                   * Descrizione per il tipo manutenzione       *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-des-man          .
      *                   *--------------------------------------------*
      *                   * Nome overlay per il tipo manutenzione      *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-ovy-man          .
       fas-dcc-450-075.
      *              *-------------------------------------------------*
      *              * Prompts per impostazione tipo manutenzione      *
      *              *-------------------------------------------------*
       fas-dcc-450-077.
      *                  *---------------------------------------------*
      *                  * Prompt effettivo per tipo manutenzione      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
      *
           if        w-ipc-sub-pgm-ind    =    "CS"
                     move  "Tipo selezione      :"
                                          to   v-alf
           else      move "Tipo manutenzione   :"
                                          to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dcc-450-079.
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice numerico per tipo    *
      *                  * manutenzione                                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-key-num-man        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dcc-450-081.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione per tipo manu-  *
      *                  * tenzione                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-man        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dcc-450-083.
      *                  *---------------------------------------------*
      *                  * Visualizzazione trattini di separazione     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dcc-450-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina attuale                  *
      *              *-------------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
       fas-dcc-450-125.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dcc-450-200.
      *              *-------------------------------------------------*
      *              * Accettazione codice numerico tipo manutenzione  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           if        w-tmn-ele-num        >    zero
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-vis-pag-snx-pre    =    "S"
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-vis-pag-snx-seg    =    "S"
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-key-num-man        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tasto di funzione usa- *
      *              * to                                              *
      *              *-------------------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to fas-dcc-450-300
           else if   v-key                =    "PRSC"
                     go to fas-dcc-450-400
           else if   v-key                =    "NXSC"
                     go to fas-dcc-450-450
           else if   v-key                =    "DOWN"
                     go to fas-dcc-450-500
           else if   v-key                =    "EXIT"
                     go to fas-dcc-450-250
           else      go to fas-dcc-450-200.
       fas-dcc-450-250.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fas-dcc-450-999.
       fas-dcc-450-300.
      *              *-------------------------------------------------*
      *              * Se Return o Do                                  *
      *              *-------------------------------------------------*
       fas-dcc-450-310.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           move      v-num                to   w-key-num-man          .
       fas-dcc-450-320.
      *                  *---------------------------------------------*
      *                  * Ricerca codice numerico impostato in ta-    *
      *                  * bella dei tipi manutenzione, e deviazione   *
      *                  * a seconda dell'esito della ricerca          *
      *                  *---------------------------------------------*
           if        w-key-num-man        =    zero
                     go to fas-dcc-450-330.
           move      zero                 to   w-tmn-ele-inx          .
       fas-dcc-450-322.
           add       1                    to   w-tmn-ele-inx          .
           if        w-tmn-ele-inx        >    w-tmn-ele-num
                     go to fas-dcc-450-340.
           if        w-tmn-num-tmn
                    (w-tmn-ele-inx)       =    w-key-num-man
                     go to fas-dcc-450-350
           else      go to fas-dcc-450-322.
       fas-dcc-450-330.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato : a zero       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo manu-   *
      *                      * tenzione a spaces                       *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-man          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo manutenzione    *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-des-man          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo manutenzione   *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-man          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * manutenzione                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-man        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad impostazione del codice nu-  *
      *                      * merico per il tipo di manutenzione      *
      *                      *-----------------------------------------*
           go to     fas-dcc-450-200.
       fas-dcc-450-340.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato non esistente  *
      *                  * in tabella dei tipi manutenzione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo manu-   *
      *                      * tenzione a spaces                       *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-man          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo manutenzione    *
      *                      * a puntini                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-key-des-man          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo manutenzione   *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-man          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * manutenzione                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-man        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad impostazione del codice nu-  *
      *                      * merico per il tipo di manutenzione      *
      *                      *-----------------------------------------*
           go to     fas-dcc-450-200.
       fas-dcc-450-350.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato trovato nella  *
      *                  * tabella dei tipi manutenzione               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo di in-  *
      *                      * terrogazione                            *
      *                      *-----------------------------------------*
           move      w-tmn-alf-tmn
                    (w-tmn-ele-inx)       to   w-key-alf-man          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo di manutenzio-  *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      w-tmn-des-tmn
                    (w-tmn-ele-inx)       to   w-key-des-man          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo di manutenzio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      w-tmn-ovy-tmn
                    (w-tmn-ele-inx)       to   w-key-ovy-man          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * manutenzione                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-man        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione del nome della overlay   *
      *                      * per formare il pathname completo per    *
      *                      * il richiamo del programma di esecuzione *
      *                      *-----------------------------------------*
           move      w-key-ovy-man        to   w-ovy-exe-pos          .
      *                      *-----------------------------------------*
      *                      * Se il nome della overlay e' a spaces :  *
      *                      * si ricicla ad accettazione del codice   *
      *                      * numerico per il tipo di manutenzione    *
      *                      *-----------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to fas-dcc-450-200.
      *                      *-----------------------------------------*
      *                      * Altrimenti si va' al richiamo effettivo *
      *                      * del programma di esecuzione             *
      *                      *-----------------------------------------*
           go to     fas-dcc-450-700.
       fas-dcc-450-400.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Decremento numero pagina attualmente visua- *
      *                  * lizzata                                     *
      *                  *---------------------------------------------*
           subtract  1                    from w-tmn-ele-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina attuale              *
      *                  *---------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di manutenzione                     *
      *                  *---------------------------------------------*
           go to     fas-dcc-450-200.
       fas-dcc-450-450.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina attualmente visua- *
      *                  * lizzata                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-tmn-ele-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina attuale              *
      *                  *---------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di manutenzione                     *
      *                  *---------------------------------------------*
           go to     fas-dcc-450-200.
       fas-dcc-450-500.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
       fas-dcc-450-525.
      *                  *---------------------------------------------*
      *                  * Normalizzazione e visualizzazione valori di *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico per il tipo manutenzio- *
      *                      * ne a zero                               *
      *                      *-----------------------------------------*
           move      zero                 to   w-key-num-man          .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo manu-   *
      *                      * tenzione a spaces                       *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-man          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo manutenzione    *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-des-man          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo manutenzione   *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-man          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice numerico per il  *
      *                      * tipo manutenzione                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-key-num-man        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * manutenzione                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-man        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dcc-450-550.
      *                  *---------------------------------------------*
      *                  * Accettazione direttamente da corpo video    *
      *                  *---------------------------------------------*
       fas-dcc-450-555.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice per scansione   *
      *                      * su elementi visualizzati                *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
       fas-dcc-450-560.
      *                      *-----------------------------------------*
      *                      * Determinazione linea a video per ele-   *
      *                      * mento in esame                          *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-tbl    to   w-vis-pag-lin-acv      .
           subtract  w-vis-pag-inx-pel    from w-vis-pag-lin-acv      .
           add       w-vis-pag-lin-000    to   w-vis-pag-lin-acv      .
           add       1                    to   w-vis-pag-lin-acv      .
       fas-dcc-450-570.
      *                      *-----------------------------------------*
      *                      * Accettazione tasto di funzione alla li- *
      *                      * nea corrispondente all'elemento in esa- *
      *                      * me                                      *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           if        w-vis-pag-inx-tbl    <    w-tmn-ele-num
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-vis-pag-snx-pre    =    "S"
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-vis-pag-snx-seg    =    "S"
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dcc-450-580.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tasto di fun-  *
      *                      * zione usato                             *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to fas-dcc-450-630
           else if   v-key                =    "PRSC"
                     go to fas-dcc-450-600
           else if   v-key                =    "NXSC"
                     go to fas-dcc-450-610
           else if   v-key                =    "UP  "
                     go to fas-dcc-450-620
           else if   v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to fas-dcc-450-650
           else if   v-key                =    "EXIT"
                     go to fas-dcc-450-660
           else      go to fas-dcc-450-570.
       fas-dcc-450-600.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attualmen- *
      *                          * te visualizzata                     *
      *                          *-------------------------------------*
           subtract  1                    from w-tmn-ele-pag          .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina attuale      *
      *                          *-------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                          *-------------------------------------*
      *                          * Indice di scansione al primo ele-   *
      *                          * mento della pagina                  *
      *                          *-------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione tasto di    *
      *                          * funzione                            *
      *                          *-------------------------------------*
           go to     fas-dcc-450-560.
       fas-dcc-450-610.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attualmen- *
      *                          * te visualizzata                     *
      *                          *-------------------------------------*
           add       1                    to   w-tmn-ele-pag          .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina attuale      *
      *                          *-------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                          *-------------------------------------*
      *                          * Indice di scansione al primo ele-   *
      *                          * mento della pagina                  *
      *                          *-------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione tasto di    *
      *                          * funzione                            *
      *                          *-------------------------------------*
           go to     fas-dcc-450-560.
       fas-dcc-450-620.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
       fas-dcc-450-622.
      *                          *-------------------------------------*
      *                          * Se si e' al primo elemento in asso- *
      *                          * luto della tabella si ricicla ad    *
      *                          * accettazione codice numerico per il *
      *                          * tipo manutenzione                   *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    1
                     go to fas-dcc-450-200.
       fas-dcc-450-624.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-pel
                     go to fas-dcc-450-626
           else      go to fas-dcc-450-628.
       fas-dcc-450-626.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame e' il primo  *
      *                          * della pagina                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Decremento numero pagina at-    *
      *                              * tualmente visualizzata          *
      *                              *---------------------------------*
           subtract  1                    from w-tmn-ele-pag          .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina attuale  *
      *                              *---------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                              *---------------------------------*
      *                              * Indice di scansione all'ultimo  *
      *                              * elemento della pagina           *
      *                              *---------------------------------*
           move      w-vis-pag-inx-uel    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dcc-450-560.
       fas-dcc-450-628.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame non e' il    *
      *                          * primo della pagina                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Decremento indice di scansione  *
      *                              *---------------------------------*
           subtract  1                    from w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dcc-450-560.
       fas-dcc-450-630.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
       fas-dcc-450-632.
      *                          *-------------------------------------*
      *                          * Se si e' all'ultimo elemento in as- *
      *                          * soluto della tabella si ricicla ad  *
      *                          * accettazione tasto di funzione      *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-tmn-ele-num
                     go to fas-dcc-450-570.
       fas-dcc-450-634.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-uel
                     go to fas-dcc-450-636
           else      go to fas-dcc-450-638.
       fas-dcc-450-636.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame e' l'ultimo  *
      *                          * della pagina                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Incremento numero pagina at-    *
      *                              * tualmente visualizzata          *
      *                              *---------------------------------*
           add       1                    to   w-tmn-ele-pag          .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina attuale  *
      *                              *---------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                              *---------------------------------*
      *                              * Indice di scansione al primo    *
      *                              * elemento della pagina           *
      *                              *---------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dcc-450-560.
       fas-dcc-450-638.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame non e' l'ul- *
      *                          * timo della pagina                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Incremento indice di scansione  *
      *                              *---------------------------------*
           add       1                    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dcc-450-560.
       fas-dcc-450-650.
      *                      *-----------------------------------------*
      *                      * Se Return o Do o Slct                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice numerico per il tipo manu-   *
      *                          * tenzione                            *
      *                          *-------------------------------------*
           move      w-tmn-num-tmn
                    (w-vis-pag-inx-tbl)   to   w-key-num-man          .
      *                          *-------------------------------------*
      *                          * Codice alfanumerico per il tipo     *
      *                          * manutenzione                        *
      *                          *-------------------------------------*
           move      w-tmn-alf-tmn
                    (w-vis-pag-inx-tbl)   to   w-key-alf-man          .
      *                          *-------------------------------------*
      *                          * Descrizione per il tipo manutenzio- *
      *                          * ne                                  *
      *                          *-------------------------------------*
           move      w-tmn-des-tmn
                    (w-vis-pag-inx-tbl)   to   w-key-des-man          .
      *                          *-------------------------------------*
      *                          * Nome overlay per il tipo manuten-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      w-tmn-ovy-tmn
                    (w-vis-pag-inx-tbl)   to   w-key-ovy-man          .
      *                          *-------------------------------------*
      *                          * Memorizzazione del nome della over- *
      *                          * lay per formare il pathname comple- *
      *                          * to per il richiamo del programma di *
      *                          * esecuzione                          *
      *                          *-------------------------------------*
           move      w-key-ovy-man        to   w-ovy-exe-pos          .
      *                          *-------------------------------------*
      *                          * Se il nome della overlay e' a spa-  *
      *                          * ces : si ricicla ad accettazione    *
      *                          * tasto di funzione                   *
      *                          *-------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to fas-dcc-450-570.
      *                          *-------------------------------------*
      *                          * Altrimenti si va' al richiamo ef-   *
      *                          * fettivo del programma di esecuzione *
      *                          *-------------------------------------*
           go to     fas-dcc-450-700.
       fas-dcc-450-660.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ritorno all'accettazione del codice *
      *                          * numerico per il tipo manutenzione   *
      *                          *-------------------------------------*
           go to     fas-dcc-450-200.
       fas-dcc-450-700.
      *              *-------------------------------------------------*
      *              * Richiamo del programma di esecuzione            *
      *              *-------------------------------------------------*
           perform   ric-pgm-exe-000      thru ric-pgm-exe-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile 'snx-slc' dallo stesso  *
      *              * livello di profondita' applicativa              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'esito della lettura *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to fas-dcc-450-725
           else      go to fas-dcc-450-750.
       fas-dcc-450-725.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fas-dcc-450-999.
       fas-dcc-450-750.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice numerico per tipo    *
      *                  * manutenzione                                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-key-num-man        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione per tipo manu-  *
      *                  * tenzione                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-man        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di manutenzione                     *
      *                  *---------------------------------------------*
           go to     fas-dcc-450-200.
       fas-dcc-450-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina attualmente visualizzata           *
      *    *-----------------------------------------------------------*
       vis-pag-att-000.
      *              *-------------------------------------------------*
      *              * Determinazioni iniziali                         *
      *              *-------------------------------------------------*
       vis-pag-att-010.
      *                  *---------------------------------------------*
      *                  * Si/No pagina precedente alla pagina attuale *
      *                  *---------------------------------------------*
           if        w-tmn-ele-pag        >    1
                     move  "S"            to   w-vis-pag-snx-pre
           else      move  "N"            to   w-vis-pag-snx-pre      .
       vis-pag-att-020.
      *                  *---------------------------------------------*
      *                  * Si/No pagina seguente alla pagina attuale   *
      *                  *---------------------------------------------*
           if        w-tmn-ele-pag        <    w-tmn-ele-npt
                     move  "S"            to   w-vis-pag-snx-seg
           else      move  "N"            to   w-vis-pag-snx-seg      .
       vis-pag-att-030.
      *                  *---------------------------------------------*
      *                  * Indice del primo elemento visualizzato nel- *
      *                  * la pagina attuale                           *
      *                  *---------------------------------------------*
           move      w-tmn-ele-pag        to   w-vis-pag-inx-pel      .
           subtract  1                    from w-vis-pag-inx-pel      .
           multiply  w-tmn-ele-nep        by   w-vis-pag-inx-pel      .
           add       1                    to   w-vis-pag-inx-pel      .
       vis-pag-att-040.
      *                  *---------------------------------------------*
      *                  * Indice dell'ultimo elemento visualizzato    *
      *                  * nella pagina attuale                        *
      *                  *---------------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-uel      .
           add       w-tmn-ele-nep        to   w-vis-pag-inx-uel      .
           subtract  1                    from w-vis-pag-inx-uel      .
           if        w-vis-pag-inx-uel    >    w-tmn-ele-num
                     move  w-tmn-ele-num  to   w-vis-pag-inx-uel      .
       vis-pag-att-050.
      *              *-------------------------------------------------*
      *              * Abblencamento area totale occupata              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      w-vis-pag-lin-000    to   v-lto                  .
           add       w-tmn-ele-nep        to   v-lto                  .
           add       1                    to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-pag-att-100.
      *              *-------------------------------------------------*
      *              * Linea iniziale per il numero pagina             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste una sola pagina non si visualizza *
      *                  * il numero pagina                            *
      *                  *---------------------------------------------*
           if        w-tmn-ele-npt        not  > 1
                     go to vis-pag-att-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pagina"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero pagina               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      08                   to   v-pos                  .
           move      w-tmn-ele-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-pag-att-200.
      *              *-------------------------------------------------*
      *              * Linee effettive di tipi manutenzione            *
      *              *-------------------------------------------------*
       vis-pag-att-300.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice per scansione su e- *
      *                  * lementi da visualizzare                     *
      *                  *---------------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
       vis-pag-att-400.
      *                  *---------------------------------------------*
      *                  * Trattamento elemento in esame               *
      *                  *---------------------------------------------*
       vis-pag-att-410.
      *                      *-----------------------------------------*
      *                      * Determinazione linea a video per ele-   *
      *                      * mento in esame                          *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-tbl    to   w-vis-pag-lin-acv      .
           subtract  w-vis-pag-inx-pel    from w-vis-pag-lin-acv      .
           add       w-vis-pag-lin-000    to   w-vis-pag-lin-acv      .
           add       1                    to   w-vis-pag-lin-acv      .
       vis-pag-att-420.
      *                      *-----------------------------------------*
      *                      * Se codice numerico per il tipo manuten- *
      *                      * zione a zero : no visualizzazione       *
      *                      *-----------------------------------------*
           if        w-tmn-num-tmn
                    (w-vis-pag-inx-tbl)   =    zero
                     go to vis-pag-att-500.
       vis-pag-att-430.
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice numerico per il  *
      *                      * tipo manutenzione                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      16                   to   v-pos                  .
           move      w-tmn-num-tmn
                    (w-vis-pag-inx-tbl)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-440.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal '-'             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-450.
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * di manutenzione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-tmn-des-tmn
                    (w-vis-pag-inx-tbl)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-500.
      *                  *---------------------------------------------*
      *                  * Incremento indice per scansione su elementi *
      *                  * da visualizzare                             *
      *                  *---------------------------------------------*
           add       1                    to   w-vis-pag-inx-tbl      .
      *                  *---------------------------------------------*
      *                  * Se oltre ultimo elemento da visualizzare    *
      *                  * per la pagina si va' alla conclusione, al-  *
      *                  * trimenti si ricicla al trattamento dell'e-  *
      *                  * lemento in esame                            *
      *                  *---------------------------------------------*
           if        w-vis-pag-inx-tbl    >    w-vis-pag-inx-uel
                     go to vis-pag-att-900
           else      go to vis-pag-att-400.
       vis-pag-att-900.
      *              *-------------------------------------------------*
      *              * Linea finale per il literal 'segue'             *
      *              *-------------------------------------------------*
       vis-pag-att-905.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se esiste o no una pa- *
      *                  * gina seguente alla pagina attuale           *
      *                  *---------------------------------------------*
           if        w-vis-pag-snx-seg    =    "S"
                     go to vis-pag-att-910
           else      go to vis-pag-att-915.
       vis-pag-att-910.
      *                  *---------------------------------------------*
      *                  * Se esiste una pagina seguente alla pagina   *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'segue'         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           add       w-tmn-ele-nep        to   v-lin                  .
           add       1                    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Segue"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-915.
      *                  *---------------------------------------------*
      *                  * Se non esiste una pagina seguente alla pa-  *
      *                  * gina attuale                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del numero di pa-  *
      *                      * gine totali                             *
      *                      *-----------------------------------------*
           if        w-tmn-ele-npt        >    1
                     go to vis-pag-att-925
           else      go to vis-pag-att-920.
       vis-pag-att-920.
      *                      *-----------------------------------------*
      *                      * Se numero di pagine totali non superio- *
      *                      * re a 1                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-925.
      *                      *-----------------------------------------*
      *                      * Se numero di pagine totali superiore a  *
      *                      * 1                                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione literal 'fine '     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fine "              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-999.
           exit.

      *    *===========================================================*
      *    * Richiamo del programma di esecuzione                      *
      *    *-----------------------------------------------------------*
       ric-pgm-exe-000.
      *              *-------------------------------------------------*
      *              * Se nome overlay a spaces : uscita               *
      *              *-------------------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to ric-pgm-exe-999.
       ric-pgm-exe-300.
      *              *-------------------------------------------------*
      *              * Eventuale flag di visualizzazione forzata       *
      *              *-------------------------------------------------*
           move      w-cnt-mfu-vis-sgr    to   w-ovy-exe-vis          .
       ric-pgm-exe-400.
      *              *-------------------------------------------------*
      *              * Scrittura della variabile di i.p.c. relativa    *
      *              * al tipo di chiamante per il sottoprogramma, se  *
      *              * il main-program o un altro sottoprogramma       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tdc-mos"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
      *
           if        w-ipc-sub-pgm-ind    =    "CS"
                     move  "S"            to   s-alf
           else      move  "M"            to   s-alf                  .
      *
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       ric-pgm-exe-600.
      *              *-------------------------------------------------*
      *              * Richiamo programma                              *
      *              *-------------------------------------------------*
           add       1                    to   w-ovy-exe-inx          .
           move      w-ovy-exe-pos        to   w-ovy-exe-spv
                                              (w-ovy-exe-inx)         .
           call      w-ovy-exe-pat       using i-ide
                                               w-ovy-exe
                                               w-tmn
                                               w-spg
                                               w-prs                  .
      *              *-------------------------------------------------*
      *              * Cancellazione programma                         *
      *              *-------------------------------------------------*
           move      w-ovy-exe-spv
                    (w-ovy-exe-inx)       to   w-ovy-exe-pos          .
           cancel    w-ovy-exe-pat                                    .
           subtract  1                    from w-ovy-exe-inx          .
       ric-pgm-exe-999.
           exit.

