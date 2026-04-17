       Identification Division.
       Program-Id.                                 pcge5500           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    for                 *
      *                                   Fase:    cge550              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/07/94    *
      *                       Ultima revisione:    NdK del 24/11/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Manutenzioni anagrafiche fornitori contabi- *
      *                    li                                          *
      *                                                                *
      *                    Main                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * La fase cge550, nella sua globalita', esegue i tipi di manu-   *
      * tenzione elencati nella tabella seguente :                     *
      *                                                                *
      *                                                                *
      *   Codice                                                       *
      *    tipo                                                        *
      *  manutenz.        Descrizione per il tipo manutenzione         *
      * ----------  -------------------------------------------------- *
      *                                                                *
      * MANGEO      Manutenzione Localizzazione geografica             *
      *                                                                *
      * MANNTI      Manutenzione Numeri telefonici ed interlocutore    *
      *                                                                *
      * MANCON      Manutenzione Dati per la contabilita'              *
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
      *    * Link-area comune per programmi della serie pcge5500       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge5500.pgl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *              * Esecuzione ciclo per la fase 'cge550'           *
      *              *-------------------------------------------------*
           perform   fas-cge-550-000      thru fas-cge-550-999        .
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
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della tabella dei tipi in- *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           perform   loa-tbl-tmn-000      thru loa-tbl-tmn-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Personalizzazione relativa al tipo di ordi- *
      *                  * namento anagrafiche fornitori               *
      *                  *---------------------------------------------*
           perform   prs-tip-ord-000      thru prs-tip-ord-999        .
      *                  *---------------------------------------------*
      *                  * Personalizzazione relativa al record for-   *
      *                  * nitore                                      *
      *                  *---------------------------------------------*
           perform   prs-rec-fnt-000      thru prs-rec-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Numero livelli del piano dei conti          *
      *                  *---------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
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
           move      "pgm/cge/for/cge550[tip-ord]"
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
      *    * Lettura delle personalizzazioni relative al record for-   *
      *    * nitore                                                    *
      *    *-----------------------------------------------------------*
       prs-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni relative al record    *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[rec-fnt]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rec-fnt
           else      move  spaces         to   w-prs-rec-fnt          .
           if        w-prs-rec-fnt-mco    not  numeric
                     move  zero           to   w-prs-rec-fnt-mco      .
           if        w-prs-rec-fnt-mco    =    zero
                     move  9999999        to   w-prs-rec-fnt-mco      .
           if        w-prs-rec-fnt-fco    not  = "A"
                     move  "M"            to   w-prs-rec-fnt-fco      .
           if        w-prs-rec-fnt-omn    not  = "O"
                     move  "N"            to   w-prs-rec-fnt-omn      .
           if        w-prs-rec-fnt-umn    not  = "U"
                     move  "N"            to   w-prs-rec-fnt-umn      .
           if        w-prs-rec-fnt-mmn    not  = "N"
                     move  "M"            to   w-prs-rec-fnt-mmn      .
           if        w-prs-rec-fnt-osc    not  = "N"
                     move  "N"            to   w-prs-rec-fnt-osc      .
           if        w-prs-rec-fnt-msc    not  = "N"
                     move  "M"            to   w-prs-rec-fnt-msc      .
       prs-rec-fnt-999.
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
      -              " MANGEO     pcge550a  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Numeri telefonici ed interlocutore                
      -              " MANNTI     pcge550b  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Dati per la contabilita'                          
      -              " MANCON     pcge550c  "
                                          to   w-tmn-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
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
      *    * Esecuzione ciclo per la fase 'cge550'                     *
      *    *-----------------------------------------------------------*
       fas-cge-550-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-cge-550-025.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
       fas-cge-550-050.
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
       fas-cge-550-075.
      *              *-------------------------------------------------*
      *              * Prompts per impostazione tipo manutenzione      *
      *              *-------------------------------------------------*
       fas-cge-550-077.
      *                  *---------------------------------------------*
      *                  * Prompt effettivo per tipo manutenzione      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo manutenzione   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-cge-550-079.
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
       fas-cge-550-081.
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
       fas-cge-550-083.
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
       fas-cge-550-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina attuale                  *
      *              *-------------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
       fas-cge-550-125.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-cge-550-200.
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
                     go to fas-cge-550-300
           else if   v-key                =    "PRSC"
                     go to fas-cge-550-400
           else if   v-key                =    "NXSC"
                     go to fas-cge-550-450
           else if   v-key                =    "DOWN"
                     go to fas-cge-550-500
           else if   v-key                =    "EXIT"
                     go to fas-cge-550-250
           else      go to fas-cge-550-200.
       fas-cge-550-250.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fas-cge-550-999.
       fas-cge-550-300.
      *              *-------------------------------------------------*
      *              * Se Return o Do                                  *
      *              *-------------------------------------------------*
       fas-cge-550-310.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           move      v-num                to   w-key-num-man          .
       fas-cge-550-320.
      *                  *---------------------------------------------*
      *                  * Ricerca codice numerico impostato in ta-    *
      *                  * bella dei tipi manutenzione, e deviazione   *
      *                  * a seconda dell'esito della ricerca          *
      *                  *---------------------------------------------*
           if        w-key-num-man        =    zero
                     go to fas-cge-550-330.
           move      zero                 to   w-tmn-ele-inx          .
       fas-cge-550-322.
           add       1                    to   w-tmn-ele-inx          .
           if        w-tmn-ele-inx        >    w-tmn-ele-num
                     go to fas-cge-550-340.
           if        w-tmn-num-tmn
                    (w-tmn-ele-inx)       =    w-key-num-man
                     go to fas-cge-550-350
           else      go to fas-cge-550-322.
       fas-cge-550-330.
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
           go to     fas-cge-550-200.
       fas-cge-550-340.
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
           go to     fas-cge-550-200.
       fas-cge-550-350.
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
                     go to fas-cge-550-200.
      *                      *-----------------------------------------*
      *                      * Altrimenti si va' al richiamo effettivo *
      *                      * del programma di esecuzione             *
      *                      *-----------------------------------------*
           go to     fas-cge-550-700.
       fas-cge-550-400.
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
           go to     fas-cge-550-200.
       fas-cge-550-450.
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
           go to     fas-cge-550-200.
       fas-cge-550-500.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
       fas-cge-550-525.
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
       fas-cge-550-550.
      *                  *---------------------------------------------*
      *                  * Accettazione direttamente da corpo video    *
      *                  *---------------------------------------------*
       fas-cge-550-555.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice per scansione   *
      *                      * su elementi visualizzati                *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
       fas-cge-550-560.
      *                      *-----------------------------------------*
      *                      * Determinazione linea a video per ele-   *
      *                      * mento in esame                          *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-tbl    to   w-vis-pag-lin-acv      .
           subtract  w-vis-pag-inx-pel    from w-vis-pag-lin-acv      .
           add       w-vis-pag-lin-000    to   w-vis-pag-lin-acv      .
           add       1                    to   w-vis-pag-lin-acv      .
       fas-cge-550-570.
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
       fas-cge-550-580.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tasto di fun-  *
      *                      * zione usato                             *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to fas-cge-550-630
           else if   v-key                =    "PRSC"
                     go to fas-cge-550-600
           else if   v-key                =    "NXSC"
                     go to fas-cge-550-610
           else if   v-key                =    "UP  "
                     go to fas-cge-550-620
           else if   v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to fas-cge-550-650
           else if   v-key                =    "EXIT"
                     go to fas-cge-550-660
           else      go to fas-cge-550-570.
       fas-cge-550-600.
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
           go to     fas-cge-550-560.
       fas-cge-550-610.
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
           go to     fas-cge-550-560.
       fas-cge-550-620.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
       fas-cge-550-622.
      *                          *-------------------------------------*
      *                          * Se si e' al primo elemento in asso- *
      *                          * luto della tabella si ricicla ad    *
      *                          * accettazione codice numerico per il *
      *                          * tipo manutenzione                   *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    1
                     go to fas-cge-550-200.
       fas-cge-550-624.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-pel
                     go to fas-cge-550-626
           else      go to fas-cge-550-628.
       fas-cge-550-626.
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
           go to     fas-cge-550-560.
       fas-cge-550-628.
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
           go to     fas-cge-550-560.
       fas-cge-550-630.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
       fas-cge-550-632.
      *                          *-------------------------------------*
      *                          * Se si e' all'ultimo elemento in as- *
      *                          * soluto della tabella si ricicla ad  *
      *                          * accettazione tasto di funzione      *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-tmn-ele-num
                     go to fas-cge-550-570.
       fas-cge-550-634.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-uel
                     go to fas-cge-550-636
           else      go to fas-cge-550-638.
       fas-cge-550-636.
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
           go to     fas-cge-550-560.
       fas-cge-550-638.
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
           go to     fas-cge-550-560.
       fas-cge-550-650.
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
                     go to fas-cge-550-570.
      *                          *-------------------------------------*
      *                          * Altrimenti si va' al richiamo ef-   *
      *                          * fettivo del programma di esecuzione *
      *                          *-------------------------------------*
           go to     fas-cge-550-700.
       fas-cge-550-660.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ritorno all'accettazione del codice *
      *                          * numerico per il tipo manutenzione   *
      *                          *-------------------------------------*
           go to     fas-cge-550-200.
       fas-cge-550-700.
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
                     go to fas-cge-550-725
           else      go to fas-cge-550-750.
       fas-cge-550-725.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fas-cge-550-999.
       fas-cge-550-750.
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
           go to     fas-cge-550-200.
       fas-cge-550-999.
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
           move      "M"                  to   s-alf                  .
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

