       Identification Division.
       Program-Id.                                 piic3000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    iic                 *
      *                                Settore:    mov                 *
      *                                   Fase:    iic300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 19/01/93    *
      *                       Versione attuale:    NdK del 26/11/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Movimenti per gestione Iva intracomunitaria *
      *                                                                *
      *                    Main                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Funzioni :                                                     *
      *                                                                *
      * La fase iic300, nella sua globalita', esegue le funzioni elen- *
      * cate nella tabella seguente :                                  *
      *                                                                *
      *                                                                *
      *   Codice                                                       *
      *    tipo                                                        *
      * operazione         Descrizione per il tipo operazione          *
      * ----------  -------------------------------------------------- *
      *                                                                *
      *     Overlay piic300a                                           *
      *                                                                *
      *   0001      Registrazione acquisto intracomunitario            *
      *                                                                *
      *                                                                *
      *     Overlay piic300b                                           *
      *                                                                *
      *   0002      Registrazione cessione intracomunitaria            *
      *                                                                *
      *                                                                *
      *     Overlay piic300c                                           *
      *                                                                *
      *   0011      Rettifica ad acquisto intracomunitario             *
      *                                                                *
      *                                                                *
      *     Overlay piic300d                                           *
      *                                                                *
      *   0012      Rettifica a cessione intracomunitaria              *
      *                                                                *
      *                                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Aggiornamenti contabili :                                      *
      *                                                                *
      * Nessun tipo operazione esegue aggiornamenti contabili          *
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
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "iic"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "iic300"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "piic3000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "GESTIONE MOVIMENTI IVA INTRACOMUNITARIA "       .

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
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-tus.
      *            *---------------------------------------------------*
      *            * Da accettazione campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-key      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-key-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per personalizzazioni                           *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per gestione Iva intracomunitaria   *
      *        *-------------------------------------------------------*
           05  w-prs-iic.
      *            *---------------------------------------------------*
      *            * Data registrazione minima per movimenti relativi  *
      *            * all'Iva intracomunitaria                          *
      *            *---------------------------------------------------*
               10  w-prs-iic-dtr-min      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice provincia italiana di default              *
      *            *---------------------------------------------------*
               10  w-prs-iic-def-prv      pic  x(03)                  .

      *    *===========================================================*
      *    * Work-area per tipi operazione                             *
      *    *-----------------------------------------------------------*
       01  w-top.
      *        *-------------------------------------------------------*
      *        * Tabella tipi operazione e dati ad essi associati      *
      *        *-------------------------------------------------------*
           05  w-top-tbl-top.
      *            *---------------------------------------------------*
      *            * Indice per puntamento su elemento in tabella      *
      *            *---------------------------------------------------*
               10  w-top-ele-inx          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-top-ele-num          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-top-ele-max          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Elementi per tipi operazione                      *
      *            *---------------------------------------------------*
               10  w-top-ele-top occurs 30.
      *                *-----------------------------------------------*
      *                * Codice tipo operazione                        *
      *                *-----------------------------------------------*
                   15  w-top-cod-top      pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Codice mnemonico                              *
      *                *-----------------------------------------------*
                   15  w-top-cod-mne      pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Descrizione                                   *
      *                *-----------------------------------------------*
                   15  w-top-des-top      pic  x(50)                  .
      *                *-----------------------------------------------*
      *                * Flag 1                                        *
      *                *-----------------------------------------------*
                   15  w-top-f01-top      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Flag 2                                        *
      *                *-----------------------------------------------*
                   15  w-top-f02-top      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Flag 3                                        *
      *                *-----------------------------------------------*
                   15  w-top-f03-top      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Flag 4                                        *
      *                *-----------------------------------------------*
                   15  w-top-f04-top      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Flag 5                                        *
      *                *-----------------------------------------------*
                   15  w-top-f05-top      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per valori di defaults generali                 *
      *    *-----------------------------------------------------------*
       01  w-def.
      *        *-------------------------------------------------------*
      *        * Data di registrazione di default                      *
      *        *-------------------------------------------------------*
           05  w-def-dat-reg              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice tipo operazione di default                     *
      *        *-------------------------------------------------------*
           05  w-def-tip-ope              pic  9(04)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione campi accettazione campi    *
      *    * chiave trattati dal main                                  *
      *    *-----------------------------------------------------------*
       01  w-key.
      *        *-------------------------------------------------------*
      *        * Tipo funzione richiesta al sottoprogramma richiamato  *
      *        * dal main-program; il significato varia da sottopro-   *
      *        * gramma a sottoprogramma; attualmente viene passata a  *
      *        * spaces a tutti i sottoprogrammi                       *
      *        *-------------------------------------------------------*
           05  w-key-tip-fun              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Data di registrazione                                 *
      *        *-------------------------------------------------------*
           05  w-key-dat-reg              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice tipo operazione                                *
      *        *-------------------------------------------------------*
           05  w-key-tip-ope              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per codice tipo operazione                *
      *        *-------------------------------------------------------*
           05  w-key-tip-ope-des          pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo se selezionato in chiave            *
      *        *-------------------------------------------------------*
           05  w-key-num-prt              pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di uscita da completamento impostazione campi    *
      *        * chiave effettuato nel sottoprogramma specifico per    *
      *        * l'esecuzione della funzione corrispondente al tipo    *
      *        * operazione                                            *
      *        *                                                       *
      *        * - Spaces : Il sottoprogramma ha provveduto a far im-  *
      *        *            postare i campi residui della chiave, ed   *
      *        *            ha completato il suo lavoro; pertanto il   *
      *        *            main-program deve riciclare all'imposta-   *
      *        *            zione campi chiave, riproponendo l'ultima  *
      *        *            data registrazione e l'ultimo tipo opera-  *
      *        *            zione                                      *
      *        *                                                       *
      *        * - E      : Il sottoprogramma ha raccolto Exit in im-  *
      *        *            postazione dei campi residui della chiave, *
      *        *            pertanto il main deve rientrare all' im-   *
      *        *            postazione del tipo operazione             *
      *        *                                                       *
      *        * - U      : Il sottoprogramma ha raccolto Up in impo-  *
      *        *            stazione dei campi residui della chiave,   *
      *        *            pertanto il main deve rientrare all' im-   *
      *        *            postazione del tipo operazione             *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-key-tus-ack              pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [iit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiit"                          .
      *        *-------------------------------------------------------*
      *        * [iir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiir"                          .
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .
      *        *-------------------------------------------------------*
      *        * [gxp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxp"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [rnnumiic]                                            *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/num/rec/rnnumiic"                       .

      *    *===========================================================*
      *    * Area di comunicazione per gestione catena rig, con buffer *
      *    * dati in grado di ospitare l'area w-rig di ogni overlay    *
      *    *-----------------------------------------------------------*
       01  w-cat-rig.
           05  w-cat-rig-ope              pic  x(02)                  .
           05  w-cat-rig-exs              pic  x(01)                  .
           05  w-cat-rig-num              pic  9(05)                  .
           05  w-cat-rig-cur              pic  9(05)                  .
           05  w-cat-rig-prg              pic  9(05)                  .
           05  w-cat-rig-max              pic  9(05)                  .
           05  w-cat-rig-app              pic  x(01)                  .
           05  w-cat-rig-ins              pic  x(01)                  .
           05  w-cat-rig-new              pic  x(01)                  .
           05  w-cat-rig-lst              pic  x(01)                  .
           05  w-cat-rig-buf.
               10  filler occurs 1024     pic  x(01)                  .

      *    *===========================================================*
      *    * Tabella tipi operazione per gestione Iva intracomunitaria *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.tab"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo operazione per la  *
      *    * gestione movimenti Iva intracomunitaria                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice ISO stato CEE           *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/aisocee0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice nazione                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice provincia               *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdeprv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acl"                   .

      *    *===========================================================*
      *    * Link-area per aggiornamento contabilita' generale, clien- *
      *    * ti, fornitori, iva                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Acc                               *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per accettazione primo campo della chiave        *
      *        *-------------------------------------------------------*
           05  w-acc-acc-uno.
      *            *---------------------------------------------------*
      *            * Selettore primo campo da accettare                *
      *            * - D : Data registrazione                          *
      *            * - T : Tipo operazione                             *
      *            *---------------------------------------------------*
               10  w-acc-acc-uno-sel      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [iit]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-iit.
               10  w-fnd-arc-iit-sns      pic  x(01)                  .
               10  w-fnd-arc-iit-sel      pic  x(01)                  .
               10  w-fnd-arc-iit-mpn.
                   15  w-fnd-arc-iit-dtr  pic  9(07)                  .
                   15  w-fnd-arc-iit-top  pic  9(04)                  .
                   15  w-fnd-arc-iit-prt  pic  9(09)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .

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
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione ciclo per la fase 'iic300'           *
      *              *-------------------------------------------------*
           perform   fas-iic-300-000      thru fas-iic-300-999        .
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
      *              * Caricamento iniziale delle personalizzazioni    *
      *              *-------------------------------------------------*
           perform   loa-prs-iic-000      thru loa-prs-iic-999        .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale dei valori di defaults ge- *
      *              * nerali                                          *
      *              *-------------------------------------------------*
           perform   loa-def-gen-000      thru loa-def-gen-999        .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della tabella dei tipi o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           perform   loa-tbl-top-000      thru loa-tbl-top-999        .
      *              *-------------------------------------------------*
      *              * Open moduli di accettazione                     *
      *              *-------------------------------------------------*
           perform   opn-mod-acc-000      thru opn-mod-acc-999        .
      *              *-------------------------------------------------*
      *              * Open moduli di aggiornamento                    *
      *              *-------------------------------------------------*
           perform   opn-mod-agg-000      thru opn-mod-agg-999        .
      *              *-------------------------------------------------*
      *              * Open files utilizzati dalla fase in tutte le    *
      *              * sue funzioni                                    *
      *              *-------------------------------------------------*
           perform   opn-fls-fas-000      thru opn-fls-fas-999        .
      *              *-------------------------------------------------*
      *              * Determinazione della data di registrazione mi-  *
      *              * nima per i movimenti Iva intracomunitaria       *
      *              *-------------------------------------------------*
           perform   det-dtr-min-000      thru det-dtr-min-999        .
      *              *-------------------------------------------------*
      *              * Determinazione del codice provincia di default  *
      *              *-------------------------------------------------*
           perform   det-def-prv-000      thru det-def-prv-999        .
       pre-exe-pgm-800.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-999.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Programma non eseguibile dall'utente !            
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close moduli di accettazione                    *
      *              *-------------------------------------------------*
           perform   cls-mod-acc-000      thru cls-mod-acc-999        .
      *              *-------------------------------------------------*
      *              * Close moduli di aggiornamento                   *
      *              *-------------------------------------------------*
           perform   cls-mod-agg-000      thru cls-mod-agg-999        .
      *              *-------------------------------------------------*
      *              * Close files utilizzati dalla fase in tutte le   *
      *              * sue funzioni                                    *
      *              *-------------------------------------------------*
           perform   cls-fls-fas-000      thru cls-fls-fas-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ciclo per la fase 'iic300'                     *
      *    *-----------------------------------------------------------*
       fas-iic-300-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Selettore primo campo da accettare della chia-  *
      *              * ve : data di registrazione                      *
      *              *-------------------------------------------------*
           move      "D"                  to   w-acc-acc-uno-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di accettazione chiave   *
      *              *-------------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *              *-------------------------------------------------*
      *              * Preparazione valori pre-cablati per accettazio- *
      *              * ne da valori di default generali                *
      *              *-------------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per impostazione campi  *
      *              * chiave                                          *
      *              *-------------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-iic-300-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare a Spaces per il ti- *
      *              * po uscita da impostazione campi chiave          *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tus-ack          .
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Se tipo uscita "E" : fine programma             *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to fas-iic-300-999.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma per esecuzione del tipo *
      *              * operazione                                      *
      *              *-------------------------------------------------*
           perform   ric-spg-top-000      thru ric-spg-top-999        .
       fas-iic-300-200.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo uscita da im-   *
      *              * postazione campi chiave                         *
      *              *-------------------------------------------------*
           if        w-key-tus-ack        =    spaces
                     go to fas-iic-300-300
           else if   w-key-tus-ack        =    "U"
                     go to fas-iic-300-400
           else if   w-key-tus-ack        =    "E"
                     go to fas-iic-300-500
           else      go to fas-iic-300-300.
       fas-iic-300-300.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da impostazione campi chiave al  *
      *              * valore Spaces                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : data di registrazione              *
      *                  *---------------------------------------------*
           move      "D"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-iic-300-100.
       fas-iic-300-400.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da im postazione campi chiave al *
      *              * valore 'U'                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : tipo operazione                    *
      *                  *---------------------------------------------*
           move      "T"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-iic-300-100.
       fas-iic-300-500.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da im postazione campi chiave al *
      *              * valore 'E'                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : tipo operazione                    *
      *                  *---------------------------------------------*
           move      "T"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-iic-300-100.
       fas-iic-300-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione valori di accettazione chiave             *
      *    *-----------------------------------------------------------*
       nor-val-key-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-tip-ope          .
      *              *-------------------------------------------------*
      *              * Descrizione per codice tipo operazione          *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-ope-des      .
      *              *-------------------------------------------------*
      *              * Numero protocollo se selezionato in chiave      *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-num-prt          .
       nor-val-key-999.
           exit.

      *    *===========================================================*
      *    * Preparazione valori pre-cablati per accettazione campi    *
      *    * chiave, assumendo dai valori di default generali          *
      *    *-----------------------------------------------------------*
       pre-key-def-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      w-def-dat-reg        to   w-key-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           move      w-def-tip-ope        to   w-key-tip-ope          .
      *              *-------------------------------------------------*
      *              * Lettura codice tipo operazione                  *
      *              *-------------------------------------------------*
           move      "C"                  to   w-top-iic-tab-tle      .
           move      w-key-tip-ope        to   w-top-iic-tab-top      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *              *-------------------------------------------------*
      *              * Descrizione tipo operazione                     *
      *              *-------------------------------------------------*
           move      w-top-iic-tab-des    to   w-key-tip-ope-des      .
       pre-key-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per accettazione campi chiave, e  *
      *    * dei valori relativi ai campi pre-cablati per defaults     *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dalla chiave              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      07                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Prompt per data di registrazione                *
      *              *-------------------------------------------------*
           perform   pmt-dat-reg-000      thru pmt-dat-reg-999        .
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *              *-------------------------------------------------*
      *              * Prompt per tipo operazione                      *
      *              *-------------------------------------------------*
           perform   pmt-tip-ope-000      thru pmt-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione, descrizione             *
      *              *-------------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *              *-------------------------------------------------*
      *              * Prompt per numero operazione                    *
      *              *-------------------------------------------------*
           perform   pmt-num-ope-000      thru pmt-num-ope-999        .
      *              *-------------------------------------------------*
      *              * Trattini di separazione                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per data di registrazione                          *
      *    *-----------------------------------------------------------*
       pmt-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per tipo operazione                                *
      *    *-----------------------------------------------------------*
       pmt-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo operazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Prompt per numero operazione                              *
      *    *-----------------------------------------------------------*
       pmt-num-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Nr.  operazione    :                              
      -              "                              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-num-ope-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave                                 *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-key      .
      *              *-------------------------------------------------*
      *              * L'accettazione ha inizio a seconda del valore   *
      *              * del Selettore primo campo da accettare della    *
      *              *  chiave                                         *
      *              *-------------------------------------------------*
           if        w-acc-acc-uno-sel    =    "T"
                     go to acc-key-reg-200
           else      go to acc-key-reg-100.
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazione Data registrazione                 *
      *              *-------------------------------------------------*
           perform   acc-dat-reg-000      thru acc-dat-reg-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *              *-------------------------------------------------*
      *              * Accettazione Tipo operazione                    *
      *              *-------------------------------------------------*
           perform   acc-tip-ope-000      thru acc-tip-ope-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Data registrazione                           *
      *    *-----------------------------------------------------------*
       acc-dat-reg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-key-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-dat-reg-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-key-dat-reg          .
       acc-dat-reg-250.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-reg-400.
      *                  *---------------------------------------------*
      *                  * Find su movimenti per la gestione dell'Iva  *
      *                  * intracomunitaria                            *
      *                  *---------------------------------------------*
           move      "S"                  to   w-fnd-arc-iit-sns      .
           perform   fnd-arc-iit-000      thru fnd-arc-iit-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata : a reimposta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-fnd-arc-iit-sel    not  = spaces
                     go to acc-dat-reg-100.
      *                  *---------------------------------------------*
      *                  * Ripresa data registrazione selezionata      *
      *                  *---------------------------------------------*
           move      w-fnd-arc-iit-dtr    to   w-key-dat-reg          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data registrazione selezio- *
      *                  * nata                                        *
      *                  *---------------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *                  *---------------------------------------------*
      *                  * Ripresa tipo operazione selezionata         *
      *                  *---------------------------------------------*
           move      w-fnd-arc-iit-top    to   w-key-tip-ope          .
      *                  *---------------------------------------------*
      *                  * Lettura codice tipo operazione              *
      *                  *---------------------------------------------*
           move      "C"                  to   w-top-iic-tab-tle      .
           move      w-key-tip-ope        to   w-top-iic-tab-top      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione tipo operazione  *
      *                  *---------------------------------------------*
           move      w-top-iic-tab-des    to   w-key-tip-ope-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione tipo operazione             *
      *                  *---------------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione tipo operazione *
      *                  *---------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *                  *---------------------------------------------*
      *                  * Ripresa numero protocollo selezionato       *
      *                  *---------------------------------------------*
           move      w-fnd-arc-iit-prt    to   w-key-num-prt          .
      *                  *---------------------------------------------*
      *                  * Forzatura simulata del tasto Do             *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-dat-reg-800.
       acc-dat-reg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che la data di registrazione sia di-   *
      *                  * versa da zero                               *
      *                  *---------------------------------------------*
           if        w-key-dat-reg        =    zero
                     go to acc-dat-reg-100.
       acc-dat-reg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-reg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dat-reg-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dat-reg-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Data registrazione                        *
      *    *-----------------------------------------------------------*
       vis-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-key-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Codice tipo operazione                       *
      *    *-----------------------------------------------------------*
       acc-tip-ope-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ope-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-top-iic-ope      .
           move      w-key-tip-ope        to   w-cmn-top-iic-top      .
           move      05                   to   w-cmn-top-iic-lin      .
           move      22                   to   w-cmn-top-iic-pos      .
           move      05                   to   w-cmn-top-iic-dln      .
           move      27                   to   w-cmn-top-iic-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cmn-top-iic-cll-000  thru cmn-top-iic-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cmn-top-iic-foi-000  thru cmn-top-iic-foi-999    .
       acc-tip-ope-110.
           perform   cmn-top-iic-cll-000  thru cmn-top-iic-cll-999    .
           if        w-cmn-top-iic-ope    =    "F+"
                     go to acc-tip-ope-115.
           if        w-cmn-top-iic-ope    =    "AC"
                     go to acc-tip-ope-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tip-ope-115.
           perform   cmn-top-iic-foi-000  thru cmn-top-iic-foi-999    .
           go to     acc-tip-ope-110.
       acc-tip-ope-120.
           move      w-cmn-top-iic-top    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-key-tip-ope          .
       acc-tip-ope-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice tipo operazione              *
      *                  *---------------------------------------------*
           move      "C"                  to   w-top-iic-tab-tle      .
           move      w-key-tip-ope        to   w-top-iic-tab-top      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione tipo operazione  *
      *                  *---------------------------------------------*
           move      w-top-iic-tab-des    to   w-key-tip-ope-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione tipo operazione *
      *                  *---------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se tipo operazione e-  *
      *                  * sistente o non esistente                    *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    =    spaces
                     go to acc-tip-ope-450.
       acc-tip-ope-425.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione non esistente            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tip-ope-100.
       acc-tip-ope-450.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione esistente                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del valore         *
      *                      *-----------------------------------------*
           if        w-key-tip-ope        =    zero
                     go to acc-tip-ope-500
           else      go to acc-tip-ope-550.
       acc-tip-ope-500.
      *                      *-----------------------------------------*
      *                      * Se valore a zero                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non si e' in Up : reimpostazione *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-tip-ope-100.
      *                          *-------------------------------------*
      *                          * Se si e' in Up : continuazione      *
      *                          *-------------------------------------*
           go to     acc-tip-ope-600.
       acc-tip-ope-550.
      *                      *-----------------------------------------*
      *                      * Se valore diverso da zero               *
      *                      *-----------------------------------------*
           go to     acc-tip-ope-600.
       acc-tip-ope-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ope-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-ope-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-ope-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice tipo operazione                    *
      *    *-----------------------------------------------------------*
       vis-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-key-tip-ope        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Descrizione per codice tipo operazione    *
      *    *-----------------------------------------------------------*
       vis-tip-ope-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-key-tip-ope-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tip-ope-des-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do                        *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-100.
      *              *-------------------------------------------------*
      *              * Test su data di registrazione, che non manchi   *
      *              *-------------------------------------------------*
           if        w-key-dat-reg        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Test su tipo operazione, che non manchi         *
      *              *-------------------------------------------------*
           if        w-key-tip-ope        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
       cnt-tdo-key-999.
           exit.
           
      *    *===========================================================*
      *    * Richiamo sottoprogramma per esecuzione Tipo Operazione    *
      *    *-----------------------------------------------------------*
       ric-spg-top-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0001                    *
      *                  *---------------------------------------------*
           if        w-key-tip-ope        =    0001
                     perform   ric-000-001-000
                                          thru ric-000-001-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0002                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0002
                     perform   ric-000-002-000
                                          thru ric-000-002-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0011                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0011
                     perform   ric-000-011-000
                                          thru ric-000-011-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0012                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0012
                     perform   ric-000-012-000
                                          thru ric-000-012-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  non riconosciuto        *
      *                  *---------------------------------------------*
           else      go to ric-spg-top-999.
       ric-spg-top-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0001         *
      *    *-----------------------------------------------------------*
       ric-000-001-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           call      "pgm/iic/prg/obj/piic300a"
                                         using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    "pgm/iic/prg/obj/piic300a"                       .
       ric-000-001-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0002         *
      *    *-----------------------------------------------------------*
       ric-000-002-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           call      "pgm/iic/prg/obj/piic300b"
                                         using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    "pgm/iic/prg/obj/piic300b"                       .
       ric-000-002-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0011         *
      *    *-----------------------------------------------------------*
       ric-000-011-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           call      "pgm/iic/prg/obj/piic300c"
                                         using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    "pgm/iic/prg/obj/piic300c"                       .
       ric-000-011-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0012         *
      *    *-----------------------------------------------------------*
       ric-000-012-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           call      "pgm/iic/prg/obj/piic300d"
                                         using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           cancel    "pgm/iic/prg/obj/piic300d"                       .
       ric-000-012-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale delle personalizzazioni              *
      *    *-----------------------------------------------------------*
       loa-prs-iic-000.
       loa-prs-iic-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale dei valori di defaults generali      *
      *    *-----------------------------------------------------------*
       loa-def-gen-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-def-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione di default               *
      *              *-------------------------------------------------*
           move      zero                 to   w-def-tip-ope          .
       loa-def-gen-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della tabella dei tipi operazione    *
      *    *-----------------------------------------------------------*
       loa-tbl-top-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale della tabella          *
      *              *-------------------------------------------------*
       loa-tbl-top-010.
      *                  *---------------------------------------------*
      *                  * Massimo numero di elementi in tabella       *
      *                  *---------------------------------------------*
           move      30                   to   w-top-ele-max          .
       loa-tbl-top-015.
      *                  *---------------------------------------------*
      *                  * Normalizzazione numero effettivo di elemen- *
      *                  * ti in tabella                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-num          .
       loa-tbl-top-020.
      *                  *---------------------------------------------*
      *                  * Normalizzazione degli elementi in tabella   *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-inx          .
       loa-tbl-top-022.
           add       1                    to   w-top-ele-inx          .
           if        w-top-ele-inx        >    w-top-ele-max
                     go to loa-tbl-top-030.
           move      zero                 to   w-top-cod-top
                                              (w-top-ele-inx)         .
           move      spaces               to   w-top-cod-mne
                                              (w-top-ele-inx)         .
           move      spaces               to   w-top-des-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f01-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f02-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f03-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f04-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f05-top
                                              (w-top-ele-inx)         .
           go to     loa-tbl-top-022.
       loa-tbl-top-030.
      *                  *---------------------------------------------*
      *                  * Fine normalizzazione tabella                *
      *                  *---------------------------------------------*
           go to     loa-tbl-top-100.
       loa-tbl-top-100.
      *              *-------------------------------------------------*
      *              * Caricamento della tabella                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su tabella in memoria tipi operazione *
      *                  * Iva intracomunitaria                        *
      *                  *---------------------------------------------*
           move      "S"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to loa-tbl-top-180.
       loa-tbl-top-120.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale tabella in memoria tipi *
      *                  * operazione Iva intracomunitaria             *
      *                  *---------------------------------------------*
           move      "N"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to loa-tbl-top-180.
       loa-tbl-top-140.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento in tabella         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento numero elementi memorizzati  *
      *                      *-----------------------------------------*
           add       1                    to   w-top-ele-num          .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati associati al tipo  *
      *                      * operazione                              *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-top    to   w-top-cod-top
                                              (w-top-ele-num)         .
           move      w-top-iic-tab-mne    to   w-top-cod-mne
                                              (w-top-ele-num)         .
           move      w-top-iic-tab-des    to   w-top-des-top
                                              (w-top-ele-num)         .
           move      w-top-iic-tab-f01    to   w-top-f01-top
                                              (w-top-ele-num)         .
           move      w-top-iic-tab-f02    to   w-top-f02-top
                                              (w-top-ele-num)         .
           move      w-top-iic-tab-f03    to   w-top-f03-top
                                              (w-top-ele-num)         .
           move      w-top-iic-tab-f04    to   w-top-f04-top
                                              (w-top-ele-num)         .
           move      w-top-iic-tab-f05    to   w-top-f05-top
                                              (w-top-ele-num)         .
       loa-tbl-top-160.
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura elemento successivo       *
      *                  *---------------------------------------------*
           go to     loa-tbl-top-120.
       loa-tbl-top-180.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     loa-tbl-top-999.
       loa-tbl-top-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di accettazione                               *
      *    *-----------------------------------------------------------*
       opn-mod-acc-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo operazione per    *
      *              * gestione Iva intracomunitaria                   *
      *              *-------------------------------------------------*
           perform   cmn-top-iic-opn-000  thru cmn-top-iic-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice ISO stato CEE   *
      *              *-------------------------------------------------*
           perform   cde-iso-cee-opn-000  thru cde-iso-cee-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nazione         *
      *              *-------------------------------------------------*
           perform   cod-des-naz-opn-000  thru cod-des-naz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice provincia       *
      *              *-------------------------------------------------*
           perform   cod-des-prv-opn-000  thru cod-des-prv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente         *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-opn-000  thru cod-mne-cli-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore       *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-opn-000  thru cod-mne-fnt-opn-999    .
       opn-mod-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mod-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo operazione per   *
      *              * gestione Iva intracomunitaria                   *
      *              *-------------------------------------------------*
           perform   cmn-top-iic-cls-000  thru cmn-top-iic-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice ISO stato CEE  *
      *              *-------------------------------------------------*
           perform   cde-iso-cee-cls-000  thru cde-iso-cee-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nazione        *
      *              *-------------------------------------------------*
           perform   cod-des-naz-cls-000  thru cod-des-naz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice provincia      *
      *              *-------------------------------------------------*
           perform   cod-des-prv-cls-000  thru cod-des-prv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente        *
      *              *-------------------------------------------------*
           perform   cod-mne-cli-cls-000  thru cod-mne-cli-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore      *
      *              *-------------------------------------------------*
           perform   cod-mne-fnt-cls-000  thru cod-mne-fnt-cls-999    .
       cls-mod-acc-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di aggiornamento                              *
      *    *-----------------------------------------------------------*
       opn-mod-agg-000.
      *              *-------------------------------------------------*
      *              * Open modulo aggiornamento contabilita' genera-  *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-opn-000  thru mdl-agg-cge-opn-999    .
       opn-mod-agg-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di aggiornamento                             *
      *    *-----------------------------------------------------------*
       cls-mod-agg-000.
      *              *-------------------------------------------------*
      *              * Close modulo aggiornamento contabilita' genera- *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-cls-000  thru mdl-agg-cge-cls-999    .
       cls-mod-agg-999.
           exit.

      *    *===========================================================*
      *    * Open files utilizzati dalla fase in tutte le sue funzioni *
      *    *-----------------------------------------------------------*
       opn-fls-fas-000.
      *              *-------------------------------------------------*
      *              * Open sottoprogramma gestione catena righe       *
      *              *-------------------------------------------------*
           move      "OP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *              *-------------------------------------------------*
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [numiic]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/num/ioc/obj/innumiic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-iic             .
       opn-fls-fas-999.
           exit.

      *    *===========================================================*
      *    * Close files utilizzati dalla fase in tutte le sue fun-    *
      *    * zioni                                                     *
      *    *-----------------------------------------------------------*
       cls-fls-fas-000.
      *              *-------------------------------------------------*
      *              * Close sottoprogramma gestione catena righe con  *
      *              * cancellazione del sottoprogramma stesso         *
      *              *-------------------------------------------------*
           move      "CL"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           perform   cnc-sub-cat-000      thru cnc-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *              *-------------------------------------------------*
      *              * [mgt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * [mgr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [numiic]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/num/ioc/obj/innumiic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-iic             .
       cls-fls-fas-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           call      "pgm/iic/prg/obj/piic3002"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-sub-cat-000.
           cancel    "pgm/iic/prg/obj/piic3002"                       .
       cnc-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Determinazione della data di registrazione minima per le  *
      *    * operazioni Iva intracomunitaria                           *
      *    *-----------------------------------------------------------*
       det-dtr-min-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma per aggiornamenti con-  *
      *              * tabili per la determinazione della data di re-  *
      *              * gistrazione minima per la contabilita' genera-  *
      *              * le, clienti, fornitori, ed iva                  *
      *              *-------------------------------------------------*
           move      "DR"                 to   l-cge-300-tip-ope      .
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
      *              *-------------------------------------------------*
      *              * Memorizzazione della data di registrazione mi-  *
      *              * nima appena determinata                         *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   w-prs-iic-dtr-min      .
       det-dtr-min-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice provincia di default            *
      *    *-----------------------------------------------------------*
       det-def-prv-000.
      *              *-------------------------------------------------*
      *              * Lettura della referenza corrispondente          *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/iic/mov/iic300[cod-prv]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se referenza esistente si pone il default, al-  *
      *              * trimenti si pone il default a Spaces            *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-iic-def-prv
           else      move  spaces         to   w-prs-iic-def-prv      .
       det-def-prv-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [iit]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-iit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      "N"                  to   w-fnd-arc-iit-sel      .
       fnd-arc-iit-050.
      *              *-------------------------------------------------*
      *              * Se programma di interrogazione gia' attivo :    *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "piic3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to fnd-arc-iit-999.
       fnd-arc-iit-100.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per il  *
      *              * livello successivo per l'ammissibilita' del ta- *
      *              * sto Slct                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non ammesso : no scrittura               *
      *                  *---------------------------------------------*
           if        w-fnd-arc-iit-sns    not  = "S"
                     go to fnd-arc-iit-150.
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-iit-150.
      *              *-------------------------------------------------*
      *              * Richiamo del programma di interrogazione        *
      *              *-------------------------------------------------*
           move      "pgm/iic/prg/obj/piic3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
       fnd-arc-iit-200.
      *              *-------------------------------------------------*
      *              * Cancellazione del programma di interrogazione   *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       fnd-arc-iit-250.
      *              *-------------------------------------------------*
      *              * Determinazione selezione avvenuta               *
      *              *-------------------------------------------------*
       fnd-arc-iit-255.
      *                  *---------------------------------------------*
      *                  * Se non era ammessa la selezioone : uscita   *
      *                  *---------------------------------------------*
           if        w-fnd-arc-iit-sns    not  = "S"
                     go to fnd-arc-iit-999.
       fnd-arc-iit-260.
      *                  *---------------------------------------------*
      *                  * Lettura variabile di i.p.c. 'dtp-iic' dal   *
      *                  * livello successivo per data registrazione,  *
      *                  * tipo operazione, e numero protocollo sele-  *
      *                  * zionati                                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dtp-iic"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to fnd-arc-iit-265
           else      go to fnd-arc-iit-270.
       fnd-arc-iit-265.
      *                  *---------------------------------------------*
      *                  * Se variabile esistente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di selezione avvenuta              *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-arc-iit-sel      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori selezionati per : *
      *                      *  - Data registrazione                   *
      *                      *  - Tipo operazione                      *
      *                      *  - Numero protocollo                    *
      *                      *-----------------------------------------*
           move      s-alf                to   w-fnd-arc-iit-mpn      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-iit-999.
       fnd-arc-iit-270.
      *                  *---------------------------------------------*
      *                  * Se variabile non esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-iit-999.
       fnd-arc-iit-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
       box-msg-err-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Lettura tabella tipi operazione per gestione Iva intra-   *
      *    * comunitaria                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.rtn"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice operazione per  *
      *    * la gestione movimenti Iva intracomunitari                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice ISO stato CEE   *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/aisocee0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nazione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdenaz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice provincia       *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acdeprv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice cliente         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncli0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnfnt0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per modulo aggiornamento contabilita' genera- *
      *    * le, clienti, fornitori, iva                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgs"                   .
