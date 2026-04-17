       Identification Division.
       Program-Id.                                 pods300h           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    mov                 *
      *                                   Fase:    ods300h             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 31/10/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Accettazione righe da lettore bar-code      *
      *                                                                *
      *                    Richiamata in ods300 da funzione 'PF2'      *
      *                                                                *
      *                    ___ DA IMPLEMENTARE ___                     *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-alb-rdb-tip-ope = "OP"                       *
      *                 l-alb-rdb-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-alb-rdb-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-alb-rdb-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-alb-rdb-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "RB" - Accettazione dati identificativi file esterno           *
      *                                                                *
      *        Input  : l-alb-rdb-tip-ope = "RB"                       *
      *                                                                *
      *        Output : l-alb-rdb-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "SC" - Saldaconto per selezione righe                          *
      *                                                                *
      *        Input  : l-alb-rdb-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-alb-rdb-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe bolla selezionate in catena           *
      *                                                                *
      *        Input  : l-alb-rdb-tip-ope = "CC"                       *
      *                                                                *
      *        Output : nessuno                                        *
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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files area 'fat'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .

      *    *===========================================================*
      *    * Record files area 'cge'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .

      *    *===========================================================*
      *    * Record files area 'dcp'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .

      *    *===========================================================*
      *    * Record files area 'dpm'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .

      *    *===========================================================*
      *    * Record files area 'dps'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work-area generica per il programma                       *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-gen-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Numero protocollo bolla in corso di trattamento       *
      *        *-------------------------------------------------------*
           05  w-gen-prt-bol              pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per campi di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  w-acc-cod-dpz              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo                                     *
      *        *-------------------------------------------------------*
           05  w-acc-num-prt              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento bolle clienti                          *
      *        *-------------------------------------------------------*
           05  w-acc-cod-tmb              pic  x(05)                  .
           05  w-acc-cod-tmb-des          pic  x(30)                  .
           05  w-acc-cod-tmb-sgl          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Data documento                                        *
      *        *-------------------------------------------------------*
           05  w-acc-dat-doc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero documento                                      *
      *        *-------------------------------------------------------*
           05  w-acc-num-doc              pic  9(11)                  .
           05  w-acc-num-doc-r redefines
               w-acc-num-doc.
               10  w-acc-num-doc-saa      pic  9(03)                  .
               10  w-acc-num-doc-dpz      pic  9(02)                  .
               10  w-acc-num-doc-prg      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ricalcolo da effettuare                       *
      *        *  - Spaces : No                                        *
      *        *  - X      : Si                                        *
      *        *-------------------------------------------------------*
           05  w-acc-tip-rcl.
      *            *---------------------------------------------------*
      *            * Prezzi                                            *
      *            *---------------------------------------------------*
               10  w-acc-tip-rcl-prz      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Sconti                                            *
      *            *---------------------------------------------------*
               10  w-acc-tip-rcl-sco      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Provvigioni                                       *
      *            *---------------------------------------------------*
               10  w-acc-tip-rcl-pvg      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Quantita' di vendita                                  *
      *        *-------------------------------------------------------*
           05  w-sav-qta-ven              pic s9(10)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Codice prodotto numerico                              *
      *        *-------------------------------------------------------*
           05  w-sav-num-pro              pic  9(07)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ricalcolo da effettuare               *
      *        *-------------------------------------------------------*
           05  w-exp-tip-rcl.
               10  w-exp-tip-rcl-num      pic  9(02)       value 3    .
               10  w-exp-tip-rcl-lun      pic  9(02)       value 15   .
               10  w-exp-tip-rcl-tbl.
                   15  filler             pic  x(15) value
                            "[ ] Prezzi     "                         .
                   15  filler             pic  x(15) value
                            "[ ] Sconti     "                         .
                   15  filler             pic  x(15) value
                            "[ ] Provvigioni"                         .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe bolla da accettare    *
      *    *-----------------------------------------------------------*
       01  w-brb.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-brb-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-brb-max-ele              pic  9(05) value 800        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-brb-tbl.
               10  w-brb-sng-ele occurs 800.
                   15  w-brb-num-prg      pic  9(05)       comp-3     .
                   15  w-brb-tip-rig      pic  x(01)                  .
                   15  w-brb-num-pro      pic  9(07)       comp-3     .
                   15  w-brb-alf-pro      pic  x(14)                  .
                   15  w-brb-des-pro      pic  x(40)                  .
                   15  w-brb-umi-ven      pic  x(03)                  .
                   15  w-brb-dec-qta      pic  9(01)                  .
                   15  w-brb-dec-prz      pic  9(01)                  .
                   15  w-brb-qta-ven      pic s9(10)v9(03) comp-3     .
                   15  w-brb-prz-uni      pic  9(09)       comp-3     .
                   15  w-brb-snx-acc      pic  x(01)                  .
                   15  w-brb-flg-dfq      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per funzione saldaconto                         *
      *    *-----------------------------------------------------------*
       01  w-sdc.
      *        *-------------------------------------------------------*
      *        * Flag di uscita da saldaconto                          *
      *        * - spaces : Saldaconto eseguito                        *
      *        * - #      : Uscita per Exit                            *
      *        *-------------------------------------------------------*
           05  w-sdc-flg-exi              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagine massimo                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-max              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina massimo trattata                        *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-mpt              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina attualmente visualizzato                *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-vis              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina da trattare                             *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-dat              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore righe saldaconto                            *
      *        *-------------------------------------------------------*
           05  w-sdc-ctr-rig              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work-area di appoggio                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-wrk-rig              pic  9(03)                  .
           05  w-sdc-wrk-lin              pic  9(03)                  .
           05  w-sdc-wrk-rem              pic  9(03)                  .
           05  w-sdc-wrk-c01              pic  9(03)                  .
           05  w-sdc-wrk-c02              pic  9(03)                  .
           05  w-sdc-wrk-c03              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-trt-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-trt.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-trt-flg          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per routine car-rig-cat-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-car-rig-cat.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-car-rig-cat-ctr          pic  9(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-cod      pic  9(07)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dps]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dps.
               10  w-let-arc-dps-flg      pic  x(01)                  .
               10  w-let-arc-dps-cod      pic  9(07)                  .
               10  w-let-arc-dps-des      pic  x(40)                  .
               10  w-let-arc-dps-umi      pic  x(03)                  .
               10  w-let-arc-dps-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zac]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zac.
               10  w-let-arc-zac-flg      pic  x(01)                  .
               10  w-let-arc-zac-tip      pic  9(02)                  .
               10  w-let-arc-zac-cod      pic  9(03)                  .
               10  w-let-arc-zac-des.
                   15  w-let-arc-zac-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-zac-civ      pic  9(05)                  .
               10  w-let-arc-zac-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.ltw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione codice numerico prodotto           *
      *        *-------------------------------------------------------*
           05  w-det-num-pro.
      *            *---------------------------------------------------*
      *            * Codice alfanumerico prodotto                      *
      *            *---------------------------------------------------*
               10  w-det-num-pro-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico prodotto                          *
      *            *---------------------------------------------------*
               10  w-det-num-pro-num      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det flag di estensione alla descrizione      *
      *        *-------------------------------------------------------*
           05  w-det-des-ext.
               10  w-det-des-ext-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

      *    *===========================================================*
      *    * Work-area per calcolo prezzo netto                        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per prezzo sottoposto a legame valutario        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale, o per messaggi centra- *
      *        * li circondati da un box                               *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione prezzo di vendi- *
      *    * ta                                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dprzven0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione sconti in riga   *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dperscr0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione provvigioni      *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dtl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per funzione di accettazione righe  *
      *    * da apparato di lettura bar-code                           *
      *    *-----------------------------------------------------------*
       01  l-alb-rdb.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-alb-rdb-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-alb-rdb-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-alb-rdb-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento                                   *
      *        *-------------------------------------------------------*
           05  l-alb-rdb-flg-trt          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area libera per espansioni future                     *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(99)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pods3000       *
      *    *                                                           *
      *    * La link-area comprende :                                  *
      *    *                                                           *
      *    *  - 'w-tes'     : Work-area per bufferizzazione testata    *
      *    *  - 'w-pie'     : Work-area per bufferizzazione piede      *
      *    *  - 'w-rig'     : Work-area per bufferizzazione riga       *
      *    *  - 'w-cat-rig' : Work-area di comunicazione per gestione  *
      *    *                  catena righe                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/pods3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-alb-rdb
                                               w-tes
                                               w-pie
                                               w-rig
                                               w-cat-rig              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   l-alb-rdb-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi bolla      *
      *                  *---------------------------------------------*
           if        l-alb-rdb-tip-ope    =    "RB"
                     perform acc-dti-fls-000
                                          thru acc-dti-fls-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per selezione righe bolla        *
      *                  *---------------------------------------------*
           else if   l-alb-rdb-tip-ope    =    "SC"
                     perform sdc-sel-trt-000
                                          thru sdc-sel-trt-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe bolla in catena           *
      *                  *---------------------------------------------*
           else if   l-alb-rdb-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-alb-rdb-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-alb-rdb-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-alb-rdb-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       exe-fun-opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-gen-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice dipendenza in uso        *
      *              *-------------------------------------------------*
           move      l-alb-rdb-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
       exe-fun-opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       exe-fun-cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-gen-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
       exe-fun-cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tst-cnc-mod-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-gen-ctr-opn        =    zero
                     move  spaces         to   l-alb-rdb-exi-sts
           else      move  "#"            to   l-alb-rdb-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione dati identificativi file esterno             *
      *    *-----------------------------------------------------------*
       acc-dti-fls-000.
      *              *-------------------------------------------------*
      *              * Preparazione righe bolla da trattare            *
      *              *-------------------------------------------------*
           perform   buf-rig-trt-000      thru buf-rig-trt-999        .
      *              *-------------------------------------------------*
      *              * A uscita                                        *
      *              *-------------------------------------------------*
           go to     acc-dti-fls-900.
       acc-dti-fls-800.
      *              *-------------------------------------------------*
      *              * Se uscita per Exit                              *
      *              *-------------------------------------------------*
           move      "#"                  to   l-alb-rdb-exi-sts      .
       acc-dti-fls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dti-fls-999.
       acc-dti-fls-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione work-area di accettazione                 *
      *    *-----------------------------------------------------------*
       nor-wrk-acc-000.
       nor-wrk-acc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe bolla da trattare                   *
      *    *-----------------------------------------------------------*
       buf-rig-trt-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita ad errore       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-buf-rig-trt-flg      .
      *              *-------------------------------------------------*
      *              * Azzeramento numero righe nel buffer             *
      *              *-------------------------------------------------*
           move      zero                 to   w-brb-num-ele          .
       buf-rig-trt-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore elementi nel buffer        *
      *              *-------------------------------------------------*
           if        w-brb-num-ele        <    w-brb-max-ele
                     add   1              to   w-brb-num-ele
           else      go to buf-rig-trt-500.

           move      spaces               to   w-buf-rig-trt-flg      .

       buf-rig-trt-250.
      *              *-------------------------------------------------*
      *              * Preparazione elemento nel buffer                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      w-brb-num-ele        to   w-brb-num-prg
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Prodotto                                    *
      *                  *---------------------------------------------*
           move      "P"                  to   w-brb-tip-rig
                                              (w-brb-num-ele)         .
           move      zero                 to   w-brb-num-pro
                                              (w-brb-num-ele)         .
           move      spaces               to   w-brb-alf-pro
                                              (w-brb-num-ele)         .
           move      spaces               to   w-brb-des-pro
                                              (w-brb-num-ele)         .
           move      spaces               to   w-brb-umi-ven
                                              (w-brb-num-ele)         .
           move      zero                 to   w-brb-qta-ven
                                              (w-brb-num-ele)         .
           move      zero                 to   w-brb-prz-uni
                                              (w-brb-num-ele)         .
           move      spaces               to   w-brb-snx-acc
                                              (w-brb-num-ele)         .
           move      spaces               to   w-brb-flg-dfq
                                              (w-brb-num-ele)         .
       buf-rig-trt-400.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file             *
      *              *-------------------------------------------------*
           go to     buf-rig-trt-200.
       buf-rig-trt-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-trt-999.
       buf-rig-trt-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per selezione righe da trattare                *
      *    *-----------------------------------------------------------*
       sdc-sel-trt-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Richiamo funzione di saldaconto                 *
      *              *-------------------------------------------------*
           perform   acc-fun-sdc-000      thru acc-fun-sdc-999        .
           if        w-sdc-flg-exi        not  = spaces
                     move  "#"            to   l-alb-rdb-exi-sts      .
       sdc-sel-trt-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per selezione righe bolla                      *
      *    *-----------------------------------------------------------*
       acc-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-sdc-flg-exi          .
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagine            *
      *              *-------------------------------------------------*
           move      w-brb-num-ele        to   w-sdc-npg-max          .
           divide    14                   into w-sdc-npg-max
                                        giving w-sdc-npg-max
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-max          .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
           perform   pmt-fun-sdc-000      thru pmt-fun-sdc-999        .
      *              *-------------------------------------------------*
      *              * Numero pagina visualizzata                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Numero pagina massima trattata                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-npg-mpt          .
      *              *-------------------------------------------------*
      *              * Numero riga da trattare                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-ctr-rig          .
       acc-fun-sdc-200.
           add       1                    to   w-sdc-ctr-rig          .
           if        w-sdc-ctr-rig        >    w-brb-num-ele
                     go to  acc-fun-sdc-800.
       acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Test su contatore elementi                      *
      *              *-------------------------------------------------*
           if        w-sdc-ctr-rig        >    w-brb-max-ele
                     move  w-brb-max-ele  to   w-sdc-ctr-rig
                                               w-brb-num-ele          .
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina da trattare        *
      *              *-------------------------------------------------*
           divide    14                   into w-sdc-ctr-rig
                                        giving w-sdc-npg-dat
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-dat          .
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagina trattata   *
      *              *-------------------------------------------------*
           if        w-sdc-npg-dat        >    w-sdc-npg-mpt
                     move  w-sdc-npg-dat  to   w-sdc-npg-mpt          .
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina da trattare              *
      *              *-------------------------------------------------*
           if        w-sdc-npg-dat        not  = w-sdc-npg-vis
                     perform vis-pag-sdc-000
                                          thru vis-pag-sdc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione numero linea                     *
      *              *-------------------------------------------------*
           subtract  1                    from w-sdc-npg-dat
                                        giving w-sdc-wrk-rig          .
           multiply  14                   by   w-sdc-wrk-rig          .
           subtract  w-sdc-wrk-rig        from w-sdc-ctr-rig
                                        giving w-sdc-wrk-lin          .
           add       7                    to   w-sdc-wrk-lin          .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice prodotto - da bar-code               *
      *                  *---------------------------------------------*
           perform   acc-cod-pro-000      thru acc-cod-pro-999        .
      *                      *-----------------------------------------*
      *                      * Test su tipo uscita                     *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  " or
                     v-key                =    "DOWN" or
                     v-key                =    "DO  " or
                     v-key                =    "EXIT" or
                     v-key                =    "NXSC" or
                     v-key                =    "PRSC" or
                     v-key                =    "TAB " or
                     v-key                =    "BACK"
                     go to  acc-fun-sdc-520.
       acc-fun-sdc-500.
      *                  *---------------------------------------------*
      *                  * Quantita' di vendita                        *
      *                  *---------------------------------------------*
           perform   acc-qta-ven-000      thru acc-qta-ven-999        .
           if        v-key                =    "UP  "
                     go to  acc-fun-sdc-400.
       acc-fun-sdc-520.
      *                  *---------------------------------------------*
      *                  * Test su tipo uscita                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-fun-sdc-900.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-fun-sdc-910.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     subtract 1           from w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
       acc-fun-sdc-560.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to  acc-fun-sdc-200.
       acc-fun-sdc-570.
      *                      *-----------------------------------------*
      *                      * Se Next Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to  acc-fun-sdc-580.
           multiply  14                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           add       1                    to   w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-580.
      *                      *-----------------------------------------*
      *                      * Se Prev Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to  acc-fun-sdc-600.
           subtract  1                    from w-sdc-npg-dat          .
           multiply  14                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           subtract  13                   from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-600.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                =    "TAB "
                     move   w-brb-num-ele to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "BACK"
                     move   1             to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-200.
       acc-fun-sdc-800.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-fun-sdc-810.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-fun-sdc-810.
      *              *-------------------------------------------------*
      *              * Test su risposta dell'utente                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-fun-sdc-900
           else if   v-key                =    "EXIT"
                     go to acc-fun-sdc-920
           else if   v-key                =    "UP  "
                     go to acc-fun-sdc-820
           else      go to acc-fun-sdc-800.
       acc-fun-sdc-820.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           subtract  1                    from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-999.
       acc-fun-sdc-910.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-800.
       acc-fun-sdc-920.
      *              *-------------------------------------------------*
      *              * Se Exit da conferma impostazioni                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sdc-flg-exi          .
           go to     acc-fun-sdc-999.
       acc-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Funzione saldaconto              *
      *    *-----------------------------------------------------------*
       pmt-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fun-sdc-100.
      *              *-------------------------------------------------*
      *              * Intestazione documento                          *
      *              *-------------------------------------------------*
       pmt-fun-sdc-200.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Linea di fincatura                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non prevede personalizzazioni   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.R|    Codice    |               Descrizione     
      -              "         |Udm| Quantita' | S  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "---|--------------|-------------------------------
      -              "---------|---|-----------|----"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-700.
      *              *-------------------------------------------------*
      *              * Colonne per saldaconto                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c01          .
       pmt-fun-sdc-710.
           add       1                    to   w-sdc-wrk-c01          .
           if        w-sdc-wrk-c01        >    14
                     go to pmt-fun-sdc-900.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "   |              |                               
      -              "         |   |           |    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-fun-sdc-710.
       pmt-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina saldaconto                         *
      *    *-----------------------------------------------------------*
       vis-pag-sdc-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Aggiornamento numero pagina visualizzata        *
      *              *-------------------------------------------------*
           move      w-sdc-npg-dat        to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Determinazione contatore righe iniziale         *
      *              *-------------------------------------------------*
           multiply  14                   by   w-sdc-npg-dat
                                      giving   w-sdc-wrk-rig          .
           subtract  14                   from w-sdc-wrk-rig          .
      *              *-------------------------------------------------*
      *              * Ciclo di visualizzazione                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c01          .
       vis-pag-sdc-100.
           add       1                    to   w-sdc-wrk-c01          .
           if        w-sdc-wrk-c01        >    14
                     go to  vis-pag-sdc-900.
           add       1                    to   w-sdc-wrk-rig          .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che sia una riga vuo-  *
      *                  * ta o piena                                  *
      *                  *---------------------------------------------*
           if        w-brb-num-pro
                    (w-sdc-wrk-rig)       =    zero
                     go to vis-pag-sdc-200.
           if        w-sdc-wrk-rig        >    w-brb-num-ele
                     go to vis-pag-sdc-200
           else      go to vis-pag-sdc-300.
       vis-pag-sdc-200.
      *                  *---------------------------------------------*
      *                  * Se riga vuota                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "   |              |                               
      -              "         |   |           |    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-300.
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga piena                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Comodo per linea a video                *
      *                      *-----------------------------------------*
           add       7
                     w-sdc-wrk-c01      giving w-sdc-wrk-c02          .
           move      w-sdc-wrk-rig        to   w-sdc-wrk-c03          .
      *                      *-----------------------------------------*
      *                      * Progressivo riga                        *
      *                      *-----------------------------------------*
           perform   vis-prg-rig-000      thru vis-prg-rig-999        .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           perform   vis-cod-pro-000      thru vis-cod-pro-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           perform   vis-des-pro-000      thru vis-des-pro-999        .
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
           perform   vis-umi-ven-000      thru vis-umi-ven-999        .
      *                      *-----------------------------------------*
      *                      * Quantita' di vendita                    *
      *                      *-----------------------------------------*
           perform   vis-qta-ven-000      thru vis-qta-ven-999        .
      *                      *-----------------------------------------*
      *                      * Si/no accettazione                      *
      *                      *-----------------------------------------*
           perform   vis-snx-acc-000      thru vis-snx-acc-999        .
       vis-pag-sdc-400.
      *                      *-----------------------------------------*
      *                      * Riciclo su riga successiva              *
      *                      *-----------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Progressivo riga                  *
      *    *-----------------------------------------------------------*
       vis-prg-rig-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-sdc-wrk-c02        to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-sdc-wrk-c03        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prg-rig-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo riga corpo : Codice prodotto           *
      *    *-----------------------------------------------------------*
       acc-cod-pro-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-pro-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente vale solo per i prodotti di     *
      *                  * vendita                                     *
      *                  *---------------------------------------------*
           perform   acc-cod-dcp-000      thru acc-cod-dcp-999        .
       acc-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice prodotto                   *
      *    *-----------------------------------------------------------*
       vis-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      w-sdc-wrk-c02        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-brb-alf-pro
                    (w-sdc-wrk-c03)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione prodotto              *
      *    *-----------------------------------------------------------*
       vis-des-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-sdc-wrk-c02        to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-brb-des-pro
                    (w-sdc-wrk-c03)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Unita' di misura                  *
      *    *-----------------------------------------------------------*
       vis-umi-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-sdc-wrk-c02        to   v-lin                  .
           move      61                   to   v-pos                  .
           move      w-brb-umi-ven
                    (w-sdc-wrk-c03)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umi-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no accettazione                *
      *    *-----------------------------------------------------------*
       vis-snx-acc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-c02        to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-brb-snx-acc
                    (w-sdc-wrk-c03)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-acc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo riga corpo : Codice prodotto [dcp]     *
      *    *-----------------------------------------------------------*
       acc-cod-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore numerico precedente      *
      *                  *---------------------------------------------*
           move      w-brb-num-pro
                    (w-sdc-ctr-rig)       to   w-sav-num-pro          .
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-brb-flg-dfq
                    (w-sdc-ctr-rig)       not  = spaces
                     move  spaces         to   w-brb-flg-dfq
                                              (w-sdc-ctr-rig)
                     go to acc-cod-dcp-600.
       acc-cod-dcp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "L"                  to   w-cod-cod-dcp-tac      .
           move      zero                 to   w-cod-cod-dcp-num      .
           move      w-brb-alf-pro
                    (w-sdc-ctr-rig)       to   w-cod-cod-dcp-alf      .
           move      w-sdc-wrk-lin        to   w-cod-cod-dcp-lin      .
           move      05                   to   w-cod-cod-dcp-pos      .
           move      w-sdc-wrk-lin        to   w-cod-cod-dcp-dln      .
           move      20                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max and
                     w-sdc-npg-dat        <    w-sdc-npg-mpt
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-brb-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-cod-dcp-110.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-cod-dcp-115.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-cod-dcp-120.
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-dcp-115.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-cod-dcp-110.
       acc-cod-dcp-120.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
       acc-cod-dcp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cod-dcp-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-brb-alf-pro
                                              (w-sdc-ctr-rig)         .
       acc-cod-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice a spaces : reimpostazione se non  *
      *                  * in caso di function-key                     *
      *                  *---------------------------------------------*
           if        w-brb-alf-pro
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-cod-dcp-420.
      *                      *-----------------------------------------*
      *                      * Function-key consentite                 *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  " or
                     v-key                =    "DO  " or
                     v-key                =    "PRSC"
                     go to acc-cod-dcp-800.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-dcp-100.
       acc-cod-dcp-420.
      *                  *---------------------------------------------*
      *                  * In base al valore alfanumerico ricevuto in  *
      *                  * input, si determina il codice numerico      *
      *                  * interno del prodotto                        *
      *                  *---------------------------------------------*
           move      w-brb-alf-pro
                    (w-sdc-ctr-rig)       to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito determinazione            *
      *                      *-----------------------------------------*
           if        w-det-num-pro-num    =    zero
                     go to acc-cod-dcp-100.
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di destinazione *
      *                  *---------------------------------------------*
           move      w-det-num-pro-num    to   w-brb-num-pro
                                              (w-sdc-ctr-rig)         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione del codice alfanumerico     *
      *                  *---------------------------------------------*
           move      w-det-num-pro-alf    to   w-brb-alf-pro
                                              (w-sdc-ctr-rig)         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del codice alfanumerico     *
      *                  *---------------------------------------------*
           move      w-sdc-wrk-lin        to   w-sdc-wrk-c02          .
           move      w-sdc-ctr-rig        to   w-sdc-wrk-c03          .
           perform   vis-cod-pro-000      thru vis-cod-pro-999        .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-brb-num-pro
                    (w-sdc-ctr-rig)       to   w-let-dcp-pdx-cod      .
           move      w-tes-tip-arc (1)    to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-arc (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-arc-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-dcp-pdx-flg    not  = spaces
                     go to acc-cod-dcp-100.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione se non in *
      *                  * caso di 'Up' o 'Do'                         *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-dcp-800.
           if        v-key                =    "DO  "
                     go to acc-cod-dcp-800.
           if        w-brb-num-pro
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-cod-dcp-100.
       acc-cod-dcp-450.
      *                  *---------------------------------------------*
      *                  * Test su status commerciale del prodotto     *
      *                  *---------------------------------------------*
       acc-cod-dcp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-brb-num-pro
                    (w-sdc-ctr-rig)       =    w-sav-num-pro
                     go to acc-cod-dcp-999.
       acc-cod-dcp-602.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al record  *
      *                  * [dcp]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione descrizione prodotto o- *
      *                      * riginale                                *
      *                      *-----------------------------------------*
           move      w-let-dcp-pdx-des    to   w-brb-des-pro
                                              (w-sdc-ctr-rig)         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione prodotto    *
      *                      *-----------------------------------------*
           move      w-sdc-wrk-lin        to   w-sdc-wrk-c02          .
           move      w-sdc-ctr-rig        to   w-sdc-wrk-c03          .
           perform   vis-des-pro-000      thru vis-des-pro-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione unita' di misura        *
      *                      *-----------------------------------------*
           move      w-let-dcp-pdx-umi    to   w-brb-umi-ven
                                              (w-sdc-ctr-rig)         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione unita' di misura        *
      *                      *-----------------------------------------*
           move      w-sdc-wrk-lin        to   w-sdc-wrk-c02          .
           move      w-sdc-ctr-rig        to   w-sdc-wrk-c03          .
           perform   vis-umi-ven-000      thru vis-umi-ven-999        .
       acc-cod-dcp-640.
      *                  *---------------------------------------------*
      *                  * Decimali quantita'                          *
      *                  *---------------------------------------------*
           move      w-let-dcp-pdx-deq    to   w-brb-dec-qta
                                              (w-sdc-ctr-rig)         .
       acc-cod-dcp-650.
      *                  *---------------------------------------------*
      *                  * Default per la quantita'                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se zero                            *
      *                      *-----------------------------------------*
           if        w-brb-qta-ven
                    (w-sdc-ctr-rig)       not  = zero
                     go to acc-cod-dcp-660.
      *                      *-----------------------------------------*
      *                      * Default per la quantita'                *
      *                      *-----------------------------------------*
           if        w-brb-qta-ven
                    (w-sdc-ctr-rig)       =    zero
                     move  1              to   w-brb-qta-ven
                                              (w-sdc-ctr-rig)
                     move  "#"            to   w-brb-flg-dfq
                                              (w-sdc-ctr-rig)         .
       acc-cod-dcp-655.
      *                      *-----------------------------------------*
      *                      * Visualizzazione quantita' di vendita    *
      *                      *-----------------------------------------*
           move      w-sdc-wrk-lin        to   w-sdc-wrk-c02          .
           move      w-sdc-ctr-rig        to   w-sdc-wrk-c03          .
           perform   vis-qta-ven-000      thru vis-qta-ven-999        .
       acc-cod-dcp-660.
      *                  *---------------------------------------------*
      *                  * TUTTO DA RIVEDERE ___                       *
      *                  *---------------------------------------------*
       acc-cod-dcp-780.
      *                  *---------------------------------------------*
      *                  * Segnale di riga accettata                   *
      *                  *---------------------------------------------*
           if        w-brb-snx-acc
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-cod-dcp-785.
           move      "S"                  to   w-brb-snx-acc
                                              (w-sdc-ctr-rig)         .
       acc-cod-dcp-785.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del progressivo             *
      *                  *---------------------------------------------*
           move      w-sdc-wrk-lin        to   w-sdc-wrk-c02          .
           move      w-sdc-ctr-rig        to   w-sdc-wrk-c03          .
           perform   vis-prg-rig-000      thru vis-prg-rig-999        .
      *                  *---------------------------------------------*
      *                  * Si/no accettazione                          *
      *                  *---------------------------------------------*
           move      w-sdc-wrk-lin        to   w-sdc-wrk-c02          .
           move      w-sdc-ctr-rig        to   w-sdc-wrk-c03          .
           perform   vis-snx-acc-000      thru vis-snx-acc-999        .
       acc-cod-dcp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Beep di sistema                             *
      *                  *---------------------------------------------*
           display   ""                   with BELL                   .
       acc-cod-dcp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo riga corpo : Codice prodotto [dcp]     *
      *    *                                                           *
      *    * Box di errore emesso in funzione dello status commerciale *
      *    * del prodotto, se diverso da 'normale'                     *
      *    *-----------------------------------------------------------*
       acc-cod-dcp-bsp-000.
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio del video attuale               *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      20                   to   v-lto                  .
           move      08                   to   v-pos                  .
           move      73                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-dcp-bsp-200.
      *                  *---------------------------------------------*
      *                  * Messaggi entro il box                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo centrale                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "                     A T T E N Z I O N E          
      -              "            "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riga messaggio 1                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Per il prodotto impostato si rileva la seguente co
      -              "ndizione :  "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione riga messaggio 2           *
      *                      *-----------------------------------------*
           if        w-let-dcp-pdx-tus    =    11
                     move  "[Prodotto ad esaurimento]                   
      -                    "                  "
                                          to   v-alf
           else if   w-let-dcp-pdx-tus    =    21
                     move  "[Sostituito da ns. nuovo prodotto]          
      -                    "                  "
                                          to   v-alf
           else if   w-let-dcp-pdx-tus    =    51
                     move  "[Cessata commercializzazione]               
      -                    "                  "
                                          to   v-alf
           else if   w-let-dcp-pdx-tus    =    52
                     move  "[Cessata commercializzazione, ma sostituito 
      -                    "da ns. prodotto]  "
                                          to   v-alf
           else if   w-let-dcp-pdx-tus    =    71
                     move  "[Obsoleto]                                  
      -                    "                  "
                                          to   v-alf
           else if   w-let-dcp-pdx-tus    =    72
                     move  "[Obsoleto, ma sostituito da ns. nuovo prodot
      -                    "to]               "
                                          to   v-alf
           else      move  "(status non determinato)                    
      -                    "                  "
                                          to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Riga messaggio 2                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      10                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riga messaggio 3                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da emettere                 *
      *                          *-------------------------------------*
           if        w-let-dcp-pdx-tud    =    zero
                     go to acc-cod-dcp-bsp-300.
      *                          *-------------------------------------*
      *                          * Emissione                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "La determinazione di tale condizione risale al :  
      -              "            "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Data di rilevamento status          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      59                   to   v-pos                  .
           move      w-let-dcp-pdx-tud    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-dcp-bsp-300.
      *                      *-----------------------------------------*
      *                      * Righe messaggio 4 e 5                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da emettere                 *
      *                          *-------------------------------------*
           if        w-let-dcp-pdx-tuc    =    zero
                     go to acc-cod-dcp-bsp-600.
      *                          *-------------------------------------*
      *                          * Emissione                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Il prodotto utilizzabile in alternativa e' il segu
      -              "ente :       "      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Codice prodotto alternativo         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura record [dcp]            *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-dcp-pdx-tuc    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-dcp-alf-pro
                     move  all "."        to   rf-dcp-des-pro         .
      *                              *---------------------------------*
      *                              * Visualizzazione codice prodotto *
      *                              *---------------------------------*

           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      rf-dcp-alf-pro       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione descrizione     *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      rf-dcp-des-pro       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Lettura record [dcp] di ripris- *
      *                              * tino                            *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-dcp-pdx-cod    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       acc-cod-dcp-bsp-600.
      *                      *-----------------------------------------*
      *                      * Prompt per presa visione                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-dcp-bsp-800.
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
           move      19                   to   v-lin                  .
           move      70                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-dcp-bsp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo saldaconto : Quantita' di vendita      *
      *    *-----------------------------------------------------------*
       acc-qta-ven-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-brb-qta-ven
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-ven-050.
           if        w-brb-flg-dfq
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-qta-ven-600.
       acc-qta-ven-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-brb-qta-ven
                    (w-sdc-ctr-rig)       to   w-sav-qta-ven          .
       acc-qta-ven-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-brb-dec-qta
                    (w-sdc-ctr-rig)       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-brb-qta-ven
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-ven-999.
       acc-qta-ven-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-brb-qta-ven
                                              (w-sdc-ctr-rig)         .
       acc-qta-ven-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' a zero non accettabile            *
      *                  *---------------------------------------------*
           if        w-brb-qta-ven
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-ven-100.
       acc-qta-ven-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-qta-ven-800.
       acc-qta-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Quantita' di vendita              *
      *    *-----------------------------------------------------------*
       vis-qta-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-brb-dec-qta
                    (w-sdc-wrk-c03)       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-c02        to   v-lin                  .
           move      65                   to   v-pos                  .
           move      w-brb-qta-ven
                    (w-sdc-wrk-c03)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-qta-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Prezzo di vendita                 *
      *    *                                                           *
      *    * ATTUALMENTE NON UTILIZZATO                                *
      *    *-----------------------------------------------------------*
       vis-prz-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        w-brb-prz-uni
                    (w-sdc-wrk-c03)       >    9999999
                     move  09             to   v-car
           else      move  07             to   v-car                  .
           move      w-brb-dec-prz
                    (w-sdc-wrk-c03)       to   v-dec                  .
           move      spaces               to   v-sgn                  .
           if        w-brb-prz-uni
                    (w-sdc-wrk-c03)       >    9999999
                     move  "B"            to   v-edm
           else      move  "GB"           to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      72                   to   v-pos                  .
           move      w-brb-prz-uni
                    (w-sdc-wrk-c03)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prz-ven-999.
           exit.

      *    *===========================================================*
      *    * Caricamento righe in catena                               *
      *    *-----------------------------------------------------------*
       car-rig-cat-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione buffer righe bolla           *
      *              *-------------------------------------------------*
           move      zero                 to   w-car-rig-cat-ctr      .
       car-rig-cat-100.
      *              *-------------------------------------------------*
      *              * Incremento e test sul contatore                 *
      *              *-------------------------------------------------*
           add       1                    to   w-car-rig-cat-ctr      .
      *              *-------------------------------------------------*
      *              * Selezione su righe nel buffer                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su quantita'                      *
      *                  *---------------------------------------------*
           if        w-brb-num-pro
                    (w-car-rig-cat-ctr)   =    zero
                     go to car-rig-cat-900.
      *                  *---------------------------------------------*
      *                  * Selezione su segnale di si/no accettazione  *
      *                  *---------------------------------------------*
           if        w-brb-snx-acc
                    (w-car-rig-cat-ctr)   =    spaces
                     go to car-rig-cat-900.
           if        w-car-rig-cat-ctr    >    w-brb-num-ele
                     go to car-rig-cat-900.
      *              *-------------------------------------------------*
      *              * Caricamento in 'w-rig'                          *
      *              *-------------------------------------------------*
       car-rig-cat-150.
      *                  *---------------------------------------------*
      *                  * Append in catena                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Append non possibile : uscita con    *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to car-rig-cat-950.
      *                      *-----------------------------------------*
      *                      * Append                                  *
      *                      *-----------------------------------------*
           move      "AP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       car-rig-cat-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione riga                        *
      *                  *---------------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
           move      w-rig-val-aep (1)    to   w-rig-val-aep (2)      .
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero progressivo riga                 *
      *                      *-----------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
      *                      *-----------------------------------------*
      *                      * Tipo riga                               *
      *                      *                                         *
      *                      * Forzatura                               *
      *                      *-----------------------------------------*
           move      "P    "              to   w-rig-tip-rig (1)      .
           move      "P"                  to   w-rig-tip-rig-tpr (1)  .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico prodotto            *
      *                      *-----------------------------------------*
           move      w-brb-alf-pro
                    (w-car-rig-cat-ctr)   to   w-rig-alf-pro (1)      .
      *                      *-----------------------------------------*
      *                      * Codice numerico prodotto                *
      *                      *-----------------------------------------*
           move      w-brb-num-pro
                    (w-car-rig-cat-ctr)   to   w-rig-num-pro (1)      .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [dcp]                  *
      *                      *-----------------------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      w-tes-tip-arc (1)    to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-arc (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-arc-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
       car-rig-cat-330.
      *                      *-----------------------------------------*
      *                      * Tipo magazzino                          *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     move  01             to   w-rig-tip-mag (1)
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     move  02             to   w-rig-tip-mag (1)
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     move  03             to   w-rig-tip-mag (1)
           else      move  zero           to   w-rig-tip-mag (1)      .
       car-rig-cat-360.
      *                      *-----------------------------------------*
      *                      * Sigla variante : non gestita            *
      *                      *-----------------------------------------*
       car-rig-cat-365.
      *                      *-----------------------------------------*
      *                      * Codice prodotto secondo il cliente      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      w-tes-tip-arc (1)    to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-arc (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-arc-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                          *-------------------------------------*
      *                          * Bufferizzazione codice prodotto     *
      *                          * secondo il cliente                  *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-cpc    to   w-rig-cop-scl (1)      .
           go to     car-rig-cat-370.
       car-rig-cat-370.
      *                      *-----------------------------------------*
      *                      * Descrizione prodotto                    *
      *                      *-----------------------------------------*
           move      w-brb-des-pro
                    (w-car-rig-cat-ctr)   to   w-rig-des-rig (1)      .
       car-rig-cat-400.
      *                      *-----------------------------------------*
      *                      * Tipo prodotto                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-402
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-404
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-406
           else      go to car-rig-cat-410.
       car-rig-cat-402.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-tpr    to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-404.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      01                   to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-406.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      01                   to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-410.
      *                      *-----------------------------------------*
      *                      * Codice iva                              *
      *                      *-----------------------------------------*
       car-rig-cat-420.
      *                      *-----------------------------------------*
      *                      * Contropartita vendite                   *
      *                      *-----------------------------------------*
       car-rig-cat-430.
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-432
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-434
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-436
           else      go to car-rig-cat-440.
       car-rig-cat-432.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-umi    to   w-rig-umi-ven (1)      .
           go to     car-rig-cat-440.
       car-rig-cat-434.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      w-let-arc-dpm-umi    to   w-rig-umi-ven (1)      .
           go to     car-rig-cat-440.
       car-rig-cat-436.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      w-let-arc-dps-umi    to   w-rig-umi-ven (1)      .
           go to     car-rig-cat-440.
       car-rig-cat-440.
      *                      *-----------------------------------------*
      *                      * Decimali quantita'                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-442
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-444
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-446
           else      go to car-rig-cat-450.
       car-rig-cat-442.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-deq    to   w-rig-dec-qta (1)      .
           go to     car-rig-cat-450.
       car-rig-cat-444.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      w-let-arc-dpm-deq    to   w-rig-dec-qta (1)      .
           go to     car-rig-cat-450.
       car-rig-cat-446.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      w-let-arc-dps-deq    to   w-rig-dec-qta (1)      .
           go to     car-rig-cat-450.
       car-rig-cat-450.
      *                      *-----------------------------------------*
      *                      * Quantita' di vendita                    *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     move  zero           to   w-rig-qta-ven (1)
           else      move  w-brb-qta-ven
                          (w-car-rig-cat-ctr)
                                          to   w-rig-qta-ven (1)      .
       car-rig-cat-470.
      *                      *-----------------------------------------*
      *                      * Trattamento della seconda quantita'     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se trattamento necessario      *
      *                          *-------------------------------------*
           if        w-rig-snx-2qt (1)    not  = 1
                     go to car-rig-cat-480.
           if        w-rig-qta-ven (1)    =    zero
                     move  zero           to   w-rig-qta-a02 (1)
                     go to car-rig-cat-480.
      *                          *-------------------------------------*
      *                          * Determinazione                      *
      *                          *-------------------------------------*
           move      w-rig-qta-ven (1)    to   w-det-qta-a02-qim      .
           move      w-rig-dec-2qt (1)    to   w-det-qta-a02-ndu      .
      *
           if        w-rig-flg-puq (1)    =    "D"
                     move  10             to   w-det-qta-a02-cmu
           else if   w-rig-flg-puq (1)    =    "C"
                     move  100            to   w-det-qta-a02-cmu
           else if   w-rig-flg-puq (1)    =    "K"
                     move  1000           to   w-det-qta-a02-cmu
           else      move  1              to   w-det-qta-a02-cmu      .
      *
           move      1                    to   w-det-qta-a02-cdu      .
           perform   det-qta-a02-000      thru det-qta-a02-999        .
           move      w-det-qta-a02-qts    to   w-rig-qta-a02 (1)      .
       car-rig-cat-480.
      *                      *-----------------------------------------*
      *                      * Terza quantita' : non gestita           *
      *                      *-----------------------------------------*
       car-rig-cat-490.
      *                      *-----------------------------------------*
      *                      * Decimali prezzo                         *
      *                      *-----------------------------------------*
       car-rig-cat-500.
      *                      *-----------------------------------------*
      *                      * Valuta per prezzi standard              *
      *                      *-----------------------------------------*
       car-rig-cat-510.
      *                      *-----------------------------------------*
      *                      * Prezzo lordo standard                   *
      *                      *-----------------------------------------*
       car-rig-cat-520.
      *                      *-----------------------------------------*
      *                      * Valuta per il prezzo                    *
      *                      *-----------------------------------------*
       car-rig-cat-540.
      *                      *-----------------------------------------*
      *                      * Prezzo di vendita                       *
      *                      *-----------------------------------------*
       car-rig-cat-550.
      *                      *-----------------------------------------*
      *                      * Secondo prezzo : non gestito            *
      *                      *-----------------------------------------*
       car-rig-cat-560.
      *                      *-----------------------------------------*
      *                      * Legame valutario                        *
      *                      *-----------------------------------------*
       car-rig-cat-570.
      *                      *-----------------------------------------*
      *                      * Tipo esposizione riga in fattura        *
      *                      *-----------------------------------------*
       car-rig-cat-580.
      *                      *-----------------------------------------*
      *                      * Categoria e percentuali di sconto in    *
      *                      * riga associate al prodotto              *
      *                      *-----------------------------------------*
       car-rig-cat-590.
      *                      *-----------------------------------------*
      *                      * Sconti in riga                          *
      *                      *-----------------------------------------*
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Tipo esposizione prezzi e sconti        *
      *                      *-----------------------------------------*
       car-rig-cat-610.
      *                      *-----------------------------------------*
      *                      * Valuta per costo di riferimento : non   *
      *                      * gestita                                 *
      *                      *-----------------------------------------*
       car-rig-cat-620.
      *                      *-----------------------------------------*
      *                      * Costo di riferimento : non gestito      *
      *                      *-----------------------------------------*
       car-rig-cat-630.
      *                      *-----------------------------------------*
      *                      * Importo in riga                         *
      *                      *-----------------------------------------*
       car-rig-cat-635.
      *                      *-----------------------------------------*
      *                      * Importo ausiliario riga : non gestito   *
      *                      *-----------------------------------------*
       car-rig-cat-640.
      *                      *-----------------------------------------*
      *                      * Categoria e percentuali di provvigione  *
      *                      * associate al prodotto                   *
      *                      *-----------------------------------------*
       car-rig-cat-650.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' provvigioni in *
      *                      * riga                                    *
      *                      *-----------------------------------------*
       car-rig-cat-660.
      *                      *-----------------------------------------*
      *                      * Categoria provvigioni in riga : non ge- *
      *                      * stito                                   *
      *                      *-----------------------------------------*
       car-rig-cat-670.
      *                      *-----------------------------------------*
      *                      * Percentuali di provvigione in riga      *
      *                      *-----------------------------------------*
       car-rig-cat-680.
      *                      *-----------------------------------------*
      *                      * Provvigione a forfait in riga : zero    *
      *                      *-----------------------------------------*
           move      zero                 to   w-rig-pvf-rig (1)      .
       car-rig-cat-690.
      *                      *-----------------------------------------*
      *                      * Altri valori rimangono normalizzati     *
      *                      *-----------------------------------------*
       car-rig-cat-800.
      *                      *-----------------------------------------*
      *                      * Valori indotti                          *
      *                      *-----------------------------------------*
       car-rig-cat-850.
      *                  *---------------------------------------------*
      *                  * Update catena                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su riga bolla successiva                *
      *              *-------------------------------------------------*
           go to     car-rig-cat-100.
       car-rig-cat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     car-rig-cat-999.
       car-rig-cat-950.
      *              *-------------------------------------------------*
      *              * Messaggio di errore per esubero numero righe    *
      *              *-------------------------------------------------*
           move      "Numero righe documento oltre il massimo previsto !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       car-rig-cat-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           call      "pgm/bol/prg/obj/pods3002"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-prg (1)      .
           move      zero                 to   w-rig-bld-flb (1)      .
           move      zero                 to   w-rig-bld-tpb (1)      .
           move      zero                 to   w-rig-bld-rgb (1)      .
           move      spaces               to   w-rig-tip-rig (1)      .
           move      spaces               to   w-rig-tip-rig-tpr (1)  .
           move      spaces               to   w-rig-tip-rig-tfu (1)  .
           move      zero                 to   w-rig-tip-rig-cac (1)  .
           move      spaces               to   w-rig-tip-rig-ast (1)  .
           move      zero                 to   w-rig-tip-mag (1)      .
           move      zero                 to   w-rig-num-pro (1)      .
           move      spaces               to   w-rig-alf-pro (1)      .
           move      spaces               to   w-rig-sgl-vrn (1)      .
           move      spaces               to   w-rig-cop-scl (1)      .
           move      zero                 to   w-rig-des-ext (1)      .
           move      spaces               to   w-rig-des-por (1)      .
           move      spaces               to   w-rig-des-rig (1)      .
           move      zero                 to   w-rig-tip-pro (1)      .
           move      zero                 to   w-rig-coi-rig (1)      .
           move      spaces               to   w-rig-coi-rig-des (1)  .
           move      zero                 to   w-rig-ctv-rig (1)      .
           move      spaces               to   w-rig-ctv-rig-des (1)  .
           move      spaces               to   w-rig-umi-ven (1)      .
           move      zero                 to   w-rig-dec-qta (1)      .
           move      zero                 to   w-rig-qta-ven (1)      .
           move      zero                 to   w-rig-snx-2qt (1)      .
           move      zero                 to   w-rig-dec-2qt (1)      .
           move      zero                 to   w-rig-qta-a02 (1)      .
           move      zero                 to   w-rig-snx-3qt (1)      .
           move      zero                 to   w-rig-dec-3qt (1)      .
           move      zero                 to   w-rig-qta-a03 (1)      .
           move      zero                 to   w-rig-dec-prz (1)      .
           move      spaces               to   w-rig-sgl-vps (1)      .
           move      zero                 to   w-rig-dec-vps (1)      .
           move      spaces               to   w-rig-tdc-vps (1)      .
           move      zero                 to   w-rig-cdc-vps (1)      .
           move      zero                 to   w-rig-prz-lrs (1)      .
           move      zero                 to   w-rig-prz-nts (1)      .
           move      spaces               to   w-rig-sgl-vpp (1)      .
           move      zero                 to   w-rig-dec-vpp (1)      .
           move      spaces               to   w-rig-tdc-vpp (1)      .
           move      zero                 to   w-rig-cdc-vpp (1)      .
           move      zero                 to   w-rig-prz-ven (1)      .
           move      zero                 to   w-rig-snx-2pz (1)      .
           move      zero                 to   w-rig-prz-a02 (1)      .
           move      spaces               to   w-rig-sgl-vpl (1)      .
           move      zero                 to   w-rig-dec-vpl (1)      .
           move      spaces               to   w-rig-tdc-vpl (1)      .
           move      zero                 to   w-rig-prz-vpl (1)      .
           move      zero                 to   w-rig-cdc-vpl (1)      .
           move      zero                 to   w-rig-ccr-vpl (1)      .
           move      zero                 to   w-rig-plm-vpl (1)      .
           move      spaces               to   w-rig-tlm-vpl (1)      .
           move      spaces               to   w-rig-map-vpl (1)      .
           move      zero                 to   w-rig-epz-rgf (1)      .
           move      zero                 to   w-rig-csr-aap (1)      .
           move      zero                 to   w-rig-psr-aap (1, 1)   .
           move      zero                 to   w-rig-psr-aap (1, 2)   .
           move      zero                 to   w-rig-psr-aap (1, 3)   .
           move      zero                 to   w-rig-psr-aap (1, 4)   .
           move      zero                 to   w-rig-psr-aap (1, 5)   .
           move      zero                 to   w-rig-per-scr (1, 1)   .
           move      zero                 to   w-rig-per-scr (1, 2)   .
           move      zero                 to   w-rig-per-scr (1, 3)   .
           move      zero                 to   w-rig-per-scr (1, 4)   .
           move      zero                 to   w-rig-per-scr (1, 5)   .
           move      zero                 to   w-rig-prz-net (1)      .
           move      zero                 to   w-rig-epz-pes (1)      .
           move      spaces               to   w-rig-sgl-vpc (1)      .
           move      zero                 to   w-rig-dec-vpc (1)      .
           move      spaces               to   w-rig-tdc-vpc (1)      .
           move      zero                 to   w-rig-cdc-vpc (1)      .
           move      zero                 to   w-rig-dec-cos (1)      .
           move      zero                 to   w-rig-cos-rif (1)      .
           move      zero                 to   w-rig-imp-rig (1)      .
           move      zero                 to   w-rig-iau-rig (1)      .
           move      zero                 to   w-rig-cpv-aap (1)      .
           move      zero                 to   w-rig-ppv-aap (1, 1)   .
           move      zero                 to   w-rig-ppv-aap (1, 2)   .
           move      zero                 to   w-rig-ppv-aap (1, 3)   .
           move      zero                 to   w-rig-fsp-rig (1)      .
           move      zero                 to   w-rig-cpv-rig (1)      .
           move      zero                 to   w-rig-ppv-rig (1, 1)   .
           move      zero                 to   w-rig-ppv-rig (1, 2)   .
           move      zero                 to   w-rig-ppv-rig (1, 3)   .
           move      zero                 to   w-rig-pvf-rig (1)      .
           move      zero                 to   w-rig-ocl-dat (1)      .
           move      spaces               to   w-rig-ocl-num (1)      .
           move      spaces               to   w-rig-cmc-tip (1)      .
           move      zero                 to   w-rig-cmc-dat (1)      .
           move      zero                 to   w-rig-cmc-num (1)      .
           move      spaces               to   w-rig-coc-tip (1)      .
           move      zero                 to   w-rig-coc-dat (1)      .
           move      zero                 to   w-rig-coc-num (1)      .
           move      zero                 to   w-rig-coc-prt (1)      .
           move      zero                 to   w-rig-coc-prg (1)      .
           move      spaces               to   w-rig-coc-fzs (1)      .
           move      spaces               to   w-rig-flg-rch (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-flg-puq (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dpm]                         *
      *    *-----------------------------------------------------------*
       let-arc-dpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dpm-cod    =    zero
                     go to let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-let-arc-dpm-cod    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dpm-400.
       let-arc-dpm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dpm-des-map       to   w-let-arc-dpm-des      .
           move      rf-dpm-umi-prd       to   w-let-arc-dpm-umi      .
           move      rf-dpm-dec-qta       to   w-let-arc-dpm-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dpm-999.
       let-arc-dpm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dpm-flg      .
           move      all   "."            to   w-let-arc-dpm-des      .
           go to     let-arc-dpm-600.
       let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-des      .
       let-arc-dpm-600.
           move      spaces               to   w-let-arc-dpm-umi      .
           move      zero                 to   w-let-arc-dpm-deq      .
       let-arc-dpm-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dps]                         *
      *    *-----------------------------------------------------------*
       let-arc-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-cod    =    zero
                     go to let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-let-arc-dps-cod    to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
       let-arc-dps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dps-des-sem       to   w-let-arc-dps-des      .
           move      rf-dps-umi-prd       to   w-let-arc-dps-umi      .
           move      rf-dps-dec-qta       to   w-let-arc-dps-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dps-999.
       let-arc-dps-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dps-flg      .
           move      all   "."            to   w-let-arc-dps-des      .
           go to     let-arc-dps-600.
       let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-des      .
       let-arc-dps-600.
           move      spaces               to   w-let-arc-dps-umi      .
           move      zero                 to   w-let-arc-dps-deq      .
       let-arc-dps-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zac]                         *
      *    *-----------------------------------------------------------*
       let-arc-zac-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zac-tip    =    zero or
                     w-let-arc-zac-cod    =    zero
                     go to let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAOC    "         to   f-key                  .
           move      w-let-arc-zac-tip    to   rf-zac-tip-rec         .
           move      w-let-arc-zac-cod    to   rf-zac-cod-aoc         .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zac-400.
       let-arc-zac-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zac-des-aoc       to   w-let-arc-zac-des      .
           move      rf-zac-cod-iva       to   w-let-arc-zac-civ      .
           move      rf-zac-cod-ctp       to   w-let-arc-zac-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zac-999.
       let-arc-zac-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zac-flg      .
           move      spaces               to   w-let-arc-zac-des      .
           move      all   "."            to   w-let-arc-zac-drg (1)  .
           go to     let-arc-zac-600.
       let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-des      .
       let-arc-zac-600.
           move      zero                 to   w-let-arc-zac-civ      .
           move      zero                 to   w-let-arc-zac-ccp      .
       let-arc-zac-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

      *    *===========================================================*
      *    * Determinazione flag di estensione alla descrizione        *
      *    *-----------------------------------------------------------*
       det-des-ext-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero righe occupate            *
      *              *-------------------------------------------------*
           move      11                   to   w-det-des-ext-ctr      .
       det-des-ext-100.
           subtract  1                    from w-det-des-ext-ctr      .
           if        w-det-des-ext-ctr    =    zero
                     go to det-des-ext-200.
           if        w-rig-der-rig
                    (1, w-det-des-ext-ctr)
                                          =    spaces
                     go to det-des-ext-100.
       det-des-ext-200.
      *              *-------------------------------------------------*
      *              * Se numero righe non superiore a 1               *
      *              *-------------------------------------------------*
           if        w-det-des-ext-ctr    not  > 1
                     move  0              to   w-rig-des-ext (1)
                     go to det-des-ext-999.
      *              *-------------------------------------------------*
      *              * Se numero righe maggiore di 1                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se descrizione impostata uguale a      *
      *                  * descrizione prodotto                        *
      *                  *---------------------------------------------*
           if        w-rig-des-rig (1)    =    w-rig-des-por (1)
                     move  2              to   w-rig-des-ext (1)
           else      move  1              to   w-rig-des-ext (1)      .
       det-des-ext-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di provvigione in riga con e-  *
      *    * ventuale aggiustamento flag di significativita' provvi-   *
      *    * gioni in riga                                             *
      *    *-----------------------------------------------------------*
       det-ppv-rig-000.
      *              *-------------------------------------------------*
      *              * Test se da effettuare                           *
      *              *-------------------------------------------------*
           if        w-rig-fsp-rig (1)    not  = 01
                     go to det-ppv-rig-999.
       det-ppv-rig-100.
      *              *-------------------------------------------------*
      *              * Preparazione link-area                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori diretti da w-tes o w-rig             *
      *                  *---------------------------------------------*
           move      "PP"                 to   d-pvg-age-tip-ope      .
           move      w-tes-cod-age (1)    to   d-pvg-age-cod-age      .
           move      w-tes-tip-vpa (1)    to   d-pvg-age-tip-vpa      .
           move      w-tes-cpv-aaa (1)    to   d-pvg-age-cpv-aaa      .
           move      w-tes-ppv-aaa (1, 1) to   d-pvg-age-ppv-aaa (1)  .
           move      w-tes-ppv-aaa (1, 2) to   d-pvg-age-ppv-aaa (2)  .
           move      w-tes-ppv-aaa (1, 3) to   d-pvg-age-ppv-aaa (3)  .
           move      w-rig-tip-mag (1)    to   d-pvg-age-tip-mag      .
           move      w-rig-num-pro (1)    to   d-pvg-age-num-mag      .
           move      w-tes-cod-lst (1)    to   d-pvg-age-cod-lst      .
           move      w-rig-cpv-aap (1)    to   d-pvg-age-cpv-aap      .
           move      w-rig-ppv-aap (1, 1) to   d-pvg-age-ppv-aap (1)  .
           move      w-rig-ppv-aap (1, 2) to   d-pvg-age-ppv-aap (2)  .
           move      w-rig-ppv-aap (1, 3) to   d-pvg-age-ppv-aap (3)  .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-arc (1)
                                          to   d-pvg-age-cod-cli
           else      move  w-tes-arc-plf (1)
                                          to   d-pvg-age-cod-cli      .
           move      w-tes-cpv-aac (1)    to   d-pvg-age-cpv-aac      .
           move      w-tes-ppv-aac (1, 1) to   d-pvg-age-ppv-aac (1)  .
           move      w-tes-ppv-aac (1, 2) to   d-pvg-age-ppv-aac (2)  .
           move      w-tes-ppv-aac (1, 3) to   d-pvg-age-ppv-aac (3)  .
           move      w-rig-sgl-vpp (1)    to   d-pvg-age-sgl-vpp      .
           move      w-rig-per-scr (1, 1) to   d-pvg-age-per-scr (1)  .
           move      w-rig-per-scr (1, 2) to   d-pvg-age-per-scr (2)  .
           move      w-rig-per-scr (1, 3) to   d-pvg-age-per-scr (3)  .
           move      w-rig-per-scr (1, 4) to   d-pvg-age-per-scr (4)  .
           move      w-rig-per-scr (1, 5) to   d-pvg-age-per-scr (5)  .
           move      w-tes-dat-doc        to   d-pvg-age-dat-rif      .
       det-ppv-rig-200.
      *                  *---------------------------------------------*
      *                  * Valori calcolati                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo lordo standard espresso nella    *
      *                      * valuta per il prezzo                    *
      *                      *-----------------------------------------*
           move      w-rig-prz-lrs (1)    to   d-pvg-age-prz-lrs      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per il prezzo  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-rig-sgl-vpp (1)
                     go to det-ppv-rig-220.
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta per i  *
      *                          * prezzi standard a valuta base       *
      *                          *-------------------------------------*
           move      w-rig-sgl-vps (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vps (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vps (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vps (1)    to   w-cvs-vlt-cdc          .
           move      d-pvg-age-prz-lrs    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta base a *
      *                          * valuta per il prezzo                *
      *                          *-------------------------------------*
           move      w-rig-sgl-vpp (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vpp (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vpp (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vpp (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-lrs      .
       det-ppv-rig-220.
      *                      *-----------------------------------------*
      *                      * Prezzo netto standard espresso nella    *
      *                      * valuta per il prezzo                    *
      *                      *-----------------------------------------*
           move      w-rig-prz-nts (1)    to   d-pvg-age-prz-nts      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per il prezzo  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-rig-sgl-vpp (1)
                     go to det-ppv-rig-240.
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta per i  *
      *                          * prezzi standard a valuta base       *
      *                          *-------------------------------------*
           move      w-rig-sgl-vps (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vps (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vps (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vps (1)    to   w-cvs-vlt-cdc          .
           move      d-pvg-age-prz-nts    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta base a *
      *                          * valuta per il prezzo                *
      *                          *-------------------------------------*
           move      w-rig-sgl-vpp (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vpp (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vpp (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vpp (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-nts      .
       det-ppv-rig-240.
      *                      *-----------------------------------------*
      *                      * Prezzo netto effettivo, gia' espresso   *
      *                      * nella valuta per il prezzo              *
      *                      *-----------------------------------------*
           move      w-rig-prz-net (1)    to   d-pvg-age-prz-net      .
       det-ppv-rig-300.
      *              *-------------------------------------------------*
      *              * Richiamo del sottoprogramma                     *
      *              *-------------------------------------------------*
           perform   det-pvg-age-cll-000  thru det-pvg-age-cll-999    .
       det-ppv-rig-400.
      *              *-------------------------------------------------*
      *              * Memorizzazione percentuali determinate          *
      *              *-------------------------------------------------*
           move      d-pvg-age-per-pvg (1)
                                          to   w-rig-ppv-rig (1, 1)   .
           move      d-pvg-age-per-pvg (2)
                                          to   w-rig-ppv-rig (1, 2)   .
           move      d-pvg-age-per-pvg (3)
                                          to   w-rig-ppv-rig (1, 3)   .
       det-ppv-rig-500.
      *              *-------------------------------------------------*
      *              * Eventuale aggiustamento flag di significativi-  *
      *              * ta' provvigioni in riga                         *
      *              *-------------------------------------------------*
           if        d-pvg-age-flg-pvs    not  = spaces
                     move  03             to   w-rig-fsp-rig (1)      .
       det-ppv-rig-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       det-imp-rig-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      w-rig-qta-ven (1)    to   d-imp-ven-qta-ven      .
           move      w-rig-snx-2qt (1)    to   d-imp-ven-snx-2qt      .
           move      w-rig-qta-a02 (1)    to   d-imp-ven-qta-a02      .
           move      w-rig-snx-3qt (1)    to   d-imp-ven-snx-3qt      .
           move      w-rig-qta-a03 (1)    to   d-imp-ven-qta-a03      .
           move      w-rig-prz-net (1)    to   d-imp-ven-prz-uni      .
           move      w-rig-prz-ven (1)    to   d-imp-ven-prz-unl      .
           move      w-rig-per-scr (1, 1) to   d-imp-ven-per-scr (1)  .
           move      w-rig-per-scr (1, 2) to   d-imp-ven-per-scr (2)  .
           move      w-rig-per-scr (1, 3) to   d-imp-ven-per-scr (3)  .
           move      w-rig-per-scr (1, 4) to   d-imp-ven-per-scr (4)  .
           move      w-rig-per-scr (1, 5) to   d-imp-ven-per-scr (5)  .
           move      w-rig-dec-prz (1)    to   d-imp-ven-dec-prz      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cll-000  thru det-imp-ven-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-ven-imp-rig    to   w-rig-imp-rig (1)      .
       det-imp-rig-600.
      *              *-------------------------------------------------*
      *              * Eventuale conversione nella valuta di fattura-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-rig-sgl-vpp (1)    =    w-tes-sgl-vpf (1)
                     go to det-imp-rig-999.
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta per il prezzo *
      *                  * a valuta base                               *
      *                  *---------------------------------------------*
           move      w-rig-sgl-vpp (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vpp (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vpp (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vpp (1)    to   w-cvs-vlt-cdc          .
           move      w-rig-imp-rig (1)    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta base a valuta *
      *                  * per fatturazione                            *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione del risultato               *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   w-rig-imp-rig (1)      .
       det-imp-rig-999.
           exit.

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

      *    *===========================================================*
      *    * Determinazione quantita' secondaria per la vendita        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wks"                   .

      *    *===========================================================*
      *    * Editing quantita' da incolonnare                          *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

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
           move      16                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
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
           move      16                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      19                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto in base ad    *
      *    * un valore alfanumerico di provenienza da lettore di co-   *
      *    * dice a barre                                              *
      *    *-----------------------------------------------------------*
       det-num-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-pro-num      .
       det-num-pro-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [dcp] per codice alfanumerico *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-400.
       det-num-pro-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [dcp]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-400.
       det-num-pro-230.
      *              *-------------------------------------------------*
      *              * Test max su archivio [dcp]                      *
      *              *-------------------------------------------------*
           if        rf-dcp-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-400.
       det-num-pro-240.
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice numerico                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
       det-num-pro-280.
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     det-num-pro-900.
       det-num-pro-400.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura tramite chiave libera      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "KLBPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-dcp-klb-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-900.
       det-num-pro-420.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [dcp]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-900.
       det-num-pro-430.
      *              *-------------------------------------------------*
      *              * Test max su archivio [dcp]                      *
      *              *-------------------------------------------------*
           if        rf-dcp-klb-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-900.
       det-num-pro-440.
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice numerico                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice alfanumerico             *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-det-num-pro-alf      .
       det-num-pro-480.
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     det-num-pro-900.
       det-num-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-999.
       det-num-pro-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione importo in riga            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione prezzo di vendita          *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dprzven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione sconti in riga             *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dperscr0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione provvigioni                *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
