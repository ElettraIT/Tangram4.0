       Identification Division.
       Program-Id.                                 pbfo300a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bfo                 *
      *                                Settore:    mov                 *
      *                                   Fase:    bfo300a             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 21/12/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Evasione ordini fornitori                   *
      *                                                                *
      *                    Richiamata in bfo300 da funzione 'PF4'      *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "OP"                       *
      *                 l-eva-orf-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-eva-orf-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "DI" - Dichiarazione di inizio ciclo evasione ordine fornitore *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "DI"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "OF" - Accettazione dati identificativi ordine fornitore       *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "OF"                       *
      *                 l-eva-orf-tmo-orf = codice tipo movimento or-  *
      *                                     dini fornitori da proporre *
      *                                     come default               *
      *                                                                *
      *        Output : l-eva-orf-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "T1" - Bufferizzazione testata primo ordine fornitore richia-  *
      *        mato                                                    *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "B1"                       *
      *                                                                *
      *        Output : l-eva-orf-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "T+" - Confronto dati testata ordine fornitore in esame con    *
      *        dati di testata del documento                           *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "T+"                       *
      *                                                                *
      *        Output : l-eva-orf-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "SC" - Saldaconto per evasione ordine                          *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-eva-orf-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe ordine selezionate in catena          *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "CC"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "WR" - Aggiornamento record [ofr] per quanto riguarda il se-   *
      *        gnale di riga ordine comunque considerata saldata in    *
      *        fase di Inserimento di un nuovo record [bfr]            *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "WR"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "RW" - Aggiornamento record [ofr] per quanto riguarda il se-   *
      *        gnale di riga ordine comunque considerata saldata in    *
      *        fase di Modifica di un record [bfr]                     *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "RW"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "DE" - Aggiornamento record [ofr] per quanto riguarda il se-   *
      *        gnale di riga ordine comunque considerata saldata in    *
      *        fase di Cancellazione di un record [bfr]                *
      *                                                                *
      *        Input  : l-eva-orf-tip-ope = "DE"                       *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [oft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [ofx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofx"                          .
      *        *-------------------------------------------------------*
      *        * [yof]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfyof"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [yfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyfp"                          .
      *        *-------------------------------------------------------*
      *        * [yin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyin"                          .
      *        *-------------------------------------------------------*
      *        * [ybo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfybo"                          .
      *        *-------------------------------------------------------*
      *        * [ysf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfysf"                          .
      *        *-------------------------------------------------------*
      *        * [vlt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfvlt"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione materie prime attiva                   *
      *        *-------------------------------------------------------*
           05  w-prs-dpm-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per la riga di scroll               *
      *        *-------------------------------------------------------*
           05  w-prs-rig-scr.
      *            *---------------------------------------------------*
      *            * Esposizione della descrizione                     *
      *            *                                                   *
      *            *   - 00 : Descrizione completa                     *
      *            *                                                   *
      *            *   - 01 : Primi 25 caratteri della descrizione     *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 02 : Primi 21 caratteri della descrizione     *
      *            *          Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 03 : Codice                                   *
      *            *          Primi 25 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 04 : Tipo codice                              *
      *            *          Codice                                   *
      *            *          Primi 21 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 05 : Codice                                   *
      *            *                                                   *
      *            *   - 06 : Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 11 : Codice del fornitore                     *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 12 : Codice del fornitore                     *
      *            *          Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 13 : Codice                                   *
      *            *          Codice del fornitore                     *
      *            *                                                   *
      *            *   - 14 : Tipo codice                              *
      *            *          Codice                                   *
      *            *          Codice del fornitore                     *
      *            *                                                   *
      *            *   - 21 : Codice                                   *
      *            *          Codice della casa produttrice            *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-prs-rig-scr-des      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Esposizione della colonna residuo                 *
      *            *                                                   *
      *            *   - 00 : Quantita' residua                        *
      *            *                                                   *
      *            *   - 01 : Data consegna richiesta e prevista       *
      *            *                                                   *
      *            * N.B.: Significativo solo per programma bfo300a    *
      *            *---------------------------------------------------*
               10  w-prs-rig-scr-res      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Work per manipolazione valore personalizzazione   *
      *            *---------------------------------------------------*
               10  w-prs-rig-scr-str      pic  x(05)                  .
               10  w-prs-rig-scr-str-r redefines
                   w-prs-rig-scr-str.
                   15  w-prs-rig-scr-aaa  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-prs-rig-scr-bbb  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Area gestionale [orf]                                 *
      *        *-------------------------------------------------------*
           05  w-prs-arg-orf.
      *            *---------------------------------------------------*
      *            * Tipo ordinamento righe ordine in caricamento      *
      *            *                                                   *
      *            * - '01' : Ordinamento per progressivo riga ordine  *
      *            * - '03' : Ordinamento per codice alfanumerico pro- *
      *            *          dotto                                    *
      *            *          N.B.: Con questo tipo di ordinamento     *
      *            *                vengono accodati i commenti e      *
      *            *                gli addebiti a fine righe.         *
      *            * - '11' : Ordinamento per ubicazione prodotti      *
      *            *          N.B.: Con questo tipo di ordinamento     *
      *            *                vengono accodati i commenti e      *
      *            *                gli addebiti a fine righe.         *
      *            * - '91' : Ordinamento per progressivo riga ordine  *
      *            *          o codice alfanumerico prodotto, a ri-    *
      *            *          chiesta dell'utente                      *
      *            *          N.B.: Con il secondo tipo di ordinamento *
      *            *                vengono accodati i commenti e      *
      *            *                gli addebiti a fine righe.         *
      *            *---------------------------------------------------*
               10  w-prs-arg-orf-tor      pic  x(02)                  .

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
      *        * Numero protocollo ordine fornitore in corso di trat-  *
      *        * tamento                                               *
      *        *-------------------------------------------------------*
           05  w-gen-prt-orf              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Flag di ordine gia' richiamato                        *
      *        *-------------------------------------------------------*
           05  w-gen-flg-ogr              pic  x(01)                  .

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
      *        * Tipo movimento ordini fornitori                       *
      *        *-------------------------------------------------------*
           05  w-acc-tmo-orf              pic  x(05)                  .
           05  w-acc-tmo-orf-des          pic  x(30)                  .
           05  w-acc-tmo-orf-vld          pic  9(02)                  .
           05  w-acc-tmo-orf-dpz          pic  9(02)                  .
           05  w-acc-tmo-orf-ord          pic  9(02)                  .
           05  w-acc-tmo-orf-prd          pic  9(02)                  .
           05  w-acc-tmo-orf-sgl          pic  x(03)                  .
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
      *        * Tipo ordinamento righe in caricamento                 *
      *        *                                                       *
      *        *  - 01 : In ordine di caricamento righe                *
      *        *  - 02 : In ordine di codice prodotto                  *
      *        *  - 03 : In ordine di ubicazione                       *
      *        *-------------------------------------------------------*
           05  w-acc-tip-ord              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-oft.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-oft-dds      pic  9(07)                  .
               10  w-slc-num-oft-nds      pic  9(11)                  .
               10  w-slc-num-oft-nds-r redefines
                   w-slc-num-oft-nds.
                   15  w-slc-num-oft-nsa  pic  9(03)                  .
                   15  w-slc-num-oft-ndp  pic  9(02)                  .
                   15  w-slc-num-oft-npg  pic  9(06)                  .
               10  w-slc-num-oft-dpz      pic  9(02)                  .
               10  w-slc-num-oft-sgl      pic  x(03)                  .
               10  w-slc-num-oft-saa      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-oft-sel      pic  x(01)                  .
               10  w-slc-num-oft-toc      pic  x(05)                  .
               10  w-slc-num-oft-dat      pic  9(07)                  .
               10  w-slc-num-oft-num      pic  9(11)                  .
               10  w-slc-num-oft-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-slc-num-oft-c01      pic  9(02)                  .
               10  w-slc-num-oft-c02      pic  9(02)                  .
               10  w-slc-num-oft-c03      pic  9(02)                  .
               10  w-slc-num-oft-c04      pic  9(02)                  .
               10  w-slc-num-oft-c05      pic  9(02)                  .
               10  w-slc-num-oft-nli      pic  9(02)                  .
               10  w-slc-num-oft-crb      pic  9(02)                  .
               10  w-slc-num-oft-cpb      pic  9(02)                  .
               10  w-slc-num-oft-cpa      pic  9(02)                  .
               10  w-slc-num-oft-buf
                               occurs 30.
                   15  w-slc-num-oft-bpt  pic  9(11)                  .
               10  w-slc-num-oft-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-slc-num-oft-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-slc-num-oft-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-val-acc.
               10  filler   occurs 200    pic  x(01)                  .
           05  w-sav-qta-bfo              pic s9(10)v9(03)            .
           05  w-sav-flg-fzs              pic  x(01)                  .
           05  w-sav-snx-aoc              pic  x(01)                  .
           05  w-sav-prz-acq              pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Area w-rig                                            *
      *        *-------------------------------------------------------*
           05  w-sav-wrk-rig.
               10  filler    occurs 3072  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per salvataggio catena                           *
      *        *-------------------------------------------------------*
           05  w-sav-cat-rig.
               10  filler    occurs 4096  pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento righe in caricamento      *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 3    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 30   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(30) value
                            "In ordine di inserimento Righe"          .
                   15  filler             pic  x(30) value
                            "In ordine di codice Prodotto  "          .
                   15  filler             pic  x(30) value
                            "In ordine di Ubicazione       "          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione protocolli ordini fornitori *
      *    *-----------------------------------------------------------*
       01  w-poc.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-poc-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-poc-max-ele              pic  9(05) value 99         .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-poc-tbl.
               10  w-poc-sng-ele occurs 99.
                   15  w-poc-num-prt      pic  9(11)                  .
               10  w-poc-ctr-001          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe ordine fornitore      *
      *    *-----------------------------------------------------------*
       01  w-bro.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-bro-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-bro-max-ele              pic  9(05) value 750        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-bro-tbl.
               10  w-bro-sng-ele occurs 750.
                   15  w-bro-num-prg      pic  9(05)       comp-3     .
                   15  w-bro-tip-rig-tpr  pic  x(01)                  .
                   15  w-bro-tip-rig-tfu  pic  x(01)                  .
                   15  w-bro-tip-mag      pic  9(02)                  .
                   15  w-bro-num-mag      pic  9(07)       comp-3     .
                   15  w-bro-des-rig      pic  x(40)                  .
                   15  w-bro-dec-qta      pic  9(01)                  .
                   15  w-bro-qta-dev      pic s9(10)v9(03) comp-3     .
                   15  w-bro-qta-bfo      pic s9(10)v9(03) comp-3     .
                   15  w-bro-flg-idr      pic  x(01)                  .
                   15  w-bro-flg-ids      pic  x(01)                  .
                   15  w-bro-flg-fzs      pic  x(01)                  .
                   15  w-bro-qta-res      pic s9(10)v9(03) comp-3     .
                   15  w-bro-snx-aoc      pic  x(01)                  .
                   15  w-bro-dcn-ric      pic  9(07)       comp-3     .
                   15  w-bro-fds-dcr      pic  x(01)                  .
                   15  w-bro-dcn-prv      pic  9(07)       comp-3     .
                   15  w-bro-flg-cnf      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione valute per legame valutario *
      *    *-----------------------------------------------------------*
       01  w-vpl.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-vpl-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-vpl-max-ele              pic  9(05) value 999        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-vpl-tbl.
               10  w-vpl-sng-ele occurs 999.
                   15  w-vpl-sgl-vpl      pic  x(03)                  .
                   15  w-vpl-cdc-vpl      pic  9(06)v9(05)            .
                   15  w-vpl-cdc-rif      pic  9(06)v9(05)            .
                   15  w-vpl-dat-rif      pic  9(07)                  .
                   15  w-vpl-per-scs      pic s9(03)v9(01)            .
               10  w-vpl-ctr-001          pic  9(05)                  .
               10  w-vpl-ctr-002          pic  9(05)                  .
               10  w-vpl-wrk-scs          pic s9(08)v9(05)            .

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
           05  w-sdc-ctr-rig              pic s9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work-area di appoggio                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-wrk-rig              pic  9(03)                  .
           05  w-sdc-wrk-lin              pic  9(03)                  .
           05  w-sdc-wrk-rem              pic  9(03)                  .
           05  w-sdc-wrk-c01              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area di comodo per quantita' senza segno             *
      *    *-----------------------------------------------------------*
       01  w-qss.
           05  w-qss-qta-dev              pic  9(10)v9(03)            .
           05  w-qss-qta-bfo              pic  9(10)v9(03)            .

      *    *===========================================================*
      *    * Work-area per caricamento righe ordine in catena          *
      *    *-----------------------------------------------------------*
       01  w-crc.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-crc-wrk-ctr              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-tes-orf-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-tes-orf.
           05  w-buf-tes-orf-ctr          pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-orf-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-orf.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orf-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine fornitore                    *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orf-prt          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tipo riga                    *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orf-wtr.
               10  w-buf-rig-orf-wtp      pic  x(01)                  .
               10  w-buf-rig-orf-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione in riga                                   *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orf-wde          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto righe da bufferizzare                     *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orf-cst.
      *            *---------------------------------------------------*
      *            * Singolo elemento                                  *
      *            *---------------------------------------------------*
               10  w-buf-rig-orf-ele  occurs 999.
      *                *-----------------------------------------------*
      *                * Chiave di ordinamento                         *
      *                *-----------------------------------------------*
                   15  w-buf-rig-orf-key.
      *                    *-------------------------------------------*
      *                    * Chiave                                    *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orf-kal  pic  x(47)              .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '01'                *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orf-kal-01  redefines
                           w-buf-rig-orf-kal.
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-buf-rig-orf-prg-01
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(42)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '03'                *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orf-kal-03  redefines
                           w-buf-rig-orf-kal.
      *                        *---------------------------------------*
      *                        * Codice alfanumerico prodotto          *
      *                        *---------------------------------------*
                           25  w-buf-rig-orf-alf-03
                                          pic  x(14)                  .
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-buf-rig-orf-prg-03
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(28)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '11'                *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orf-kal-11  redefines
                           w-buf-rig-orf-kal.
      *                        *---------------------------------------*
      *                        * Ubicazione                            *
      *                        *---------------------------------------*
                           25  w-buf-rig-orf-ubi-11
                                          pic  x(28)                  .
      *                        *---------------------------------------*
      *                        * Codice alfanumerico prodotto          *
      *                        *---------------------------------------*
                           25  w-buf-rig-orf-alf-11
                                          pic  x(14)                  .
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-buf-rig-orf-prg-11
                                          pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Dati del buffer                               *
      *                *-----------------------------------------------*
                   15  w-buf-rig-orf-dat.
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orf-prg
                                          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Area di comodo                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orf-wrk.
      *            *---------------------------------------------------*
      *            * Comodo per salvataggio chiave di ordinamento      *
      *            *---------------------------------------------------*
               10  w-buf-rig-orf-svk      pic  x(47)                  .
      *            *---------------------------------------------------*
      *            * Numero massimo di elementi                        *
      *            *---------------------------------------------------*
               10  w-buf-rig-orf-max      pic  9(05)     value 999    .
      *            *---------------------------------------------------*
      *            * Contatori elementi                                *
      *            *---------------------------------------------------*
               10  w-buf-rig-orf-ctr      pic  9(05)                  .
               10  w-buf-rig-orf-cte      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-buf-rig-orf-c01      pic  9(05)                  .
               10  w-buf-rig-orf-c02      pic  9(05)                  .
               10  w-buf-rig-orf-c03      pic  9(05)                  .
               10  w-buf-rig-orf-c04      pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione descrizione riga di scroll    *
      *    *-----------------------------------------------------------*
       01  w-des-scr.
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 00                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-000.
               10  w-des-scr-000-d40      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 01                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-001 redefines
               w-des-scr-000.
               10  w-des-scr-001-d25      pic  x(25)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-001-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 02                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-002 redefines
               w-des-scr-000.
               10  w-des-scr-002-d21      pic  x(21)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-002-pqs      pic  x(01)                  .
               10  w-des-scr-002-tco      pic  x(01)                  .
               10  w-des-scr-002-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-002-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 03                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-003 redefines
               w-des-scr-000.
               10  w-des-scr-003-cod      pic  x(14)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-003-d25      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 04                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-004 redefines
               w-des-scr-000.
               10  w-des-scr-004-pqs      pic  x(01)                  .
               10  w-des-scr-004-tco      pic  x(01)                  .
               10  w-des-scr-004-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-004-cod      pic  x(14)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-004-d21      pic  x(21)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 05                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-005 redefines
               w-des-scr-000.
               10  filler                 pic  x(26)                  .
               10  w-des-scr-005-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 06                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-006 redefines
               w-des-scr-000.
               10  filler                 pic  x(22)                  .
               10  w-des-scr-006-pqs      pic  x(01)                  .
               10  w-des-scr-006-tco      pic  x(01)                  .
               10  w-des-scr-006-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-006-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 11                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-011 redefines
               w-des-scr-000.
               10  filler                 pic  x(03)                  .
               10  w-des-scr-011-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-011-cdf      pic  x(14)                  .
               10  filler                 pic  x(05)                  .
               10  w-des-scr-011-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 12                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-012 redefines
               w-des-scr-000.
               10  filler                 pic  x(01)                  .
               10  w-des-scr-012-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-012-cdf      pic  x(14)                  .
               10  filler                 pic  x(03)                  .
               10  w-des-scr-012-pqs      pic  x(01)                  .
               10  w-des-scr-012-tco      pic  x(01)                  .
               10  w-des-scr-012-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-012-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 13                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-013 redefines
               w-des-scr-000.
               10  w-des-scr-013-cod      pic  x(14)                  .
               10  filler                 pic  x(04)                  .
               10  w-des-scr-013-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-013-cdf      pic  x(14)                  .
               10  filler                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 14                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-014 redefines
               w-des-scr-000.
               10  w-des-scr-014-pqs      pic  x(01)                  .
               10  w-des-scr-014-tco      pic  x(01)                  .
               10  w-des-scr-014-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-014-cod      pic  x(14)                  .
               10  filler                 pic  x(02)                  .
               10  w-des-scr-014-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-014-cdf      pic  x(14)                  .
               10  filler                 pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 21                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-021 redefines
               w-des-scr-000.
               10  w-des-scr-021-cod      pic  x(14)                  .
               10  filler                 pic  x(04)                  .
               10  w-des-scr-021-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-021-cpp      pic  x(18)                  .

      *    *===========================================================*
      *    * Work per routine dec-tip-rig-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-dec-tip-rig.
           05  w-dec-tip-rig-inx          pic  9(02)                  .
           05  w-dec-tip-rig-pnt          pic  9(02)                  .
           05  w-dec-tip-rig-str.
               10  w-dec-tip-rig-chr      occurs 05
                                          pic  x(01)                  .
           05  w-dec-tip-rig-cod.
               10  w-dec-tip-rig-num      occurs 03
                                          pic  9(01)                  .
           05  w-dec-tip-rig-c01          pic  9(02)                  .
           05  w-dec-tip-rig-c02          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per routine exe-rca-pcm-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-exe-rca-pcm.
      *        *-------------------------------------------------------*
      *        * Numero riga per la quale e' stato trovato il codice   *
      *        * di magazzino                                          *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-nrg          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Tipo codice di magazzino                              *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-tpm          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice numerico di magazzino                          *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-cnm          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico di magazzino                      *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-cam          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione di magazzino                              *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-dem          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-ctr          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Indice di comodo                                      *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-inx          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Buffer numeri riga con codice di magazzino pari a     *
      *        * quello incontrato                                     *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-buf.
               10  w-exe-rca-pcm-ele      pic  9(03)                  .
               10  w-exe-rca-pcm-max      pic  9(03) value 10         .
               10  w-exe-rca-pcm-bnr occurs 10
                                          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio linee                          *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-sl1          pic  x(80)                  .
           05  w-exe-rca-pcm-sl2          pic  x(80)                  .
           05  w-exe-rca-pcm-sl3          pic  x(80)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo codice di magazzino                              *
      *        *-------------------------------------------------------*
           05  w-sav-tip-mag              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo codice di magazzino                   *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mag.
               10  w-exp-tip-mag-num      pic  9(02)       value 2    .
               10  w-exp-tip-mag-lun      pic  9(02)       value 20   .
               10  w-exp-tip-mag-tbl.
                   15  filler             pic  x(20) value
                            "Prodotto di vendita "                    .
                   15  filler             pic  x(20) value
                            "Materia prima       "                    .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [oft]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-oft.
               10  w-fnd-arc-oft-sel      pic  x(01)                  .
               10  w-fnd-arc-oft-toc      pic  x(05)                  .
               10  w-fnd-arc-oft-dat      pic  9(07)                  .
               10  w-fnd-arc-oft-num      pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yof]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yof.
               10  w-let-arc-yof-flg      pic  x(01)                  .
               10  w-let-arc-yof-cod      pic  x(05)                  .
               10  w-let-arc-yof-des      pic  x(30)                  .
               10  w-let-arc-yof-vld      pic  9(02)                  .
               10  w-let-arc-yof-dpz      pic  9(02)                  .
               10  w-let-arc-yof-ord      pic  9(02)                  .
               10  w-let-arc-yof-prd      pic  9(02)                  .
               10  w-let-arc-yof-sgl      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
               10  w-let-arc-fnt-piv      pic  9(11)                  .
               10  w-let-arc-fnt-stc      pic  9(07)                  .
               10  w-let-arc-fnt-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-fnt      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
               10  w-let-arc-dcf-via      pic  x(40)                  .
               10  w-let-arc-dcf-loc      pic  x(40)                  .
               10  w-let-arc-dcf-vlt      pic  x(03)                  .
               10  w-let-arc-dcf-lng      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvl]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvl.
               10  w-let-arc-zvl-flg      pic  x(01)                  .
               10  w-let-arc-zvl-cod      pic  x(03)                  .
               10  w-let-arc-zvl-des      pic  x(20)                  .
               10  w-let-arc-zvl-din      pic  x(20)                  .
               10  w-let-arc-zvl-dec      pic  9(01)                  .
               10  w-let-arc-zvl-tdc      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yfp.
               10  w-let-arc-yfp-flg      pic  x(01)                  .
               10  w-let-arc-yfp-cod      pic  9(07)                  .
               10  w-let-arc-yfp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cbp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cbp.
               10  w-let-arc-cbp-flg      pic  x(01)                  .
               10  w-let-arc-cbp-tip      pic  9(02)                  .
               10  w-let-arc-cbp-cod      pic  x(10)                  .
               10  w-let-arc-cbp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axi.
               10  w-let-arc-axi-flg      pic  x(01)                  .
               10  w-let-arc-axi-cod      pic  9(05)                  .
               10  w-let-arc-axi-den      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axs.
               10  w-let-arc-axs-flg      pic  x(01)                  .
               10  w-let-arc-axs-abi      pic  9(05)                  .
               10  w-let-arc-axs-cab      pic  9(05)                  .
               10  w-let-arc-axs-den      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yin]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yin.
               10  w-let-arc-yin-flg      pic  x(01)                  .
               10  w-let-arc-yin-cod      pic  x(03)                  .
               10  w-let-arc-yin-tpg      pic  9(02)                  .
               10  w-let-arc-yin-des      pic  x(40)                  .
               10  w-let-arc-yin-coi      pic  9(05)                  .
               10  w-let-arc-yin-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ybo]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ybo.
               10  w-let-arc-ybo-flg      pic  x(01)                  .
               10  w-let-arc-ybo-cod      pic  x(03)                  .
               10  w-let-arc-ybo-tpg      pic  9(02)                  .
               10  w-let-arc-ybo-des      pic  x(40)                  .
               10  w-let-arc-ybo-coi      pic  9(05)                  .
               10  w-let-arc-ybo-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-cod      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des.
                   15  w-let-arc-dcp-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-dcp-dui      pic  x(40)                  .
               10  w-let-arc-dcp-tpr      pic  9(02)                  .
               10  w-let-arc-dcp-civ      pic  9(05)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
               10  w-let-arc-dcp-plb      pic  9(09)                  .
               10  w-let-arc-dcp-ctr      pic  9(02)                  .
               10  w-let-arc-dcp-lng      pic  x(03)                  .
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
      *        * Work per Let su archivio [mtv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-mtv.
               10  w-let-arc-mtv-flg      pic  x(01)                  .
               10  w-let-arc-mtv-cod      pic  9(07)                  .
               10  w-let-arc-mtv-des      pic  x(40)                  .
               10  w-let-arc-mtv-umi      pic  x(03)                  .
               10  w-let-arc-mtv-deq      pic  9(01)                  .
               10  w-let-arc-mtv-snm      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [aaq]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-aaq.
               10  w-let-arc-aaq-flg      pic  x(01)                  .
               10  w-let-arc-aaq-tpm      pic  9(02)                  .
               10  w-let-arc-aaq-cdm      pic  9(07)                  .
               10  w-let-arc-aaq-svl      pic  x(03)                  .
               10  w-let-arc-aaq-dvl      pic  9(01)                  .
               10  w-let-arc-aaq-dep      pic  9(01)                  .
               10  w-let-arc-aaq-prz      pic  9(09)                  .
               10  w-let-arc-aaq-civ      pic  9(05)                  .
               10  w-let-arc-aaq-ctp      pic  9(07)                  .
               10  w-let-arc-aaq-epz      pic  9(01)                  .
               10  w-let-arc-aaq-cdp      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [aaf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-aaf.
               10  w-let-arc-aaf-flg      pic  x(01)                  .
               10  w-let-arc-aaf-tpm      pic  9(02)                  .
               10  w-let-arc-aaf-cdm      pic  9(07)                  .
               10  w-let-arc-aaf-fnt      pic  9(07)                  .
               10  w-let-arc-aaf-fda      pic  x(14)                  .
               10  w-let-arc-aaf-csf      pic  x(14)                  .
               10  w-let-arc-aaf-dmg.
                   15  w-let-arc-aaf-drm occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-aaf-des.
                   15  w-let-arc-aaf-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-aaf-snu      pic  x(01)                  .
               10  w-let-arc-aaf-umf      pic  x(03)                  .
               10  w-let-arc-aaf-ndu      pic  9(01)                  .
               10  w-let-arc-aaf-cmu      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-cdu      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-tpa      pic  9(02)                  .
               10  w-let-arc-aaf-vlt      pic  x(03)                  .
               10  w-let-arc-aaf-dcv      pic  9(01)                  .
               10  w-let-arc-aaf-ndp      pic  9(01)                  .
               10  w-let-arc-aaf-lda      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-tap      pic  9(02)                  .
               10  w-let-arc-aaf-lvv      pic  x(03)                  .
               10  w-let-arc-aaf-lvd      pic  9(01)                  .
               10  w-let-arc-aaf-lvt      pic  x(01)                  .
               10  w-let-arc-aaf-lvc      pic  9(06)v9(05)            .
               10  w-let-arc-aaf-lvp      pic  9(01)v9(02)            .
               10  w-let-arc-aaf-mpa      pic  9(02)v9(01)            .
               10  w-let-arc-aaf-tbp.
                   15  w-let-arc-aaf-etp occurs 06.
                       20  w-let-arc-aaf-qtp
                                          pic  9(06)v9(03)            .
                       20  w-let-arc-aaf-pzp
                                          pic  9(09)                  .
                       20  w-let-arc-aaf-csp
                                          pic  9(05)                  .
                       20  w-let-arc-aaf-psp occurs 05
                                          pic  9(02)v9(01)            .
               10  w-let-arc-aaf-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ofx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ofx.
               10  w-let-arc-ofx-flg      pic  x(01)                  .
               10  w-let-arc-ofx-prt      pic  9(11)                  .
               10  w-let-arc-ofx-prg      pic  9(05)                  .
               10  w-let-arc-ofx-trc      pic  9(02)                  .
               10  w-let-arc-ofx-des.
                   15  w-let-arc-ofx-drg occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det documento gia' esistente oppure no       *
      *        *-------------------------------------------------------*
           05  w-det-doc-ges.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato esistente in    *
      *            *       archivio                                    *
      *            * - N : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio                                 *
      *            * - X : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio, ma esistenza di un documento   *
      *            *       con data e numero cercato, con stesso co-   *
      *            *       dice numerazione, ma con diverso tipo mo-   *
      *            *       vimento da quello cercato                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento trovato per valore precedente de-  *
      *            * terminato a 'X'                                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-tmt      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine fornitore                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dtl"                   .

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
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dimpacq0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione ubicazione       *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dtl"                   .

      *    *===========================================================*
      *    * Work per buffer documenti movimentati                     *
      *    *-----------------------------------------------------------*
       01  w-buf-doc.
      *        *-------------------------------------------------------*
      *        * Work per buffer                                       *
      *        *-------------------------------------------------------*
           05  w-buf-doc-mvm.
      *            *---------------------------------------------------*
      *            * Data di richiesta                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-drc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice utente                                     *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-ute      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Flag di selezione                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-fds      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo documento selezionato                        *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-tds      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Numero documento selezionato                      *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-nds      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Data documento selezionato                        *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-dds      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-c01      pic  9(02)                  .
               10  w-buf-doc-mvm-c02      pic  9(02)                  .
               10  w-buf-doc-mvm-c03      pic  9(02)                  .
               10  w-buf-doc-mvm-c04      pic  9(02)                  .
               10  w-buf-doc-mvm-c05      pic  9(02)                  .
               10  w-buf-doc-mvm-nli      pic  9(02)                  .
               10  w-buf-doc-mvm-crb      pic  9(02)                  .
               10  w-buf-doc-mvm-cpb      pic  9(02)                  .
               10  w-buf-doc-mvm-cpa      pic  9(02)                  .
               10  w-buf-doc-mvm-max      pic  9(02) value 54         .
               10  w-buf-doc-mvm-buf occurs 54.
                   15  w-buf-doc-mvm-tmo  pic  x(05)                  .
                   15  w-buf-doc-mvm-num  pic  9(11)                  .
                   15  w-buf-doc-mvm-dat  pic  9(07)                  .
                   15  w-buf-doc-mvm-rsa  pic  x(40)                  .
               10  w-buf-doc-mvm-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-buf-doc-mvm-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-buf-doc-mvm-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per contatori ed indici                              *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore generico 'I'                                *
      *        *-------------------------------------------------------*
           05  I                          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore generico 'J'                                *
      *        *-------------------------------------------------------*
           05  J                          pic  9(05)                  .

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
      *    * Link-area per accettazione tipo movimento ordini fornitori*
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/acdeyof0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia prima 'dpm'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per evasione ordini fornitori       *
      *    *-----------------------------------------------------------*
       01  l-eva-orf.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-eva-orf-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-eva-orf-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-eva-orf-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento ordini fornitori    *
      *        *-------------------------------------------------------*
           05  l-eva-orf-tmo-orf          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento ordine fornitore                  *
      *        *-------------------------------------------------------*
           05  l-eva-orf-flg-orf          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di function-key Tab in corso                     *
      *        *-------------------------------------------------------*
           05  l-eva-orf-fky-tab          pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pbfo3000       *
      *    *                                                           *
      *    * La link-area comprende :                                  *
      *    *                                                           *
      *    *  - 'w-tes'     : Work-area per bufferizzazione testata    *
      *    *  - 'w-pie'     : Work-area per bufferizzazione piede      *
      *    *  - 'w-rig'     : Work-area per bufferizzazione riga       *
      *    *  - 'w-cat-rig' : Work-area di comunicazione per gestione  *
      *    *                  catena righe                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/prg/cpy/pbfo3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-eva-orf
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
           move      spaces               to   l-eva-orf-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dichiarazione di inizio ciclo di evasione   *
      *                  *---------------------------------------------*
           if        l-eva-orf-tip-ope    =    "DI"
                     perform dic-ini-cic-000
                                          thru dic-ini-cic-999
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi ordine     *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "OF"
                     perform acc-dti-orf-000
                                          thru acc-dti-orf-999
      *                  *---------------------------------------------*
      *                  * Bufferizzazione testata primo ordine forni- *
      *                  * tore richiamato                             *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "T1"
                     perform buf-tes-orf-000
                                          thru buf-tes-orf-999
      *                  *---------------------------------------------*
      *                  * Confronto testata ordine fornitore richia-  *
      *                  * mato con i dati di testata del documento    *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "T+"
                     perform cnf-tes-doc-000
                                          thru cnf-tes-doc-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per evasione righe ordine        *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "SC"
                     perform sdc-eva-orf-000
                                          thru sdc-eva-orf-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe ordine in catena          *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [ofr] per quanto ri-   *
      *                  * guarda il segnale di riga ordine comunque   *
      *                  * considerata saldata in fase di Inserimento  *
      *                  * di un nuovo record [bfr]                    *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "WR"
                     perform wrt-rec-ofr-000
                                          thru wrt-rec-ofr-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [ofr] per quanto ri-   *
      *                  * guarda il segnale di riga ordine comunque   *
      *                  * considerata saldata in fase di Modifica     *
      *                  * di un record [bfr]                          *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "RW"
                     perform rew-rec-ofr-000
                                          thru rew-rec-ofr-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [ofr] per quanto ri-   *
      *                  * guarda il segnale di riga ordine comunque   *
      *                  * considerata saldata in fase di Cancellazio- *
      *                  * ne di un record [bfr]                       *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "DE"
                     perform del-rec-ofr-000
                                          thru del-rec-ofr-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-eva-orf-tip-ope    =    "C?"
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
           move      l-eva-orf-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione materie prime attiva         *
      *                  *---------------------------------------------*
           perform   prs-dpm-snx-000      thru prs-dpm-snx-999        .
      *                  *---------------------------------------------*
      *                  * Esposizione della riga in scroll            *
      *                  *---------------------------------------------*
           perform   prs-rig-scr-000      thru prs-rig-scr-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento righe ordine in caricamen- *
      *                  * to                                          *
      *                  *---------------------------------------------*
           perform   prs-orf-tor-000      thru prs-orf-tor-999        .
       exe-fun-opn-100.
      *              *-------------------------------------------------*
      *              * Apertura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oft]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * [ofx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
      *                  *---------------------------------------------*
      *                  * [yof]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
      *                  *---------------------------------------------*
      *                  * [ysf]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per gli *
      *              * ordini fornitori                                *
      *              *-------------------------------------------------*
           perform   cod-des-yof-opn-000  thru cod-des-yof-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice materia prima   *
      *              * 'dpm'                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to exe-fun-opn-200.
      *                  *---------------------------------------------*
      *                  * Apertura modulo                             *
      *                  *---------------------------------------------*
           perform   cod-cod-dpm-opn-000  thru cod-cod-dpm-opn-999    .
       exe-fun-opn-200.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine fornitore                    *
      *              *-------------------------------------------------*
           perform   det-qev-rof-opn-000  thru det-qev-rof-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione importo in riga   *
      *              *-------------------------------------------------*
           perform   det-imp-acq-opn-000  thru det-imp-acq-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione ubicazioni        *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-opn-000  thru det-prm-ubi-opn-999    .
       exe-fun-opn-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materie prime  *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dpm-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dpm[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dpm-snx
           else      move  spaces         to   w-prs-dpm-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dpm-snx        =    "S" or
                     w-prs-dpm-snx        =    "N"
                     go to prs-dpm-snx-999.
           move      "N"                  to   w-prs-dpm-snx          .
       prs-dpm-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al tipo di espo- *
      *    * sizione della riga di scroll                              *
      *    *-----------------------------------------------------------*
       prs-rig-scr-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/bfo/mov/bfo300[rig-scr]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione esistente si spostano i    *
      *              * valori letti in area di destinazione, altri-    *
      *              * menti si normalizza l'area di destinazione      *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rig-scr-str
           else      move  spaces         to   w-prs-rig-scr-str      .
      *              *-------------------------------------------------*
      *              * Decomposizione valore                           *
      *              *-------------------------------------------------*
           move      w-prs-rig-scr-aaa    to   w-prs-rig-scr-des      .
           move      w-prs-rig-scr-bbb    to   w-prs-rig-scr-res      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo esposizione descrizione    *
      *              *-------------------------------------------------*
           if        w-prs-rig-scr-des    not  = 00 and
                     w-prs-rig-scr-des    not  = 01 and
                     w-prs-rig-scr-des    not  = 02 and
                     w-prs-rig-scr-des    not  = 03 and
                     w-prs-rig-scr-des    not  = 04 and
                     w-prs-rig-scr-des    not  = 05 and
                     w-prs-rig-scr-des    not  = 06 and
                     w-prs-rig-scr-des    not  = 11 and
                     w-prs-rig-scr-des    not  = 12 and
                     w-prs-rig-scr-des    not  = 13 and
                     w-prs-rig-scr-des    not  = 14 and
                     w-prs-rig-scr-des    not  = 21
                     move  00             to   w-prs-rig-scr-des      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo esposizione residuo        *
      *              *-------------------------------------------------*
           if        w-prs-rig-scr-res    not  = 00 and
                     w-prs-rig-scr-res    not  = 01
                     move  00             to   w-prs-rig-scr-res      .
       prs-rig-scr-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo ordinamento righe ordine *
      *    * in caricamento                                            *
      *    *-----------------------------------------------------------*
       prs-orf-tor-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/bfo/mov/bfo300a[tor-cev]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-arg-orf-tor
           else      move  spaces         to   w-prs-arg-orf-tor      .
       prs-orf-tor-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-arg-orf-tor    not  = "01" and
                     w-prs-arg-orf-tor    not  = "03" and
                     w-prs-arg-orf-tor    not  = "11" and
                     w-prs-arg-orf-tor    not  = "91"
                     move  "01"           to   w-prs-arg-orf-tor      .
       prs-orf-tor-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       exe-fun-cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-gen-ctr-opn          .
       exe-fun-cls-100.
      *              *-------------------------------------------------*
      *              * Chiusura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oft]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * [ofx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
      *                  *---------------------------------------------*
      *                  * [yof]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
      *                  *---------------------------------------------*
      *                  * [ysf]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per    *
      *              * ordini fornitori                                *
      *              *-------------------------------------------------*
           perform   cod-des-yof-cls-000  thru cod-des-yof-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice materia prima  *
      *              * 'dpm'                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se gestione materie prime attiva       *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to exe-fun-cls-200.
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           perform   cod-cod-dpm-cls-000  thru cod-cod-dpm-cls-999    .
       exe-fun-cls-200.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine fornitore                    *
      *              *-------------------------------------------------*
           perform   det-qev-rof-cls-000  thru det-qev-rof-cls-999    .
       exe-fun-cls-800.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione importo in riga  *
      *              *-------------------------------------------------*
           perform   det-imp-acq-cls-000  thru det-imp-acq-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione ubicazioni       *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-cls-000  thru det-prm-ubi-cls-999    .
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
                     move  spaces         to   l-eva-orf-exi-sts
           else      move  "#"            to   l-eva-orf-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio ciclo di evasione                 *
      *    *-----------------------------------------------------------*
       dic-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Azzeramento contatore numero elementi in ta-    *
      *              * bella protocolli ordini fornitori               *
      *              *-------------------------------------------------*
           move      zero                 to   w-poc-num-ele          .
       dic-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione dati identificativi ordine fornitore         *
      *    *-----------------------------------------------------------*
       acc-dti-orf-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area di accettazione       *
      *              *-------------------------------------------------*
           perform   nor-wrk-acc-000      thru nor-wrk-acc-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo centrale                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      "RICHIAMO ORDINE FORNITORE"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Data:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "Numero:"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo movi-   *
      *              * mento ordini fornitori                          *
      *              *-------------------------------------------------*
           move      l-eva-orf-tmo-orf    to   w-acc-tmo-orf          .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [yof]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orf        to   w-let-arc-yof-cod      .
           perform   let-arc-yof-000      thru let-arc-yof-999        .
           move      w-let-arc-yof-des    to   w-acc-tmo-orf-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice                      *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Ad accettazione data documento                  *
      *              *-------------------------------------------------*
           go to     acc-dti-orf-200.
       acc-dti-orf-100.
      *              *-------------------------------------------------*
      *              * Accettazione dati identificativi ordine         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo movimento ordini fornitori             *
      *                  *---------------------------------------------*
           perform   acc-tmo-orf-000      thru acc-tmo-orf-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orf-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orf-400.
       acc-dti-orf-200.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   acc-dat-doc-000      thru acc-dat-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orf-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orf-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orf-100.
       acc-dti-orf-300.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   acc-num-doc-000      thru acc-num-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orf-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orf-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orf-200.
       acc-dti-orf-350.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento righe in caricamento       *
      *                  *---------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orf-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orf-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orf-300.
       acc-dti-orf-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori di iden-  *
      *              * tificazione dell'ordine                         *
      *              *-------------------------------------------------*
           if        w-acc-tmo-orf        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to acc-dti-orf-100.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Secolo/Anno                                 *
      *                  *---------------------------------------------*
           move      w-acc-dat-doc        to   s-dat                  .
           move      s-saa                to   w-acc-num-doc-saa      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      w-acc-cod-dpz        to   w-acc-num-doc-dpz      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione tipo ordinamento      *
      *              *-------------------------------------------------*
           if        w-prs-arg-orf-tor    =    "91" and
                     w-acc-tip-ord        =    zero
                     move  01             to   w-acc-tip-ord          .
      *              *-------------------------------------------------*
      *              * Determinazione se documento esistente           *
      *              *-------------------------------------------------*
           perform   det-doc-ges-000      thru det-doc-ges-999        .
      *                  *---------------------------------------------*
      *                  * Se documento non esistente                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "N"
                     go to acc-dti-orf-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Movimento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orf-100.
       acc-dti-orf-420.
      *                  *---------------------------------------------*
      *                  * Se documento esistente ma con tipo movimen- *
      *                  * to diverso                                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "X"
                     go to acc-dti-orf-440.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Movimento esistente, ma di tipo '"
                                delimited by   size
                     w-det-doc-ges-tmt
                                delimited by   spaces
                     "' !"
                                delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orf-100.
       acc-dti-orf-440.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo               *
      *              *-------------------------------------------------*
           move      rf-oft-num-prt       to   w-acc-num-prt          .
      *              *-------------------------------------------------*
      *              * Test su flag di ordine chiuso                   *
      *              *-------------------------------------------------*
           if        rf-oft-flg-och       =    spaces
                     go to acc-dti-orf-500.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Ordine fornitore chiuso !                         
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-orf-100.
       acc-dti-orf-500.
      *              *-------------------------------------------------*
      *              * Test se l'ordine e' gia' stato richiamato       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-gen-flg-ogr          .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-poc-ctr-001          .
       acc-dti-orf-520.
           add       1                    to   w-poc-ctr-001          .
           if        w-poc-ctr-001        >    w-poc-num-ele
                     go to acc-dti-orf-540.
           if        w-acc-num-prt        not  = w-poc-num-prt
                                                (w-poc-ctr-001)
                     go to acc-dti-orf-520.
      *                  *---------------------------------------------*
      *                  * Avviso all'operatore                        *
      *                  *---------------------------------------------*
           move      "Ordine fornitore gia' richiamato !                
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Set del flag di ordine richiamato           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-gen-flg-ogr          .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *                                             *
      *                  * Attualmente inibito                         *
      *                  *                                             *
      *                  * (Mario - Dicembre 2020)                     *
      *                  *---------------------------------------------*
______*    go to     acc-dti-orf-100.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-dti-orf-540.
       acc-dti-orf-540.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe ordine da trattare        *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-buf-rig-orf-prt      .
           perform   buf-rig-orf-000      thru buf-rig-orf-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-buf-rig-orf-flg    =    spaces
                     go to acc-dti-orf-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine fornitore gia' evaso !                     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orf-100.
       acc-dti-orf-600.
      *              *-------------------------------------------------*
      *              * Accettazione castelletto valute per legame va-  *
      *              * lutario                                         *
      *              *-------------------------------------------------*
           perform   acc-cdc-vpl-000      thru acc-cdc-vpl-999
           if        v-key                =    "EXIT"
                     go to acc-dti-orf-800.
      *              *-------------------------------------------------*
      *              * Incremento contatore protocolli ordini fornito- *
      *              * ri                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-poc-num-ele          .
           if        w-poc-num-ele        not  >  w-poc-max-ele
                     go to acc-dti-orf-700.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo                         *
      *                  *---------------------------------------------*
           move      "Numero ordini fornitori trattati oltre il massimo 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita per Exit                           *
      *                  *---------------------------------------------*
           go to     acc-dti-orf-800.
       acc-dti-orf-700.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo in tabella    *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-poc-num-prt
                                              (w-poc-num-ele)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo ordine fornitore in    *
      *              * corso di trattamento                            *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-gen-prt-orf          .
      *              *-------------------------------------------------*
      *              * A uscita                                        *
      *              *-------------------------------------------------*
           go to     acc-dti-orf-900.
       acc-dti-orf-800.
      *              *-------------------------------------------------*
      *              * Se uscita per Exit                              *
      *              *-------------------------------------------------*
           move      "#"                  to   l-eva-orf-exi-sts      .
       acc-dti-orf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dti-orf-999.
       acc-dti-orf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo movimento per ordini fornitori  *
      *    *-----------------------------------------------------------*
       acc-tmo-orf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tmo-orf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-yof-ope      .
           move      w-acc-tmo-orf        to   w-cod-des-yof-cod      .
           move      14                   to   w-cod-des-yof-lin      .
           move      09                   to   w-cod-des-yof-pos      .
           move      14                   to   w-cod-des-yof-dln      .
           move      15                   to   w-cod-des-yof-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-des-yof-cll-000  thru cod-des-yof-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-des-yof-foi-000  thru cod-des-yof-foi-999    .
       acc-tmo-orf-110.
           perform   cod-des-yof-cll-000  thru cod-des-yof-cll-999    .
           if        w-cod-des-yof-ope    =    "F+"
                     go to acc-tmo-orf-115.
           if        w-cod-des-yof-ope    =    "AC"
                     go to acc-tmo-orf-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tmo-orf-115.
           perform   cod-des-yof-foi-000  thru cod-des-yof-foi-999    .
           go to     acc-tmo-orf-110.
       acc-tmo-orf-120.
           move      w-cod-des-yof-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-tmo-orf-999.
       acc-tmo-orf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-tmo-orf          .
       acc-tmo-orf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orf        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-tmo-orf-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [yof]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orf        to   w-let-arc-yof-cod      .
           perform   let-arc-yof-000      thru let-arc-yof-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini fornitori                  *
      *                  *---------------------------------------------*
           move      w-let-arc-yof-des    to   w-acc-tmo-orf-des      .
           move      w-let-arc-yof-vld    to   w-acc-tmo-orf-vld      .
           move      w-let-arc-yof-dpz    to   w-acc-tmo-orf-dpz      .
           move      w-let-arc-yof-ord    to   w-acc-tmo-orf-ord      .
           move      w-let-arc-yof-prd    to   w-acc-tmo-orf-prd      .
           move      w-let-arc-yof-sgl    to   w-acc-tmo-orf-sgl      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yof-flg    not  = spaces
                     go to acc-tmo-orf-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orf        =    spaces
                     go to acc-tmo-orf-100.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orf-vld    not  = 02
                     go to acc-tmo-orf-600.
           if        w-acc-cod-dpz        =    w-acc-tmo-orf-dpz
                     go to acc-tmo-orf-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tmo-orf-100.
       acc-tmo-orf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tmo-orf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tmo-orf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : tipo movimento per ordini forni-  *
      *    * tori                                                      *
      *    *-----------------------------------------------------------*
       vis-tmo-orf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-tmo-orf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione tipo movimento ordini *
      *    * fornitori                                                 *
      *    *-----------------------------------------------------------*
       vis-tmo-orf-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-acc-tmo-orf-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orf-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data documento                       *
      *    *-----------------------------------------------------------*
       acc-dat-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-acc-dat-doc        not  = zero
                     go to acc-dat-doc-100.
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-acc-dat-doc          .
       acc-dat-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-dat-doc-999.
       acc-dat-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-doc-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oft]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oft-000      thru fnd-arc-oft-999        .
           if        w-fnd-arc-oft-sel    not  = spaces
                     go to acc-dat-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           if        w-prs-arg-orf-tor    not  = "91"
                     move  "DO  "         to   v-key                  .
       acc-dat-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia su Up                        *
      *                  *---------------------------------------------*
           if        w-acc-dat-doc        not  = zero
                     go to acc-dat-doc-600.
           if        v-key                =    "UP  "
                     go to acc-dat-doc-600
           else      go to acc-dat-doc-100.
       acc-dat-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data documento                    *
      *    *-----------------------------------------------------------*
       vis-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero documento                     *
      *    *-----------------------------------------------------------*
       acc-num-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-doc-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto di pagina precedente per visualizzare gli or
      -              "dini emessi alla data indicata"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "SLCT"               to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione eventuali note operative          *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-num-doc-999.
       acc-num-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-num-doc-prg      .
       acc-num-doc-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-doc-350.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oft]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oft-000      thru fnd-arc-oft-999        .
           if        w-fnd-arc-oft-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-num-doc-350.
      *              *-------------------------------------------------*
      *              * Se Select                                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-num-doc-380.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-acc-num-doc-prg    =    zero
                     go to acc-num-doc-100.
           if        w-acc-dat-doc        =    zero
                     go to acc-num-doc-100.
           if        w-acc-tmo-orf        =    spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      w-acc-num-doc-prg    to   w-slc-num-oft-npg      .
           move      w-acc-cod-dpz        to   w-slc-num-oft-dpz      .
           move      w-acc-tmo-orf-sgl    to   w-slc-num-oft-sgl      .
           move      w-acc-dat-doc        to   w-slc-num-oft-dds      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   slc-num-oft-000      thru slc-num-oft-999        .
      *                  *---------------------------------------------*
      *                  * Se non selezionato alcun elemento : a       *
      *                  * reimpostazione                              *
      *                  *---------------------------------------------*
           if        w-slc-num-oft-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-num-doc-400.
       acc-num-doc-380.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-num-doc-400.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-buf-doc-mvm-drc      .
           move      s-ute                to   w-buf-doc-mvm-ute      .
           move      w-tes-cod-dpz        to   w-buf-doc-mvm-dpz      .
      *
           if        w-acc-dat-doc        =    zero
                     move  w-tes-dat-reg  to   w-buf-doc-mvm-drc
           else if   w-tes-dat-reg        =    zero
                     move  s-dat          to   w-buf-doc-mvm-drc
           else      move  w-acc-dat-doc  to   w-buf-doc-mvm-drc      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   buf-doc-mvm-000      thru buf-doc-mvm-999        .
      *                      *-----------------------------------------*
      *                      * Test sul flag di uscita                 *
      *                      *-----------------------------------------*
           if        w-buf-doc-mvm-fds    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Valori selezionati                          *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-tds    to   w-acc-tmo-orf          .
           move      w-buf-doc-mvm-nds    to   w-acc-num-doc          .
           move      w-buf-doc-mvm-dds    to   w-acc-dat-doc          .
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi         *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
       acc-num-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero documento                  *
      *    *-----------------------------------------------------------*
       vis-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento righe in caricamen- *
      *    *                      to                                   *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-arg-orf-tor    not  = "91"
                     go to acc-tip-ord-999.
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      "RPU#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-tip-ord        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-tip-ord-999.
       acc-tip-ord-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-tip-ord          .
       acc-tip-ord-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-ord-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ord-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Coefficienti di cambio applicati per lega- *
      *    * me valutario                                              *
      *    *-----------------------------------------------------------*
       acc-cdc-vpl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da accettare                        *
      *                  *---------------------------------------------*
           if        w-vpl-num-ele        =    zero
                     go to acc-cdc-vpl-999.
      *                  *---------------------------------------------*
      *                  * Preparazione prompts area accettazione      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Titolo                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "                        CAMBI DA APPLICARE        
      -              "                 "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      " --------+------------------+---------------------
      -              "--+------------- "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "  Valuta | Cambio Da Applic.| Cambio di riferiment
      -              "o | Scostamento  "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      " --------+------------------+---------------------
      -              "--+------------- "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Corpo                                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "         |                  |                     
      -              "  |              "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      14                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      15                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      16                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      17                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      18                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Parantesi quadre per presa visione      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cdc-vpl-050.
      *                  *---------------------------------------------*
      *                  * Visualizzazione preliminare valori          *
      *                  *---------------------------------------------*
           move      zero                 to   w-vpl-ctr-001          .
       acc-cdc-vpl-052.
           add       1                    to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    w-vpl-num-ele
                     go to acc-cdc-vpl-075.
           if        w-vpl-ctr-001        >    6
                     go to acc-cdc-vpl-075.
      *                      *-----------------------------------------*
      *                      * Sigla valuta                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-vpl-sgl-vpl
                    (w-vpl-ctr-001)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio applicato        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio di riferimento   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-vpl-cdc-rif
                    (w-vpl-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Data di riferimento cambio              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      51                   to   v-pos                  .
           move      w-vpl-dat-rif
                    (w-vpl-ctr-001)       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Percentuale di scostamento              *
      *                      *-----------------------------------------*
           perform   vis-per-scs-000      thru vis-per-scs-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     acc-cdc-vpl-052.
       acc-cdc-vpl-075.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      1                    to   w-vpl-ctr-001          .
       acc-cdc-vpl-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      20                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cdc-vpl-999.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-vpl-cdc-vpl
                                              (w-vpl-ctr-001)         .
       acc-cdc-vpl-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso                   *
      *                  *---------------------------------------------*
           if        w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       =    zero
                     go to acc-cdc-vpl-100.
       acc-cdc-vpl-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione percentuale di scostamento   *
      *                  *---------------------------------------------*
           subtract  w-vpl-cdc-rif
                    (w-vpl-ctr-001)       from w-vpl-cdc-vpl
                                              (w-vpl-ctr-001)
                                        giving w-vpl-wrk-scs          .
           multiply  100                  by   w-vpl-wrk-scs          .
           divide    w-vpl-cdc-rif
                    (w-vpl-ctr-001)       into w-vpl-wrk-scs
                                        giving w-vpl-per-scs
                                              (w-vpl-ctr-001)
                                               rounded                .
      *                  *---------------------------------------------*
      *                  * Visualizzazione percentuale di scostamento  *
      *                  *---------------------------------------------*
           perform   vis-per-scs-000      thru vis-per-scs-999        .
       acc-cdc-vpl-700.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'impostazione        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cdc-vpl-710
           else if   v-key                =    "DOWN"
                     go to acc-cdc-vpl-720
           else if   v-key                =    "DO  "
                     go to acc-cdc-vpl-800
           else      go to acc-cdc-vpl-720.
       acc-cdc-vpl-710.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        w-vpl-ctr-001        =    1
                     go to acc-cdc-vpl-100.
           subtract  1                    from w-vpl-ctr-001          .
           go to     acc-cdc-vpl-100.
       acc-cdc-vpl-720.
      *              *-------------------------------------------------*
      *              * Se Down o Return                                *
      *              *-------------------------------------------------*
           if        w-vpl-ctr-001        <    w-vpl-num-ele
                     add   1              to   w-vpl-ctr-001
           else      go to acc-cdc-vpl-722.
           if        w-vpl-ctr-001        >    6
                     go to acc-cdc-vpl-722
           else      go to acc-cdc-vpl-100.
       acc-cdc-vpl-722.
      *                  *---------------------------------------------*
      *                  * Accettazione carattere di presa visione     *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-cdc-vpl-800.
           move      w-vpl-num-ele        to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    6
                     move  6              to   w-vpl-ctr-001          .
           go to     acc-cdc-vpl-100.
       acc-cdc-vpl-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo che tutti gli elementi siano sta- *
      *                  * ti impostati                                *
      *                  *---------------------------------------------*
           move      zero                 to   w-vpl-ctr-002          .
       acc-cdc-vpl-820.
           add       1                    to   w-vpl-ctr-002          .
           if        w-vpl-ctr-002        >    w-vpl-num-ele
                     go to acc-cdc-vpl-900.
           if        w-vpl-ctr-002        >    6
                     go to acc-cdc-vpl-900.
           if        w-vpl-cdc-vpl
                    (w-vpl-ctr-002)       =    zero
                     go to acc-cdc-vpl-100.
           go to     acc-cdc-vpl-820.
       acc-cdc-vpl-900.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cdc-vpl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Percentuale di scostamento              *
      *    *-----------------------------------------------------------*
       vis-per-scs-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      spaces               to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      64                   to   v-pos                  .
           move      w-vpl-per-scs
                    (w-vpl-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-per-scs-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione work-area di accettazione                 *
      *    *-----------------------------------------------------------*
       nor-wrk-acc-000.
           move      zero                 to   w-acc-num-prt          .
           move      spaces               to   w-acc-tmo-orf          .
           move      spaces               to   w-acc-tmo-orf-des      .
           move      zero                 to   w-acc-tmo-orf-vld      .
           move      zero                 to   w-acc-tmo-orf-dpz      .
           move      zero                 to   w-acc-tmo-orf-ord      .
           move      zero                 to   w-acc-tmo-orf-prd      .
           move      spaces               to   w-acc-tmo-orf-sgl      .
           move      zero                 to   w-acc-num-doc          .
           move      zero                 to   w-acc-tip-ord          .
       nor-wrk-acc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *-----------------------------------------------------------*
       buf-rig-orf-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita ad errore       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-buf-rig-orf-flg      .
      *              *-------------------------------------------------*
      *              * Preparazione castelletto righe in memoria       *
      *              *-------------------------------------------------*
           perform   buf-rig-orf-pcr-000  thru buf-rig-orf-pcr-999    .
      *              *-------------------------------------------------*
      *              * Azzeramento numero righe nel buffer             *
      *              *-------------------------------------------------*
           move      zero                 to   w-bro-num-ele          .
      *              *-------------------------------------------------*
      *              * Azzeramento numero elementi nel buffer valute   *
      *              * per legame                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-vpl-num-ele          .
      *              *-------------------------------------------------*
      *              * Azzeramento contatore righe nel buffer          *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orf-cte      .
      *              *-------------------------------------------------*
      *              * Test su numero elementi letti                   *
      *              *-------------------------------------------------*
           if        w-buf-rig-orf-ctr    not   > zero
                     go to buf-rig-orf-900.
       buf-rig-orf-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe nel buffer           *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-rig-orf-cte      .
      *              *-------------------------------------------------*
      *              * Test su contatore righe                         *
      *              *-------------------------------------------------*
           if        w-buf-rig-orf-cte    >    w-buf-rig-orf-ctr
                     go to buf-rig-orf-900.
           if        w-buf-rig-orf-cte    >    w-buf-rig-orf-max
                     go to buf-rig-orf-900.
           if        w-buf-rig-orf-prg
                    (w-buf-rig-orf-cte)   =    zero
                     go to buf-rig-orf-900.
       buf-rig-orf-210.
      *              *-------------------------------------------------*
      *              * Normalizzazione riga                            *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       buf-rig-orf-220.
      *              *-------------------------------------------------*
      *              * Lettura riga in corso di trattamento            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-orf-prt    to   rf-ofr-num-prt         .
           move      w-buf-rig-orf-prg
                    (w-buf-rig-orf-cte)   to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orf-200.
       buf-rig-orf-230.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-ofr-qta-ord       to   w-edt-qta-inc-qta      .
           move      rf-ofr-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-din    to   w-tes-dec-qta          .
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-ofr-tip-rig       to   w-buf-rig-orf-wtr      .
      *              *-------------------------------------------------*
      *              * Se tipo riga "C" : a bufferizzazione riga       *
      *              *-------------------------------------------------*
           if        w-buf-rig-orf-wtp    =    "C"
                     go to buf-rig-orf-300.
      *              *-------------------------------------------------*
      *              * Test su flag di riga chiusa                     *
      *              *-------------------------------------------------*
           if        rf-ofr-flg-rch       not  = spaces
                     go to buf-rig-orf-200.
      *              *-------------------------------------------------*
      *              * Test su segnale di riga comunque considerata    *
      *              * saldata                                         *
      *              *-------------------------------------------------*
           if        rf-ofr-sdr-ccs       not  = spaces
                     go to buf-rig-orf-200.
      *              *-------------------------------------------------*
      *              * Se tipo riga "A" : a ciclo di controllo per or- *
      *              * dine gia' richiamato                            *
      *              *-------------------------------------------------*
           if        w-buf-rig-orf-wtp    =    "A"
                     go to buf-rig-orf-250.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' evasa riga ordine for- *
      *              * nitore                                          *
      *              *                                                 *
      *              * N.B.: Viene temporaneamente inibita la selezio- *
      *              *       ne su documenti da verificare             *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-qev-rof-tip-ope      .
           move      "#"                  to   d-qev-rof-flg-dav      .
           perform   det-qev-rof-cll-000  thru det-qev-rof-cll-999    .
      *              *-------------------------------------------------*
      *              * Se riga evasa : riciclo                         *
      *              *-------------------------------------------------*
           if        d-qev-rof-qta-dri    =    zero
                     go to buf-rig-orf-200.
       buf-rig-orf-250.
      *              *-------------------------------------------------*
      *              * Se ordine gia' richiamato, aggiornamento quan-  *
      *              * tita' da ricevere per quanto gia' caricato      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-gen-flg-ogr        =    spaces
                     go to buf-rig-orf-280.
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   buf-rig-orf-qdr-000  thru buf-rig-orf-qdr-999    .
      *                  *---------------------------------------------*
      *                  * Test su quantita' da ricevere ritarata      *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-dri    =    zero
                     go to buf-rig-orf-200.
       buf-rig-orf-280.
      *              *-------------------------------------------------*
      *              * Altrimenti normalizzazione flag di uscita       *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orf-flg      .
       buf-rig-orf-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga ordine                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore elementi nel buffer    *
      *                  *---------------------------------------------*
           if        w-bro-num-ele        <    w-bro-max-ele
                     add   1              to   w-bro-num-ele          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      rf-ofr-num-prg       to   w-bro-num-prg
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo prodotto                               *
      *                  *---------------------------------------------*
           move      w-buf-rig-orf-wtp    to   w-bro-tip-rig-tpr
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
           move      w-buf-rig-orf-wtf    to   w-bro-tip-rig-tfu
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           move      rf-ofr-tip-mag       to   w-bro-tip-mag
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      rf-ofr-num-mag       to   w-bro-num-mag
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura preliminare file [ofx]          *
      *                      *-----------------------------------------*
           move      rf-ofr-num-prt       to   w-let-arc-ofx-prt      .
           move      rf-ofr-num-prg       to   w-let-arc-ofx-prg      .
           move      11                   to   w-let-arc-ofx-trc      .
           perform   let-arc-ofx-000      thru let-arc-ofx-999        .
      *
           if        w-let-arc-ofx-flg    not  = spaces
                     go to buf-rig-orf-305.
      *
           move      w-let-arc-ofx-des    to   w-buf-rig-orf-wde      .
      *
           go to     buf-rig-orf-350.
       buf-rig-orf-305.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del flag di e-   *
      *                      * stensione alla descrizione              *
      *                      *-----------------------------------------*
           if        rf-ofr-des-ext       =    0
                     go to buf-rig-orf-310
           else if   rf-ofr-des-ext       =    1
                     go to buf-rig-orf-320
           else if   rf-ofr-des-ext       =    2
                     go to buf-rig-orf-330
           else if   rf-ofr-des-ext       =    3
                     go to buf-rig-orf-340.
       buf-rig-orf-310.
      *                      *-----------------------------------------*
      *                      * Se nessuna estensione : bufferizzazione *
      *                      * descrizione contenuta nel record [ofr]  *
      *                      *-----------------------------------------*
           move      rf-ofr-des-rig       to   w-buf-rig-orf-wde      .
      *
           go to     buf-rig-orf-350.
       buf-rig-orf-320.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [ofx]            *
      *                      *-----------------------------------------*
           go to     buf-rig-orf-350.
       buf-rig-orf-330.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx]            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-ofr-num-mag       to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-des    to   w-buf-rig-orf-wde      .
           go to     buf-rig-orf-350.
       buf-rig-orf-340.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx] ma con ti- *
      *                      * po record 13 o 33                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-ofr-num-mag       to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                          *-------------------------------------*
      *                          * Lettura archivio [aaf]              *
      *                          *-------------------------------------*
           move      rf-ofr-tip-mag       to   w-let-arc-aaf-tpm      .
           move      rf-ofr-num-mag       to   w-let-arc-aaf-cdm      .
           move      rf-ofr-cod-arc       to   w-let-arc-aaf-fnt      .
           move      rf-ofr-fda-pif       to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           move      w-let-arc-aaf-des    to   w-buf-rig-orf-wde      .
           go to     buf-rig-orf-350.
       buf-rig-orf-350.
      *                      *-----------------------------------------*
      *                      * Composizione descrizione per riga di    *
      *                      * scroll                                  *
      *                      *-----------------------------------------*
           perform   cpz-des-rgs-000      thru cpz-des-rgs-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wde    to   w-bro-des-rig
                                              (w-bro-num-ele)         .
       buf-rig-orf-352.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           if        rf-ofr-snx-tum       =    "S"
                     move  rf-ofr-nde-tum to   w-bro-dec-qta
                                              (w-bro-num-ele)
           else      move  rf-ofr-dec-qta to   w-bro-dec-qta
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' da evadere                        *
      *                  *---------------------------------------------*
       buf-rig-orf-353.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento : zero   *
      *                      *-----------------------------------------*
           if        w-buf-rig-orf-wtp    =    "C" or
                     w-buf-rig-orf-wtp    =    "A"
                     move  zero           to   w-bro-qta-dev
                                              (w-bro-num-ele)
                     go to buf-rig-orf-360.
       buf-rig-orf-354.
      *                      *-----------------------------------------*
      *                      * Se riga con quantita'                   *
      *                      *-----------------------------------------*
           move      d-qev-rof-qta-dri    to   w-bro-qta-dev
                                              (w-bro-num-ele)         .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     buf-rig-orf-360.
       buf-rig-orf-360.
      *                  *---------------------------------------------*
      *                  * Quantita' in bolla                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-bro-qta-bfo
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di residuo                  *
      *                  *---------------------------------------------*
           move      "<"                  to   w-bro-flg-idr
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di saldo                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-bro-flg-ids
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag di forzatura a saldo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-bro-flg-fzs
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' residua pari alla quantita' da e- *
      *                  * vadere                                      *
      *                  *---------------------------------------------*
           move      w-bro-qta-dev
                    (w-bro-num-ele)       to   w-bro-qta-res
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-bro-snx-aoc
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Data consegna richiesta                     *
      *                  *---------------------------------------------*
           move      rf-ofr-dcn-ric       to   w-bro-dcn-ric
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Formato di stampa data consegna richiesta   *
      *                  *---------------------------------------------*
           move      rf-ofr-fds-dcr       to   w-bro-fds-dcr
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Data consegna prevista                      *
      *                  *---------------------------------------------*
           move      rf-ofr-dcn-prv       to   w-bro-dcn-prv
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag di conferma                            *
      *                  *---------------------------------------------*
           move      rf-ofr-flg-cnf       to   w-bro-flg-cnf
                                              (w-bro-num-ele)         .
       buf-rig-orf-400.
      *              *-------------------------------------------------*
      *              * Eventuale bufferizzazione valuta per legame     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rf-ofr-sgl-vpl       =    spaces
                     go to buf-rig-orf-480.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione buffer : se valuta gia'  *
      *                  * bufferizzata, oltre                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-vpl-ctr-001          .
       buf-rig-orf-420.
           add       1                    to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    w-vpl-num-ele
                     go to buf-rig-orf-440.
           if        rf-ofr-sgl-vpl       =    w-vpl-sgl-vpl
                                              (w-vpl-ctr-001)
                     go to buf-rig-orf-480.
           go to     buf-rig-orf-420.
       buf-rig-orf-440.
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi nel buffer valu- *
      *                  * te                                          *
      *                  *---------------------------------------------*
           if        w-vpl-num-ele        <    w-vpl-max-ele
                     add   1              to   w-vpl-num-ele          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per legame                 *
      *                      *-----------------------------------------*
           move      rf-ofr-sgl-vpl       to   w-vpl-sgl-vpl
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio di riferimento   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione                      *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-ofr-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-ofr-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           if        w-tes-dat-doc (1)    not  = zero
                     move  w-tes-dat-doc (1)
                                          to   w-coe-cmb-vlt-drc
           else      move  w-tes-dat-reg  to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                          *-------------------------------------*
      *                          * Se esito negativo bufferizzazione   *
      *                          * coefficiente contenuto in [ofr]     *
      *                          *-------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ofr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-vpl-cdc-rif
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Data di riferimento cambio              *
      *                      *-----------------------------------------*
           move      w-coe-cmb-vlt-drc    to   w-vpl-dat-rif
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio applicato : si   *
      *                      * forza il cambio di riferimento          *
      *                      *-----------------------------------------*
           move      w-vpl-cdc-rif
                    (w-vpl-num-ele)       to   w-vpl-cdc-vpl
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Percentuale di scostamento              *
      *                      *-----------------------------------------*
           move      zero                 to   w-vpl-per-scs
                                              (w-vpl-num-ele)         .
       buf-rig-orf-480.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [ofr]       *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-200.
       buf-rig-orf-500.
      *              *-------------------------------------------------*
      *              * Test su numero elementi in buffer valute per    *
      *              * legame                                          *
      *              *-------------------------------------------------*
           if        w-vpl-num-ele        not  > 6
                     go to buf-rig-orf-600.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "Numero valute per legame valutario trattate nell'o
      -              "rdine, maggiore"    to   w-err-box-err-msg      .
           move      "di quelle gestibili; per esse verra' forzato il ca
      -              "mbio di rifer. "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
       buf-rig-orf-600.
       buf-rig-orf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-999.
       buf-rig-orf-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *                                                           *
      *    * Subroutine di eventuale ritaratura della quantita' da     *
      *    * ricevere nel caso in cui l'ordine sia gia' stato richia-  *
      *    * mato                                                      *
      *    *-----------------------------------------------------------*
       buf-rig-orf-qdr-000.
      *              *-------------------------------------------------*
      *              * Scansione righe inserite                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio area w-rig                      *
      *                  *---------------------------------------------*
           move      w-rig                to   w-sav-wrk-rig          .
      *                  *---------------------------------------------*
      *                  * Salvataggio area catena                     *
      *                  *---------------------------------------------*
           move      w-cat-rig            to   w-sav-cat-rig          .
       buf-rig-orf-qdr-100.
      *              *-------------------------------------------------*
      *              * Start su catena movimenti                       *
      *              *-------------------------------------------------*
           move      "ST"                 to   w-cat-rig-ope          .
           move      1                    to   w-cat-rig-num          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       buf-rig-orf-qdr-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale catena movimenti            *
      *              *-------------------------------------------------*
           move      "RN"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *                  *---------------------------------------------*
      *                  * Se fine lettura : fine ciclo                *
      *                  *---------------------------------------------*
           if        w-cat-rig-exs        not  = spaces
                     go to buf-rig-orf-qdr-800.
      *                  *---------------------------------------------*
      *                  * Se buffer catena movimenti a spaces : rici- *
      *                  * clo                                         *
      *                  *---------------------------------------------*
           if        w-cat-rig-buf        =    spaces
                     go to buf-rig-orf-qdr-700.
       buf-rig-orf-qdr-300.
      *              *-------------------------------------------------*
      *              * Movimento da buffer catena movimenti a work di  *
      *              * lavoro                                          *
      *              *-------------------------------------------------*
           move      w-cat-rig-buf        to   w-rig                  .
       buf-rig-orf-qdr-400.
      *              *-------------------------------------------------*
      *              * Selezioni sulla riga letta                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo riga a spazi : riciclo              *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig (1)    =    spaces
                     go to buf-rig-orf-qdr-200.
      *                  *---------------------------------------------*
      *                  * Test su riferimenti a ordine fornitore      *
      *                  *---------------------------------------------*
           if        w-rig-orf-prt (1)    not  = rf-ofr-num-prt or
                     w-rig-orf-prg (1)    not  = rf-ofr-num-prg
                     go to buf-rig-orf-qdr-200.
      *                  *---------------------------------------------*
      *                  * Test su segnale di riga comunque saldata    *
      *                  *---------------------------------------------*
           if        w-rig-orf-fzs (1)    =    "S"
                     move  zero           to   d-qev-rof-qta-dri
                     go to buf-rig-orf-qdr-700.
       buf-rig-orf-qdr-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento quantita' da ricevere             *
      *              *-------------------------------------------------*
           if        w-rig-snx-tum (1)    =    "P"
                     subtract w-rig-qta-acq (1)
                                          from d-qev-rof-qta-dri
           else      subtract w-rig-qta-fda (1)
                                          from d-qev-rof-qta-dri      .
       buf-rig-orf-qdr-700.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura righe                         *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-qdr-200.
       buf-rig-orf-qdr-800.
      *              *-------------------------------------------------*
      *              * Ripristino area w-rig                           *
      *              *-------------------------------------------------*
           move      w-sav-wrk-rig        to   w-rig                  .
      *              *-------------------------------------------------*
      *              * Ripristino area catena                          *
      *              *-------------------------------------------------*
           move      w-sav-cat-rig        to   w-cat-rig              .
       buf-rig-orf-qdr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-qdr-999.
       buf-rig-orf-qdr-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *                                                           *
      *    * Subroutine di preparazione castelletto righe in memoria   *
      *    *-----------------------------------------------------------*
       buf-rig-orf-pcr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orf-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di visualizzazione       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-edt-qta-inc-ope      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
       buf-rig-orf-pcr-100.
      *              *-------------------------------------------------*
      *              * Start su file [ofr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-orf-prt    to   rf-ofr-num-prt         .
           move      zero                 to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della start                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orf-pcr-900.
       buf-rig-orf-pcr-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [ofr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orf-pcr-900.
       buf-rig-orf-pcr-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-ofr-num-prt       not  = w-buf-rig-orf-prt
                     go to buf-rig-orf-pcr-900.
       buf-rig-orf-pcr-400.
       buf-rig-orf-pcr-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-rig-orf-ctr      .
      *              *-------------------------------------------------*
      *              * Test sul contatore                              *
      *              *-------------------------------------------------*
           if        w-buf-rig-orf-ctr    >    w-buf-rig-orf-max
                     go to buf-rig-orf-pcr-900.
       buf-rig-orf-pcr-520.
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati per il buffer        *
      *              *-------------------------------------------------*
           perform   buf-rig-orf-obk-000  thru buf-rig-orf-obk-999    .
       buf-rig-orf-pcr-600.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-pcr-200.
       buf-rig-orf-pcr-900.
      *              *-------------------------------------------------*
      *              * Ordinamento finale delle righe bufferizzate     *
      *              *-------------------------------------------------*
           perform   buf-rig-orf-obr-000  thru buf-rig-orf-obr-999    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-pcr-999.
       buf-rig-orf-pcr-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *                                                           *
      *    * Preparazione chiave per bufferizzazione righe             *
      *    *-----------------------------------------------------------*
       buf-rig-orf-obk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare chiave              *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orf-kal
                                              (w-buf-rig-orf-ctr)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare dati                *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orf-prg
                                              (w-buf-rig-orf-ctr)     .
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-ofr-tip-rig       to   w-buf-rig-orf-wtr      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione delle personalizzazioni  *
      *              *-------------------------------------------------*
           if        w-prs-arg-orf-tor    =    "01"
                     go to buf-rig-orf-obk-010
           else if   w-prs-arg-orf-tor    =    "03"
                     go to buf-rig-orf-obk-030
           else if   w-prs-arg-orf-tor    =    "11"
                     go to buf-rig-orf-obk-110
           else if   w-prs-arg-orf-tor    =    "91" and
                     w-acc-tip-ord        =    01
                     go to buf-rig-orf-obk-010
           else if   w-prs-arg-orf-tor    =    "91" and
                     w-acc-tip-ord        =    02
                     go to buf-rig-orf-obk-030
           else if   w-prs-arg-orf-tor    =    "91" and
                     w-acc-tip-ord        =    03
                     go to buf-rig-orf-obk-110
           else      go to buf-rig-orf-obk-010.
       buf-rig-orf-obk-010.
      *              *-------------------------------------------------*
      *              * Se ordinamento per progressivo riga             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-ofr-num-prg       to   w-buf-rig-orf-prg-01
                                              (w-buf-rig-orf-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-rig-orf-obk-900.
       buf-rig-orf-obk-030.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice alfanumerico prodotto *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo riga non "P"                    *
      *                      *-----------------------------------------*
           if        w-buf-rig-orf-wtp    =    "P"
                     go to buf-rig-orf-obk-032.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      all "z"              to   w-buf-rig-orf-alf-03
                                              (w-buf-rig-orf-ctr)     .
      *                      *-----------------------------------------*
      *                      * A bufferizzazione progressivo riga      *
      *                      *-----------------------------------------*
           go to     buf-rig-orf-obk-034.
       buf-rig-orf-obk-032.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-buf-rig-orf-alf-03
                                              (w-buf-rig-orf-ctr)     .
       buf-rig-orf-obk-034.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-ofr-num-prg       to   w-buf-rig-orf-prg-03
                                              (w-buf-rig-orf-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-rig-orf-obk-900.
       buf-rig-orf-obk-110.
      *              *-------------------------------------------------*
      *              * Se ordinamento per ubicazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo riga non "P"                    *
      *                      *-----------------------------------------*
           if        w-buf-rig-orf-wtp    =    "P"
                     go to buf-rig-orf-obk-120.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione ubicazione              *
      *                      *-----------------------------------------*
           move      all "z"              to   w-buf-rig-orf-ubi-11
                                              (w-buf-rig-orf-ctr)     .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-buf-rig-orf-alf-11
                                              (w-buf-rig-orf-ctr)     .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-ofr-num-prg       to   w-buf-rig-orf-prg-11
                                              (w-buf-rig-orf-ctr)     .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     buf-rig-orf-obk-140.
       buf-rig-orf-obk-120.
      *                      *-----------------------------------------*
      *                      * Determinazione ubicazione               *
      *                      *                                         *
      *                      * N.B.: Attualmente si prende il codice   *
      *                      *       dipendenza 01 cioe' la sede       *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-prm-ubi-tip-ope      .
           move      01                   to   d-prm-ubi-cod-dpz      .
           move      01                   to   d-prm-ubi-tip-mag      .
           move      rf-ofr-num-mag       to   d-prm-ubi-num-mag      .
           move      rf-ofr-sgl-vrn       to   d-prm-ubi-var-mag      .
           perform   det-prm-ubi-cll-000  thru det-prm-ubi-cll-999    .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione ubicazione              *
      *                      *-----------------------------------------*
           move      d-prm-ubi-ubi-lit    to   w-buf-rig-orf-ubi-11
                                              (w-buf-rig-orf-ctr)     .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-buf-rig-orf-alf-11
                                              (w-buf-rig-orf-ctr)     .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-ofr-num-prg       to   w-buf-rig-orf-prg-11
                                              (w-buf-rig-orf-ctr)     .
       buf-rig-orf-obk-140.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-rig-orf-obk-900.
       buf-rig-orf-obk-900.
      *              *-------------------------------------------------*
      *              * Preparazione dati di ordinamento                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione progressivo riga            *
      *                  *---------------------------------------------*
           move      rf-ofr-num-prg       to   w-buf-rig-orf-prg
                                              (w-buf-rig-orf-ctr)     .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-obk-999.
       buf-rig-orf-obk-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *                                                           *
      *    * Ordinamento righe bufferizzate                            *
      *    *-----------------------------------------------------------*
       buf-rig-orf-obr-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-buf-rig-orf-ctr    <    2
                     go to buf-rig-orf-obr-999.
       buf-rig-orf-obr-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orf-c01      .
       buf-rig-orf-obr-100.
           add       1                    to   w-buf-rig-orf-c01      .
           if        w-buf-rig-orf-c01    =    w-buf-rig-orf-ctr
                     go to buf-rig-orf-obr-999.
           move      w-buf-rig-orf-c01    to   w-buf-rig-orf-c02
                                               w-buf-rig-orf-c03      .
           move      w-buf-rig-orf-key
                    (w-buf-rig-orf-c01)   to   w-buf-rig-orf-svk      .
       buf-rig-orf-obr-200.
           add       1                    to   w-buf-rig-orf-c02      .
           if        w-buf-rig-orf-c02    >    w-buf-rig-orf-ctr
                     go to buf-rig-orf-obr-300.
           if        w-buf-rig-orf-key
                    (w-buf-rig-orf-c02)   >    w-buf-rig-orf-svk
                     go to buf-rig-orf-obr-200.
           move      w-buf-rig-orf-c02    to   w-buf-rig-orf-c03      .
           move      w-buf-rig-orf-key
                    (w-buf-rig-orf-c02)   to   w-buf-rig-orf-svk      .
           go to     buf-rig-orf-obr-200.
       buf-rig-orf-obr-300.
           move      w-buf-rig-orf-c01    to   w-buf-rig-orf-c04      .          
           if        w-buf-rig-orf-svk    >    w-buf-rig-orf-key
                                              (w-buf-rig-orf-c04)
                     go to buf-rig-orf-obr-100.
           move      w-buf-rig-orf-ele
                    (w-buf-rig-orf-c03)   to   w-buf-rig-orf-ele (999).
           move      w-buf-rig-orf-ele
                    (w-buf-rig-orf-c04)   to   w-buf-rig-orf-ele
                                              (w-buf-rig-orf-c03)     .
           move      w-buf-rig-orf-ele
                    (999)                 to   w-buf-rig-orf-ele
                                              (w-buf-rig-orf-c04)     .
           go to     buf-rig-orf-obr-100.
       buf-rig-orf-obr-999.
           exit.

      *    *===========================================================*
      *    * Composizione descrizione per riga di scroll               *
      *    *-----------------------------------------------------------*
       cpz-des-rgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si sta trattando un co- *
      *              * dice di magazzino oppure no                     *
      *              *-------------------------------------------------*
           if       (w-buf-rig-orf-wtp    =    "P" or
                     w-buf-rig-orf-wtp    =    "M" or
                     w-buf-rig-orf-wtp    =    "V"  ) and
                     w-buf-rig-orf-wtf    =    spaces
                     go to cpz-des-rgs-200.
       cpz-des-rgs-100.
      *              *-------------------------------------------------*
      *              * Se non si sta trattando un codice di magazzino  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, in quanto la descrizione    *
      *                  * gia' composta va' bene                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-200.
      *              *-------------------------------------------------*
      *              * Se si sta trattando un codice di magazzino      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare dell'area di    *
      *                  * lavoro per la composizione                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-des-scr-000          .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione del tipo di visualizzazione  *
      *                  * della riga di scroll                        *
      *                  *---------------------------------------------*
           if        w-prs-rig-scr-des    =    00
                     go to cpz-des-rgs-400
           else if   w-prs-rig-scr-des    =    01
                     go to cpz-des-rgs-420
           else if   w-prs-rig-scr-des    =    02
                     go to cpz-des-rgs-440
           else if   w-prs-rig-scr-des    =    03
                     go to cpz-des-rgs-460
           else if   w-prs-rig-scr-des    =    04
                     go to cpz-des-rgs-480
           else if   w-prs-rig-scr-des    =    05
                     go to cpz-des-rgs-500
           else if   w-prs-rig-scr-des    =    06
                     go to cpz-des-rgs-520
           else if   w-prs-rig-scr-des    =    11
                     go to cpz-des-rgs-540
           else if   w-prs-rig-scr-des    =    12
                     go to cpz-des-rgs-560
           else if   w-prs-rig-scr-des    =    13
                     go to cpz-des-rgs-580
           else if   w-prs-rig-scr-des    =    14
                     go to cpz-des-rgs-590
           else if   w-prs-rig-scr-des    =    21
                     go to cpz-des-rgs-600
           else      go to cpz-des-rgs-400.
       cpz-des-rgs-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 40 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wde    to   w-des-scr-000-d40      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-420.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wde    to   w-des-scr-001-d25      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-001-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-440.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 02              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wde    to   w-des-scr-002-d21      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-002-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wtp    to   w-des-scr-002-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-002-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-002-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-460.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-003-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wde    to   w-des-scr-003-d25      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-480.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-004-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wtp    to   w-des-scr-004-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-004-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-004-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wde    to   w-des-scr-004-d21      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-500.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 05              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-005-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-520.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 06              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-006-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wtp    to   w-des-scr-006-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-006-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-006-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-540.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 11              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-011-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prdotto per fornitore            *
      *                      *-----------------------------------------*
           move      rf-ofr-cop-sfn       to   w-des-scr-011-cdf      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-011-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-560.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 12              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-012-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prdotto per fornitore            *
      *                      *-----------------------------------------*
           move      rf-ofr-cop-sfn       to   w-des-scr-012-cdf      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-012-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wtp    to   w-des-scr-012-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-012-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-012-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-580.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 13              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-013-cod      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-013-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prdotto per fornitore            *
      *                      *-----------------------------------------*
           move      rf-ofr-cop-sfn       to   w-des-scr-013-cdf      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-590.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 14              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-014-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orf-wtp    to   w-des-scr-014-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-014-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-014-cod      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-014-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prdotto per fornitore            *
      *                      *-----------------------------------------*
           move      rf-ofr-cop-sfn       to   w-des-scr-014-cdf      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 21              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ofr-alf-mag       to   w-des-scr-021-cod      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice casa produttrice      *
      *                      *-----------------------------------------*
           move      "[P]"                to   w-des-scr-021-pfp      .
      *                      *-----------------------------------------*
      *                      * Lettura archivio [aaq]                  *
      *                      *-----------------------------------------*
           move      rf-ofr-tip-mag       to   w-let-arc-aaq-tpm      .
           move      rf-ofr-num-mag       to   w-let-arc-aaq-cdm      .
           perform   let-arc-aaq-000      thru let-arc-aaq-999        .
      *                      *-----------------------------------------*
      *                      * Codice prodotto per casa produttrice    *
      *                      *-----------------------------------------*
           move      w-let-arc-aaq-cdp    to   w-des-scr-021-cpp      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-700.
      *                  *---------------------------------------------*
      *                  * Composizione eseguita in area di destina-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-des-scr-000        to   w-buf-rig-orf-wde      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione testata primo ordine fornitore richiamato *
      *    *-----------------------------------------------------------*
       buf-tes-orf-000.
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
      *              * Tentativo di lettura record [ofx]               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ofx]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ofx]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-gen-prt-orf        to   rf-ofx-num-prt         .
           move      zero                 to   rf-ofx-num-prg         .
           move      01                   to   rf-ofx-tip-rec         .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
      *              *-------------------------------------------------*
      *              * Flag di accettazione tipo archivio              *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-fda-tar (1)      .
      *              *-------------------------------------------------*
      *              * Flag di accettazione codice archivio            *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-fda-car (1)      .
      *              *-------------------------------------------------*
      *              * Flag di accettazione sigla valuta               *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-fda-vpf (1)      .
      *              *-------------------------------------------------*
      *              * Flag di accettazione assoggettamento iva        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-fda-aiv (1)      .
       buf-tes-orf-100.
      *              *-------------------------------------------------*
      *              * Valori testata contenuti direttamente in record *
      *              * [oft]                                           *
      *              *-------------------------------------------------*
           move      rf-oft-tip-arc       to   w-tes-tip-arc (1)      .
           move      rf-oft-cod-arc       to   w-tes-cod-arc (1)      .
           move      rf-oft-dpz-arc       to   w-tes-dpz-arc (1)      .
           move      rf-oft-cod-lng       to   w-tes-cod-lng (1)      .
           move      rf-oft-tip-ids       to   w-tes-tip-ids (1)      .
           move      rf-oft-inl-pgt       to   w-tes-inl-pgt (1)      .
           move      rf-oft-sgl-vpf       to   w-tes-sgl-vpf (1)      .
           move      rf-oft-dec-vpf       to   w-tes-dec-vpf (1)      .
           move      rf-oft-tdc-vpf       to   w-tes-tdc-vpf (1)      .
           move      rf-oft-ass-iva       to   w-tes-ass-iva (1)      .
           move      rf-oft-ctp-acq       to   w-tes-ctp-acq (1)      .
           move      rf-oft-voc-des (1)   to   w-tes-voc-des (1, 1)   .
           move      rf-oft-voc-des (2)   to   w-tes-voc-des (1, 2)   .
           move      rf-oft-voc-des (3)   to   w-tes-voc-des (1, 3)   .
           move      rf-oft-voc-des (4)   to   w-tes-voc-des (1, 4)   .
           move      rf-oft-voc-des (5)   to   w-tes-voc-des (1, 5)   .
           move      rf-oft-voc-des (6)   to   w-tes-voc-des (1, 6)   .
           move      rf-oft-cod-lst       to   w-tes-cod-lst (1)      .
           move      rf-oft-csr-aaf       to   w-tes-csr-aaf (1)      .
           move      rf-oft-psr-aaf (1)   to   w-tes-psr-aaf (1, 1)   .
           move      rf-oft-psr-aaf (2)   to   w-tes-psr-aaf (1, 2)   .
           move      rf-oft-psr-aaf (3)   to   w-tes-psr-aaf (1, 3)   .
           move      rf-oft-psr-aaf (4)   to   w-tes-psr-aaf (1, 4)   .
           move      rf-oft-psr-aaf (5)   to   w-tes-psr-aaf (1, 5)   .
           move      rf-oft-csc-aaf       to   w-tes-csc-aaf (1)      .
           move      rf-oft-psc-aaf       to   w-tes-psc-aaf (1)      .
           move      rf-oft-cod-aqt       to   w-tes-cod-aqt (1)      .
           move      rf-oft-pvf-aqt       to   w-tes-pvf-aqt (1)      .
           move      rf-oft-cod-ime       to   w-tes-cod-ime (1)      .
           move      rf-oft-pvf-ime       to   w-tes-pvf-ime (1)      .
           move      rf-oft-cod-fop       to   w-tes-cod-fop (1)      .
           move      rf-oft-scp-aap       to   w-tes-scp-aap (1)      .
           move      rf-oft-nos-ban       to   w-tes-nos-ban (1)      .
           move      rf-oft-cod-abi       to   w-tes-cod-abi (1)      .
           move      rf-oft-cod-cab       to   w-tes-cod-cab (1)      .
           move      rf-oft-ccc-app       to   w-tes-ccc-app (1)      .
           move      rf-oft-ccp-app       to   w-tes-ccp-app (1)      .
           move      rf-oft-add-spi       to   w-tes-add-spi (1)      .
           move      rf-oft-add-spb       to   w-tes-add-spb (1)      .
           move      rf-oft-pag-dsm       to   w-tes-pag-dsm (1)      .
           move      rf-oft-pag-qaf       to   w-tes-pag-qaf (1)      .
           move      rf-oft-pag-act       to   w-tes-pag-act (1)      .
           move      rf-oft-ipr-iel       to   w-tes-ipr-iel (1)      .
      *              *-------------------------------------------------*
      *              * Valori piede contenuti direttamente in record   *
      *              * [oft]                                           *
      *              *-------------------------------------------------*
           move      rf-oft-tot-scc       to   w-pie-tot-scc (1)      .
           move      rf-oft-per-scc       to   w-pie-per-scc (1)      .
           move      rf-oft-tot-scp       to   w-pie-tot-scp (1)      .
           move      rf-oft-per-scp       to   w-pie-per-scp (1)      .
           move      zero                 to   w-buf-tes-orf-ctr      .
       buf-tes-orf-110.
           add       1                    to   w-buf-tes-orf-ctr      .
           if        w-buf-tes-orf-ctr    >    6
                     go to buf-tes-orf-120.
           move      rf-oft-spe-snx
                    (w-buf-tes-orf-ctr)   to   w-pie-spe-snx
                                              (1, w-buf-tes-orf-ctr)  .
           move      rf-oft-spe-mad
                    (w-buf-tes-orf-ctr)   to   w-pie-spe-mad
                                              (1, w-buf-tes-orf-ctr)  .
           move      rf-oft-spe-per
                    (w-buf-tes-orf-ctr)   to   w-pie-spe-per
                                              (1, w-buf-tes-orf-ctr)  .
           move      rf-oft-spe-ibl
                    (w-buf-tes-orf-ctr)   to   w-pie-spe-ibl
                                              (1, w-buf-tes-orf-ctr)  .
           move      rf-oft-ibt-spe
                    (w-buf-tes-orf-ctr)   to   w-pie-spe-ibt
                                              (1, w-buf-tes-orf-ctr)  .
           move      rf-oft-spe-imp
                    (w-buf-tes-orf-ctr)   to   w-pie-spe-imp
                                              (1, w-buf-tes-orf-ctr)  .
           go to     buf-tes-orf-110.
       buf-tes-orf-120.
      *              *-------------------------------------------------*
      *              * Valori contenuti indirettamente in record [oft] *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anagrafica archivio                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [fnt]                  *
      *                      *-----------------------------------------*
           move      w-tes-cod-arc (1)    to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
           move      w-let-arc-fnt-rag    to   w-tes-cod-arc-rag (1)  .
           move      w-let-arc-fnt-via    to   w-tes-cod-arc-via (1)  .
           move      w-let-arc-fnt-loc    to   w-tes-cod-arc-loc (1)  .
           move      w-let-arc-fnt-piv    to   w-tes-cod-arc-piv (1)  .
           move      w-let-arc-fnt-stc    to   w-tes-cod-arc-stc (1)  .
      *                      *-----------------------------------------*
      *                      * Test su codice sottoconto associato al  *
      *                      * fornitore                               *
      *                      *-----------------------------------------*
           if        w-tes-cod-arc-stc (1)
                                          not  = zero
                     go to buf-tes-orf-130.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca il sottoconto contabile per il fornitore !  
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orf-900.
       buf-tes-orf-130.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcf] principale             *
      *                  *---------------------------------------------*
           move      w-tes-cod-arc (1)    to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to buf-tes-orf-140.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale per il fornitore ! 
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orf-900.
       buf-tes-orf-140.
      *                  *---------------------------------------------*
      *                  * Determinazione valori testata indotti da    *
      *                  * record [dcf] principale                     *
      *                  *---------------------------------------------*
           perform   det-vlt-dcf-000      thru det-vlt-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Anagrafica dipendenza fornitore             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura file [dcf]                      *
      *                      *-----------------------------------------*
           move      w-tes-cod-arc (1)    to   w-let-arc-dcf-fnt      .
           move      w-tes-dpz-arc (1)    to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
           move      w-let-arc-dcf-rag    to   w-tes-dpz-arc-rag (1)  .
           move      w-let-arc-dcf-via    to   w-tes-dpz-arc-via (1)  .
           move      w-let-arc-dcf-loc    to   w-tes-dpz-arc-loc (1)  .
      *                  *---------------------------------------------*
      *                  * Determinazione valori testata indotti da    *
      *                  * record [dcf] relativo alla dipendenza del   *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           perform   det-vlt-dcd-000      thru det-vlt-dcd-999        .
       buf-tes-orf-200.
      *                  *---------------------------------------------*
      *                  * Se il documento non interessa la fattura-   *
      *                  * zione : a fine bufferizzazione valori te-   *
      *                  * stata                                       *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to buf-tes-orf-320.
      *                  *---------------------------------------------*
      *                  * Anagrafica valuta                           *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-sgl-vpf-des (1)  .
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio valuta per fattura-  *
      *                  * zione alla data del documento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-tes-sgl-vpf (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-tes-tdc-vpf (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-reg        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                      *-----------------------------------------*
      *                      * Test su esito determinazione            *
      *                      *-----------------------------------------*
           if        w-coe-cmb-vlt-ope    =    "CC"
                     go to buf-tes-orf-207.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Coefficiente di cambio indeterminato !            
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orf-900.
       buf-tes-orf-207.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione coefficiente di cambio  *
      *                      *-----------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tes-cdc-vpf (1)      .
       buf-tes-orf-210.
      *                  *---------------------------------------------*
      *                  * Tabella codici iva per descrizione assog-   *
      *                  * gettamento                                  *
      *                  *---------------------------------------------*
           move      w-tes-ass-iva (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-ass-iva-des (1)  .
           if        w-tes-ass-iva (1)    =    zero
                     move  "Soggetto ad iva"
                                          to   w-tes-ass-iva-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica piano dei conti                  *
      *                  *---------------------------------------------*
           move      w-tes-ctp-acq (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-acq-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica forme di pagamento               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fop (1)    to   w-let-arc-yfp-cod      .
           perform   let-arc-yfp-000      thru let-arc-yfp-999        .
           move      w-let-arc-yfp-des    to   w-tes-cod-fop-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica nostre banche                    *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ban (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ban-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica istituti di credito              *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-cod-abi-den (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica sportelli istituti di credito    *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cod-cab (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cod-cab-den (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria spese incasso          *
      *                  *---------------------------------------------*
           move      w-tes-add-spi (1)    to   w-let-arc-yin-cod      .
           move      00                   to   w-let-arc-yin-tpg      .
           perform   let-arc-yin-000      thru let-arc-yin-999        .
           move      w-let-arc-yin-des    to   w-tes-add-spi-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria spese bollo            *
      *                  *---------------------------------------------*
           move      w-tes-add-spb (1)    to   w-let-arc-ybo-cod      .
           move      00                   to   w-let-arc-ybo-tpg      .
           perform   let-arc-ybo-000      thru let-arc-ybo-999        .
           move      w-let-arc-ybo-des    to   w-tes-add-spb-des (1)  .
       buf-tes-orf-300.
      *                  *---------------------------------------------*
      *                  * Codici iva e contropartite spese in fattura *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tes-orf-ctr      .
       buf-tes-orf-310.
           add       1                    to   w-buf-tes-orf-ctr      .
           if        w-buf-tes-orf-ctr    >    6
                     go to buf-tes-orf-320.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [ysf]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [ysf]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-buf-tes-orf-ctr    to   rf-ysf-num-spf         .
           move      "I  "                to   rf-ysf-cod-lng         .
           move      "pgm/dcf/fls/ioc/obj/iofysf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ysf                 .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori                  *
      *                      *-----------------------------------------*
           move      rf-ysf-civ-spe       to   w-pie-spe-civ
                                              (1, w-buf-tes-orf-ctr)  .
           move      rf-ysf-ccp-spe       to   w-pie-spe-ccp
                                              (1, w-buf-tes-orf-ctr)  .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     buf-tes-orf-310.
       buf-tes-orf-320.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-tes-orf-999.
       buf-tes-orf-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con status ad errore                 *
      *                  *---------------------------------------------*
           move      "#"                  to   l-eva-orf-exi-sts      .
       buf-tes-orf-999.
           exit.

      *    *===========================================================*
      *    * Confronto tra dati testata ordine fornitore in esame con  *
      *    * dati testata del documento                                *
      *    *-----------------------------------------------------------*
       cnf-tes-doc-000.
      *              *-------------------------------------------------*
      *              * Valori bloccanti                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori comunque bloccanti                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo e codice archivio                  *
      *                      *-----------------------------------------*
           if        rf-oft-tip-arc       =    w-tes-tip-arc (1) and
                     rf-oft-cod-arc       =    w-tes-cod-arc (1)
                     go to cnf-tes-doc-010.
           move      "Codice fornitore diverso da quello impostato !    
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-010.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che il tipo movimento  *
      *                  * per la bolla interessi o meno la fattura-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to cnf-tes-doc-020
           else      go to cnf-tes-doc-040.
       cnf-tes-doc-020.
      *                  *---------------------------------------------*
      *                  * Se non interessa la fatturazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessun valore bloccante                 *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-100.
       cnf-tes-doc-040.
      *                  *---------------------------------------------*
      *                  * Se bolla fatturabile                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su assoggettamento iva             *
      *                      *-----------------------------------------*
           if        rf-oft-ass-iva       =    w-tes-ass-iva (1)
                     go to cnf-tes-doc-044.
           move      "Assoggettamento iva diverso da quello impostato ! 
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-044.
      *                      *-----------------------------------------*
      *                      * Test su inoltro pagamenti               *
      *                      *-----------------------------------------*
           if        rf-oft-inl-pgt       =    w-tes-inl-pgt (1)
                     go to cnf-tes-doc-048.
           move      "Inoltro pagamenti diverso da quello impostato !   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-048.
      *                      *-----------------------------------------*
      *                      * Test su sigla valuta per fatturazione   *
      *                      *-----------------------------------------*
           if        rf-oft-sgl-vpf       =    w-tes-sgl-vpf (1)
                     go to cnf-tes-doc-050.
           move      "Sigla valuta per fatturazione diversa da quella im
      -              "postata !      "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-050.
      *                      *-----------------------------------------*
      *                      * Test su decimali valuta per fatturazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        rf-oft-dec-vpf       =    w-tes-dec-vpf (1)
                     go to cnf-tes-doc-052.
           move      "Numero decimali valuta per fatturazione diverso da
      -              " quello della  "    to   w-err-box-err-msg      .
           move      " valuta impostata !                               
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           go to     cnf-tes-doc-950.
       cnf-tes-doc-052.
      *                      *-----------------------------------------*
      *                      * Test su tipo di cambio valuta per fat-  *
      *                      * turazione                               *
      *                      *-----------------------------------------*
           if        rf-oft-tdc-vpf       =    w-tes-tdc-vpf (1)
                     go to cnf-tes-doc-053.
           move      "Tipo di cambio valuta per fatturazione diverso da 
      -              "quello della   "    to   w-err-box-err-msg      .
           move      " valuta impostata !                               
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           go to     cnf-tes-doc-950.
       cnf-tes-doc-053.
      *                      *-----------------------------------------*
      *                      * Test su data scadenza pagamento manuale *
      *                      *-----------------------------------------*
           if        rf-oft-pag-dsm       =    w-tes-pag-dsm (1)
                     go to cnf-tes-doc-054.
           move      "Data scadenza pagamento manuale diversa da quella 
      -              "impostata !    "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-054.
      *                      *-----------------------------------------*
      *                      * Test su quota a forfait pagamento       *
      *                      *-----------------------------------------*
           if        rf-oft-pag-qaf       =    w-tes-pag-qaf (1)
                     go to cnf-tes-doc-056.
           move      "Quota a forfait del pagamento diversa da quella im
      -              "postata !      "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-056.
           go to     cnf-tes-doc-100.
       cnf-tes-doc-100.
      *              *-------------------------------------------------*
      *              * Valori non-bloccanti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori comunque non bloccanti               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su dipendenza archivio             *
      *                      *-----------------------------------------*
           if        rf-oft-dpz-arc       =    w-tes-dpz-arc (1)
                     go to cnf-tes-doc-110.
      *                      *-----------------------------------------*
      *                      * Messaggio all'operatore                 *
      *                      *-----------------------------------------*
           move      "Dipendenza fornitore diversa da quella impostata !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-110.
      *                      *-----------------------------------------*
      *                      * Codice lingua                           *
      *                      *-----------------------------------------*
           if        rf-oft-cod-lng       =    w-tes-cod-lng (1)
                     go to cnf-tes-doc-120.
           move      "Codice lingua diverso da quello impostato !       
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-120.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che il tipo movimento  *
      *                  * per la bolla interessi o meno la fattura-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to cnf-tes-doc-140
           else      go to cnf-tes-doc-160.
       cnf-tes-doc-140.
      *                  *---------------------------------------------*
      *                  * Se non interessa la fatturazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessun valore non-bloccante             *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-500.
       cnf-tes-doc-160.
      *                  *---------------------------------------------*
      *                  * Se bolla fatturabile                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contropartita acquisti          *
      *                      *-----------------------------------------*
           if        rf-oft-ctp-acq       =    w-tes-ctp-acq (1)
                     go to cnf-tes-doc-162.
           move      "Contropartita acquisti diversa da quella impostata
      -              " !             "    to   w-err-box-err-msg      .
       cnf-tes-doc-162.
      *                      *-----------------------------------------*
      *                      * Test su forma di pagamento              *
      *                      *-----------------------------------------*
           if        rf-oft-cod-fop       =    w-tes-cod-fop (1)
                     go to cnf-tes-doc-164.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Forma di pagamento diversa da quella impostata !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-164.
      *                      *-----------------------------------------*
      *                      * Test su codice ABI d'appoggio           *
      *                      *-----------------------------------------*
           if        w-tes-cod-abi (1)    =    zero
                     go to cnf-tes-doc-166.
           if        rf-oft-cod-abi       =    w-tes-cod-abi (1)
                     go to cnf-tes-doc-166.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Codice ABI per l'appoggio diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-166.
      *                      *-----------------------------------------*
      *                      * Test su codice CAB d'appoggio           *
      *                      *-----------------------------------------*
           if        w-tes-cod-cab (1)    =    zero
                     go to cnf-tes-doc-168.
           if        rf-oft-cod-cab       =    w-tes-cod-cab (1)
                     go to cnf-tes-doc-168.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Codice CAB per l'appoggio diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-168.
      *                      *-----------------------------------------*
      *                      * Test su c/c d'appoggio                  *
      *                      *-----------------------------------------*
           if        w-tes-ccc-app (1)    =    spaces
                     go to cnf-tes-doc-170.
           if        rf-oft-ccc-app       =    w-tes-ccc-app (1)
                     go to cnf-tes-doc-170.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "C/C del fornitore per l'appoggio diverso da quello
      -              " impostato !  "     to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-170.
      *                      *-----------------------------------------*
      *                      * Test su nostra banca per bonifico       *
      *                      *-----------------------------------------*
           if        w-tes-nos-ban (1)    =    spaces
                     go to cnf-tes-doc-172.
           if        rf-oft-nos-ban       =    w-tes-nos-ban (1)
                     go to cnf-tes-doc-172.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Nostra banca per appoggio bonifico diversa da quel
      -              "la impostata ! "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-172.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-500.
       cnf-tes-doc-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi per testata documento *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Acconto pagamento per il documento          *
      *                  *---------------------------------------------*
           add       rf-oft-pag-act       to   w-tes-pag-act (1)      .
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi per piede documento   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale sconto in chiusura                   *
      *                  *---------------------------------------------*
           add       rf-oft-tot-scc       to   w-pie-tot-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-oft-per-scc       not  = w-pie-per-scc (1)
                     move  zero           to   w-pie-per-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Totale sconto pagamento                     *
      *                  *---------------------------------------------*
           add       rf-oft-tot-scp       to   w-pie-tot-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-oft-per-scp       not  = w-pie-per-scp (1)
                     move  zero           to   w-pie-per-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Totale spese in fattura                     *
      *                  *---------------------------------------------*
           add       rf-oft-spe-imp (1)   to   w-pie-spe-imp (1, 1)   .
           add       rf-oft-spe-imp (2)   to   w-pie-spe-imp (1, 2)   .
           add       rf-oft-spe-imp (3)   to   w-pie-spe-imp (1, 3)   .
           add       rf-oft-spe-imp (4)   to   w-pie-spe-imp (1, 4)   .
           add       rf-oft-spe-imp (5)   to   w-pie-spe-imp (1, 5)   .
           add       rf-oft-spe-imp (6)   to   w-pie-spe-imp (1, 6)   .
      *                  *---------------------------------------------*
      *                  * Percentuali spese in fattura                *
      *                  *---------------------------------------------*
           if        rf-oft-spe-snx (1)   =    0 or
                     rf-oft-spe-per (1)   not  = w-pie-spe-per (1, 1)
                     move  zero           to   w-pie-spe-per (1, 1)   .
           if        rf-oft-spe-snx (2)   =    0 or
                     rf-oft-spe-per (2)   not  = w-pie-spe-per (1, 2)
                     move  zero           to   w-pie-spe-per (1, 2)   .
           if        rf-oft-spe-snx (3)   =    0 or
                     rf-oft-spe-per (3)   not  = w-pie-spe-per (1, 3)
                     move  zero           to   w-pie-spe-per (1, 3)   .
           if        rf-oft-spe-snx (4)   =    0 or
                     rf-oft-spe-per (4)   not  = w-pie-spe-per (1, 4)
                     move  zero           to   w-pie-spe-per (1, 4)   .
           if        rf-oft-spe-snx (5)   =    0 or
                     rf-oft-spe-per (5)   not  = w-pie-spe-per (1, 5)
                     move  zero           to   w-pie-spe-per (1, 5)   .
           if        rf-oft-spe-snx (6)   =    0 or
                     rf-oft-spe-per (6)   not  = w-pie-spe-per (1, 6)
                     move  zero           to   w-pie-spe-per (1, 6)   .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cnf-tes-doc-999.
       cnf-tes-doc-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-950.
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   l-eva-orf-exi-sts      .
       cnf-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine fornitore            *
      *    *-----------------------------------------------------------*
       sdc-eva-orf-000.
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
                     move  "#"            to   l-eva-orf-exi-sts      .
       sdc-eva-orf-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine fornitore            *
      *    *-----------------------------------------------------------*
       acc-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-sdc-flg-exi          .
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagine            *
      *              *-------------------------------------------------*
           move      w-bro-num-ele        to   w-sdc-npg-max          .
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
      *              * Numero riga da trattare                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-ctr-rig          .
       acc-fun-sdc-200.
           add       1                    to   w-sdc-ctr-rig          .
           if        w-sdc-ctr-rig        >    w-bro-num-ele
                     go to  acc-fun-sdc-800.
       acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina da trattare        *
      *              *-------------------------------------------------*
           divide    14                   into w-sdc-ctr-rig
                                        giving w-sdc-npg-dat
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-dat          .
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
      *              *-------------------------------------------------*
      *              * Se Tab in corso                                 *
      *              *-------------------------------------------------*
           if        l-eva-orf-fky-tab    =    spaces
                     go to acc-fun-sdc-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di Tab in corso        *
      *                  *---------------------------------------------*
           move      spaces               to   l-eva-orf-fky-tab      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Visualizzazione note a piede saldaconto         *
      *              *-------------------------------------------------*
           perform   vis-not-sdc-000      thru vis-not-sdc-999        .
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Quantita' in bolla                          *
      *                  *---------------------------------------------*
           perform   acc-qta-bfo-000      thru acc-qta-bfo-999        .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           perform   acc-snx-aoc-000      thru acc-snx-aoc-999        .
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
                     go to acc-fun-sdc-920.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     subtract 1           from w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to  acc-fun-sdc-200.
      *                      *-----------------------------------------*
      *                      * Se Next Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to  acc-fun-sdc-500.
           multiply  14                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           add       1                    to   w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-500.
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
                     move   w-bro-num-ele to   w-sdc-ctr-rig
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
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
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
      *                  *---------------------------------------------*
      *                  * Cancellazione note operative                *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-fun-sdc-999.
       acc-fun-sdc-920.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione note operative                *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sdc-flg-exi          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Tipo                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      w-acc-tmo-orf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-acc-tmo-orf-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Data:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "Numero:"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
       pmt-fun-sdc-320.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione del tipo di visualizzazione  *
      *                  * della riga di scroll                        *
      *                  *---------------------------------------------*
           if        w-prs-rig-scr-des    =    00
                     go to pmt-fun-sdc-340
           else if   w-prs-rig-scr-des    =    01
                     go to pmt-fun-sdc-360
           else if   w-prs-rig-scr-des    =    02
                     go to pmt-fun-sdc-380
           else if   w-prs-rig-scr-des    =    03
                     go to pmt-fun-sdc-400
           else if   w-prs-rig-scr-des    =    04
                     go to pmt-fun-sdc-420
           else if   w-prs-rig-scr-des    =    05
                     go to pmt-fun-sdc-440
           else if   w-prs-rig-scr-des    =    06
                     go to pmt-fun-sdc-460
           else if   w-prs-rig-scr-des    =    11
                     go to pmt-fun-sdc-480
           else if   w-prs-rig-scr-des    =    12
                     go to pmt-fun-sdc-500
           else if   w-prs-rig-scr-des    =    13
                     go to pmt-fun-sdc-520
           else if   w-prs-rig-scr-des    =    14
                     go to pmt-fun-sdc-540
           else      go to pmt-fun-sdc-340.
       pmt-fun-sdc-340.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               Descrizione              |Da evader
      -              "e | In bolla  | I |  Residuo  "
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
           move      "----------------------------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-360.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Descrizione            Codice    |Da evader
      -              "e | In bolla  | I |  Residuo  "
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
           move      "------------------------- --------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-380.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 02              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     Descrizione            Codice      |Da evader
      -              "e | In bolla  | I |  Residuo  "
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
           move      "--------------------- ------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    Codice            Descrizione       |Da evader
      -              "e | In bolla  | I |  Residuo  "
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
           move      "-------------- -------------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-420.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      Codice            Descrizione     |Da evader
      -              "e | In bolla  | I |  Residuo  "
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
           move      "------------------ ---------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-440.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 05              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Descrizione            Codice    |Da evader
      -              "e | In bolla  | I |  Residuo  "
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
           move      "------------------------- --------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-460.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 06              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     Descrizione            Codice      |Da evader
      -              "e | In bolla  | I |  Residuo  "
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
           move      "--------------------- ------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-480.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 11              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  Codice del fornitore        Codice    |Da evader
      -              "e | In Bolla  | I |  Residuo  "
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
           move      "------------------------- --------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-500.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 12              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " Codice del fornitore       Codice      |Da evader
      -              "e | In Bolla  | I |  Residuo  "
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
           move      "--------------------- ------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-520.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 13              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    Codice       Codice del fornitore   |Da evader
      -              "e | In Bolla  | I |  Residuo  "
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
           move      "-------------- -------------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-540.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 14              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      Codice        Codice del fornitore|Da evader
      -              "e | In Bolla  | I |  Residuo  "
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
           move      "------------------ ---------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-600.
       pmt-fun-sdc-600.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione del tipo di visualizzazione  *
      *                  * colonna residuo residuo della riga di scroll*
      *                  *---------------------------------------------*
           if        w-prs-rig-scr-res    =    00
                     go to pmt-fun-sdc-610
           else if   w-prs-rig-scr-res    =    01
                     go to pmt-fun-sdc-620.
       pmt-fun-sdc-610.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-620.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Correzione fincatura                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "Rich. Prev."        to   v-alf                  .
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
           move      "                                        |         
      -              "  |           |   |           "
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
           if        w-sdc-wrk-rig        >    w-bro-num-ele
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
           move      "                                        |         
      -              "  |           |   |           "
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
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-bro-des-rig
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' da evadere                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita' da incolonnare    *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-bro-qta-dev
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-bro-dec-qta
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      42                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-bro-tip-rig-tpr
                    (w-sdc-wrk-rig)       not  = "C" and
                     w-bro-tip-rig-tpr
                    (w-sdc-wrk-rig)       not  = "A"
                     go to vis-pag-sdc-320.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      59                   to   v-pos                  .
           if        w-bro-snx-aoc
                    (w-sdc-wrk-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag indicatori a spazi                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      66                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ultima colonna a spazi                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      70                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A riciclo su riga successiva            *
      *                      *-----------------------------------------*
           go to     vis-pag-sdc-400.
       vis-pag-sdc-320.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' in bolla                  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-bro-dec-qta
                    (w-sdc-wrk-rig)       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      54                   to   v-pos                  .
           move      w-bro-qta-bfo
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Flag indicatore di residuo          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      66                   to   v-pos                  .
           move      w-bro-flg-idr
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Flag indicatore di saldo            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      68                   to   v-pos                  .
           move      w-bro-flg-ids
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-340.
      *                          *-------------------------------------*
      *                          * Colonna residuo                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione in funzione della    *
      *                              * personalizzazione               *
      *                              *---------------------------------*
           if        w-prs-rig-scr-res    =    00
                     go to vis-pag-sdc-342
           else if   w-prs-rig-scr-res    =    01
                     go to vis-pag-sdc-344.
       vis-pag-sdc-342.
      *                              *---------------------------------*
      *                              * Personalizzazione : Q.ta' resi- *
      *                              * dua                             *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "D"                  to   v-edm                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      70                   to   v-pos                  .
           move      w-bro-qta-res
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-pag-sdc-400.
       vis-pag-sdc-344.
      *                              *---------------------------------*
      *                              * Personalizzazione : Data conse- *
      *                              * gna richiesta e prevista        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Data consegna richiesta     *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Editing                 *
      *                                      *-------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "<"                  to   v-edm                  .
           move      w-bro-dcn-ric
                    (w-sdc-wrk-rig)       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * Stampa                  *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      70                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Formato di stampa data con- *
      *                                  * segna richiesta             *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      75                   to   v-pos                  .
           move      w-bro-fds-dcr
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Data consegna prevista      *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Editing                 *
      *                                      *-------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "<"                  to   v-edm                  .
           move      w-bro-dcn-prv
                    (w-sdc-wrk-rig)       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * Stampa                  *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      76                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
      *    * Visualizzazione note a piede saldaconto                   *
      *    *-----------------------------------------------------------*
       vis-not-sdc-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo riga              *
      *              *-------------------------------------------------*
           if        w-bro-tip-rig-tpr
                    (w-sdc-ctr-rig)       =    "P"    and
                     w-bro-tip-rig-tfu
                    (w-sdc-ctr-rig)       =    spaces
                     go to vis-not-sdc-100
           else      go to vis-not-sdc-200.
       vis-not-sdc-100.
      *              *-------------------------------------------------*
      *              * Tipo riga : Prodotto di vendita                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [dcp]                        *
      *                  *---------------------------------------------*
           move      w-bro-num-mag
                    (w-sdc-ctr-rig)       to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Composizione area v-not                     *
      *                  *---------------------------------------------*
           string    "Ns. Codice : "
                                delimited by   size
                     w-let-arc-dcp-alf
                                delimited by   spaces
                     "  " 
                                delimited by   size
                     w-let-arc-dcp-dui
                                delimited by   size
                                          into v-nt2                  .
      *                  *---------------------------------------------*
      *                  * A visualizzazione                           *
      *                  *---------------------------------------------*
           go to     vis-not-sdc-800.
       vis-not-sdc-200.
      *              *-------------------------------------------------*
      *              * Tipo riga : Altro                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione area v-not a spaces            *
      *                  *---------------------------------------------*
           move      spaces               to   v-not                  .
      *                  *---------------------------------------------*
      *                  * A visualizzazione                           *
      *                  *---------------------------------------------*
           go to     vis-not-sdc-800.
       vis-not-sdc-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione note                            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-not-sdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo saldaconto : Quantita' in bolla        *
      *    *-----------------------------------------------------------*
       acc-qta-bfo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-bro-tip-rig-tpr
                    (w-sdc-ctr-rig)       =    "A" or
                     w-bro-tip-rig-tpr
                    (w-sdc-ctr-rig)       =    "C"
                     go to acc-qta-bfo-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-bro-qta-bfo
                    (w-sdc-ctr-rig)       to   w-sav-qta-bfo          .
           move      w-bro-flg-fzs
                    (w-sdc-ctr-rig)       to   w-sav-flg-fzs          .
       acc-qta-bfo-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-bro-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-bro-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-bro-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "[1] "               to   v-pfk (14)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-bro-qta-bfo
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-bfo-999.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-qta-bfo-200.
      *                  *---------------------------------------------*
      *                  * Controllo che il valore non sia stato va-   *
      *                  * riato                                       *
      *                  *---------------------------------------------*
           if        v-mod                not  = spaces
                     move  w-sav-qta-bfo  to   w-bro-qta-bfo
                                              (w-sdc-ctr-rig)
                     move  w-sav-flg-fzs  to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)
                     go to acc-qta-bfo-100.
      *                  *---------------------------------------------*
      *                  * Esecuzione ricerca per codice magazzino     *
      *                  *---------------------------------------------*
           perform   exe-rca-pcm-000      thru exe-rca-pcm-999        .
      *                  *---------------------------------------------*
      *                  * Se esito negativo : a reimpostazione        *
      *                  *---------------------------------------------*
           if        w-exe-rca-pcm-nrg    =    zero
                     go to acc-qta-bfo-100.
      *                  *---------------------------------------------*
      *                  * Determinazione numero riga su cui posizio-  *
      *                  * narsi                                       *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-nrg    to   w-sdc-ctr-rig          .
           subtract  1                    from w-sdc-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Simulazione tasto Down                      *
      *                  *---------------------------------------------*
           move      "DOWN"               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-qta-bfo-999.
       acc-qta-bfo-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-bro-qta-bfo
                                              (w-sdc-ctr-rig)         .
       acc-qta-bfo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo compatibilita' fra segno quantita'*
      *                  * in bolla e segno quantita' da evadere       *
      *                  *---------------------------------------------*
           if        w-bro-qta-dev
                    (w-sdc-ctr-rig)       >    zero and
                     w-bro-qta-bfo
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-bfo-100
           else if   w-bro-qta-dev
                    (w-sdc-ctr-rig)       <    zero and
                     w-bro-qta-bfo
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-bfo-100.
      *                  *---------------------------------------------*
      *                  * Se Pf1 : controllo che il valore impostato  *
      *                  * non sia zero                                *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-qta-bfo-420.
       acc-qta-bfo-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-bfo-600.
           if        w-bro-qta-bfo
                    (w-sdc-ctr-rig)       not  = w-sav-qta-bfo
                     move   w-sav-qta-bfo to   w-bro-qta-bfo
                                              (w-sdc-ctr-rig)
                     go to  acc-qta-bfo-100.
       acc-qta-bfo-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' da evadere e quantita' in bolla   *
      *                  * in work di comodo senza segno               *
      *                  *---------------------------------------------*
           move      w-bro-qta-dev
                    (w-sdc-ctr-rig)       to   w-qss-qta-dev          .
           move      w-bro-qta-bfo
                    (w-sdc-ctr-rig)       to   w-qss-qta-bfo          .
      *                  *---------------------------------------------*
      *                  * Se Slct                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-qta-bfo-620.
      *                      *-----------------------------------------*
      *                      * Se valore gia' presente                 *
      *                      *-----------------------------------------*
           if        w-bro-qta-bfo
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-bfo-610.
      *                          *-------------------------------------*
      *                          * Quantita' in bolla                  *
      *                          *-------------------------------------*
           move      zero                 to   w-bro-qta-bfo
                                              (w-sdc-ctr-rig)         .
           move      zero                 to   w-qss-qta-bfo          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-700.
       acc-qta-bfo-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' in bolla                  *
      *                          *-------------------------------------*
           move      w-bro-qta-dev
                    (w-sdc-ctr-rig)       to   w-bro-qta-bfo
                                              (w-sdc-ctr-rig)         .
           move      w-bro-qta-dev
                    (w-sdc-ctr-rig)       to   w-qss-qta-bfo          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-700.
       acc-qta-bfo-620.
      *                  *---------------------------------------------*
      *                  * Se Pf1                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to  acc-qta-bfo-640.
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se quantita' in bolla maggiore o u- *
      *                          * guale a quella da evadere : spaces  *
      *                          *-------------------------------------*
           if        w-qss-qta-bfo        <    w-qss-qta-dev
                     go to  acc-qta-bfo-630.
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-700.
       acc-qta-bfo-630.
      *                          *-------------------------------------*
      *                          * Altrimenti : si inverte il valore   *
      *                          * attuale del flag                    *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     move  "S"            to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)
           else      move  spaces         to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-700.
       acc-qta-bfo-640.
      *                  *---------------------------------------------*
      *                  * Se Return o altri tasti funzione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se valore impostato pari al prece-  *
      *                          * dente : nessuna variazione          *
      *                          *-------------------------------------*
           if        w-bro-qta-bfo
                    (w-sdc-ctr-rig)        not  = w-sav-qta-bfo
                     go to  acc-qta-bfo-650.
           go to     acc-qta-bfo-700.
       acc-qta-bfo-650.
      *                          *-------------------------------------*
      *                          * Se valore impostato diverso dal     *
      *                          * precedente                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in bolla maggiore  *
      *                              * o uguale a quella da evadere :  *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-bfo        <    w-qss-qta-dev
                     go to  acc-qta-bfo-655.
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-700.
       acc-qta-bfo-655.
      *                              *---------------------------------*
      *                              * Altrimenti : valore inalterato  *
      *                              *---------------------------------*
           go to     acc-qta-bfo-700.
       acc-qta-bfo-700.
      *                  *---------------------------------------------*
      *                  * Trattamento altri valori riga               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag indicatore di residuo              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-bfo-710.
           move      spaces               to   w-bro-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-720.
       acc-qta-bfo-710.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in bolla maggiore  *
      *                              * o uguale a quella da evadere :  *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-bfo        <    w-qss-qta-dev
                     go to  acc-qta-bfo-715.
           move      spaces               to   w-bro-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-720.
       acc-qta-bfo-715.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a '<'                   *
      *                              *---------------------------------*
           move      "<"                  to   w-bro-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-720.
       acc-qta-bfo-720.
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-bfo-730.
           move      "S"                  to   w-bro-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-740.
       acc-qta-bfo-730.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in bolla minore    *
      *                              * o uguale a quella da evadere :  *
      *                              * viene forzato il valore spaces  *
      *                              *---------------------------------*
           if        w-qss-qta-bfo        >    w-qss-qta-dev
                     go to  acc-qta-bfo-735.
           move      spaces               to   w-bro-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-740.
       acc-qta-bfo-735.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a "+"                   *
      *                              *---------------------------------*
           move      "+"                  to   w-bro-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-740.
       acc-qta-bfo-740.
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-bfo-750.
           move      zero                 to   w-bro-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-760.
       acc-qta-bfo-750.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in bolla maggiore  *
      *                              * o uguale a quella da evadere :  *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-bfo        <    w-qss-qta-dev
                     go to  acc-qta-bfo-755.
           move      zero                 to   w-bro-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-760.
       acc-qta-bfo-755.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore e' pari  *
      *                              * alla differenza tra quantita'   *
      *                              * da evadere e quantita' in bolla *
      *                              *---------------------------------*
           subtract  w-bro-qta-bfo
                    (w-sdc-ctr-rig)       from w-bro-qta-dev
                                              (w-sdc-ctr-rig)
                                        giving w-bro-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-bfo-760.
       acc-qta-bfo-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' in bolla                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-bro-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-bro-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-bro-qta-bfo
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag indicatore di residuo              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-bro-flg-idr
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      68                   to   v-pos                  .
           move      w-bro-flg-ids
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da visualizzare             *
      *                          *-------------------------------------*
           if        w-prs-rig-scr-res    not  = 00
                     go to acc-qta-bfo-800.
      *                          *-------------------------------------*
      *                          * Editing quantita'                   *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-bro-qta-res
                    (w-sdc-ctr-rig)       to   w-edt-qta-inc-qta      .
           move      w-bro-dec-qta
                    (w-sdc-ctr-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-qta-bfo-800.
       acc-qta-bfo-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo saldaconto : Si/No Addebito o Commento *
      *    *-----------------------------------------------------------*
       acc-snx-aoc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-sdc-ctr-rig        =    zero
                     go to acc-snx-aoc-999.
           if        w-bro-tip-rig-tpr
                    (w-sdc-ctr-rig)       not  = "A" and
                     w-bro-tip-rig-tpr
                    (w-sdc-ctr-rig)       not  = "C"
                     go to acc-snx-aoc-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-bro-snx-aoc
                    (w-sdc-ctr-rig)       to   w-sav-snx-aoc          .
       acc-snx-aoc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      59                   to   v-pos                  .
      *                  *---------------------------------------------*
      *                  * Tasti funzione                              *
      *                  *---------------------------------------------*
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-bro-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
       acc-snx-aoc-150.
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-bro-snx-aoc
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-snx-aoc-999.
       acc-snx-aoc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-snx-aoc-400.
      *                  *---------------------------------------------*
      *                  * Se campo modificato : ripristino valore pre-*
      *                  * cedente e reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = w-sav-snx-aoc
                     move   w-sav-snx-aoc to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)
                     go to  acc-snx-aoc-100.
      *                  *---------------------------------------------*
      *                  * Se valore presente lo si abblenca, altri-   *
      *                  * menti lo si forza a 'S'                     *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     move  spaces         to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)
           else      move  "S"            to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
       acc-snx-aoc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = "S" and
                     w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = "N" and
                     w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-snx-aoc-100.
       acc-snx-aoc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore                      *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       =    "N"
                     move  spaces         to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Si/No addebito o commento   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      59                   to   v-pos                  .
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-snx-aoc-800.
       acc-snx-aoc-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ricerca per codice magazzino                   *
      *    *-----------------------------------------------------------*
       exe-rca-pcm-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero riga su cui posizionar- *
      *              * si                                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-nrg      .
      *              *-------------------------------------------------*
      *              * Salvataggio del video attuale                   *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-020.
      *              *-------------------------------------------------*
      *              * Accettazione tipo e codice magazzino da ricer-  *
      *              * care                                            *
      *              *-------------------------------------------------*
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
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione work-area di accettazione   *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-tpm      .
           move      zero                 to   w-exe-rca-pcm-cnm      .
           move      spaces               to   w-exe-rca-pcm-cam      .
           move      spaces               to   w-exe-rca-pcm-dem      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo centrale                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      27                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      "RICERCA PER CODICE PRODOTTO"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Tipo codice di magazzino                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da visualizzare             *
      *                          *-------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to exe-rca-pcm-030.
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo prodotto   :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-030.
      *                      *-----------------------------------------*
      *                      * Codice di magazzino                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Codice prodotto :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Carattere di presa visione              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           perform   acc-tip-mag-000      thru acc-tip-mag-999        .
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-900.
           if        v-key                =    "DO  "
                     go to exe-rca-pcm-400.
       exe-rca-pcm-200.
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico di magazzino            *
      *                  *---------------------------------------------*
           perform   acc-cod-mag-000      thru acc-cod-mag-999        .
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-900.
           if        v-key                =    "DO  "
                     go to exe-rca-pcm-400.
           if        v-key                =    "UP  "
                     go to exe-rca-pcm-100.
       exe-rca-pcm-300.
      *                  *---------------------------------------------*
      *                  * Carattere di presa visione                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-900.
           if        v-key                =    "DO  "
                     go to exe-rca-pcm-400.
           if        v-key                =    "UP  "
                     go to exe-rca-pcm-200.
       exe-rca-pcm-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori necessari *
      *              * per la rcerca                                   *
      *              *-------------------------------------------------*
           if        w-exe-rca-pcm-tpm    =    zero or
                     w-exe-rca-pcm-cnm    =    zero
                     go to exe-rca-pcm-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione numzero elementi del buffer    *
      *              * numeri riga con codice prodotto pari a quello   *
      *              * impostato                                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-ele      .
      *              *-------------------------------------------------*
      *              * Ricerca effettiva in buffer righe ordine        *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-ctr      .
       exe-rca-pcm-420.
           add       1                    to   w-exe-rca-pcm-ctr      .
           if        w-exe-rca-pcm-ctr    >    w-bro-num-ele
                     go to exe-rca-pcm-500.
           if        w-exe-rca-pcm-tpm    =    w-bro-tip-mag
                                              (w-exe-rca-pcm-ctr) and
                     w-exe-rca-pcm-cnm    =    w-bro-num-mag
                                              (w-exe-rca-pcm-ctr)
                     go to exe-rca-pcm-440.
           go to     exe-rca-pcm-420.
       exe-rca-pcm-440.
      *                  *---------------------------------------------*
      *                  * Se codice di magazzino trovato              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione numero riga in buffer   *
      *                      *-----------------------------------------*
           if        w-exe-rca-pcm-ele    <    w-exe-rca-pcm-max
                     add  1               to   w-exe-rca-pcm-ele      .
           move      w-exe-rca-pcm-ctr    to   w-exe-rca-pcm-bnr
                                              (w-exe-rca-pcm-ele)     .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     exe-rca-pcm-420.
       exe-rca-pcm-500.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del numero elementi buffe- *
      *              * rizzato                                         *
      *              *-------------------------------------------------*
           if        w-exe-rca-pcm-ele    =    zero
                     go to exe-rca-pcm-520
           else if   w-exe-rca-pcm-ele    =    1
                     go to exe-rca-pcm-540
           else      go to exe-rca-pcm-560.
       exe-rca-pcm-520.
      *              *-------------------------------------------------*
      *              * Se zero elementi bufferizzati                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Non e' stata trovata alcuna riga ordine con il cod
      -              "ice prodotto   "    to   w-err-box-err-msg      .
           move      "impostato !                                       
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     exe-rca-pcm-100.
       exe-rca-pcm-540.
      *              *-------------------------------------------------*
      *              * Se 1 elemento bufferizzato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore in uscita per numero riga            *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-bnr (1)
                                          to   w-exe-rca-pcm-nrg      .
      *                  *---------------------------------------------*
      *                  * A uscita                                    *
      *                  *---------------------------------------------*
           go to     exe-rca-pcm-900.
       exe-rca-pcm-560.
      *              *-------------------------------------------------*
      *              * Se piu' elementi bufferizzati                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione linea 12 attuale             *
      *                  *---------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-exe-rca-pcm-sl1      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione linea 14 attuale             *
      *                  *---------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-exe-rca-pcm-sl2      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione linea 15 attuale             *
      *                  *---------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      15                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-exe-rca-pcm-sl3      .
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
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione titolo                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-exe-rca-pcm-sl1    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione tipo prodotto             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-exe-rca-pcm-sl2    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione codice prodotto           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-exe-rca-pcm-sl3    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Fincatura                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      75                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "N.R Da Evadere Cons.Rich. Cons.Prev. | N.R Da Evad
      -              "ere Cons.Rich. Cons.Prev."
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      75                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "--- ---------- ---------- ---------- | --- -------
      -              "--- ---------- ----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Barrette di divisione                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      75                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                                     |            
      -              "                         "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      17                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      18                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      19                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      20                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-562.
      *                  *---------------------------------------------*
      *                  * Ciclo di visualizzazione elementi bufferiz- *
      *                  * zati                                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-ctr      .
       exe-rca-pcm-564.
           add       1                    to   w-exe-rca-pcm-ctr      .
           if        w-exe-rca-pcm-ctr    >    w-exe-rca-pcm-ele
                     go to exe-rca-pcm-566.
      *                      *-----------------------------------------*
      *                      * Determinazione indice elemento nel buf- *
      *                      * fer righe ordine                        *
      *                      *-----------------------------------------*
           move      w-exe-rca-pcm-bnr
                    (w-exe-rca-pcm-ctr)   to   w-exe-rca-pcm-inx      .
      *                      *-----------------------------------------*
      *                      * Numero riga                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  42             to   v-pos
           else      move  03             to   v-pos                  .
           move      w-exe-rca-pcm-bnr
                    (w-exe-rca-pcm-ctr)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' da evadere                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
      *
           if        w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   >    99999 or
                     w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   <   -99999
                     move  06             to   v-car
           else      move  05             to   v-car                  .
      *
           move      w-bro-dec-qta
                    (w-exe-rca-pcm-inx)   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
      *
           if        w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   >    99999 or
                     w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   <   -99999
                     move  spaces         to   v-edm
           else      move  "G"            to   v-edm                  .
      *
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
      *
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  45             to   v-pos
           else      move  06             to   v-pos                  .
      *
           move      w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Data consegna richiesta                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  57             to   v-pos
           else      move  18             to   v-pos                  .
           move      w-bro-dcn-ric
                    (w-exe-rca-pcm-inx)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Formato di stampa consegna richiesta    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  66             to   v-pos
           else      move  27             to   v-pos                  .
           move      w-bro-fds-dcr
                    (w-exe-rca-pcm-inx)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Data consegna prevista                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  68             to   v-pos
           else      move  29             to   v-pos                  .
           move      w-bro-dcn-prv
                    (w-exe-rca-pcm-inx)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag di conferma                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  77             to   v-pos
           else      move  38             to   v-pos                  .
           move      w-bro-flg-cnf
                    (w-exe-rca-pcm-inx)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     exe-rca-pcm-564.
       exe-rca-pcm-566.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ciclo di selezione di una delle righe buf-  *
      *                  * ferizzate                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-exe-rca-pcm-ctr      .
       exe-rca-pcm-568.
      *                      *-----------------------------------------*
      *                      * Accettazione function key               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  57             to   v-pos
           else      move  18             to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "RTRN"               to   v-pfk (19)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-570.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione della function   *
      *                      * key impostata                           *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-572
           else if   v-key                =    "SLCT" or
                     v-key                =    spaces
                     go to exe-rca-pcm-574
           else if   v-key                =    "DOWN"
                     go to exe-rca-pcm-576
           else if   v-key                =    "UP  "
                     go to exe-rca-pcm-578.
       exe-rca-pcm-572.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A uscita                            *
      *                          *-------------------------------------*
           go to     exe-rca-pcm-900.
       exe-rca-pcm-574.
      *                      *-----------------------------------------*
      *                      * Se Select o Return                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore in uscita per numero riga    *
      *                          *-------------------------------------*
           move      w-exe-rca-pcm-bnr
                    (w-exe-rca-pcm-ctr)   to   w-exe-rca-pcm-nrg      .
      *                          *-------------------------------------*
      *                          * A uscita                            *
      *                          *-------------------------------------*
           go to     exe-rca-pcm-900.
       exe-rca-pcm-576.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        w-exe-rca-pcm-ctr    <    w-exe-rca-pcm-ele
                     add 1                to   w-exe-rca-pcm-ctr      .
           go to     exe-rca-pcm-568.
       exe-rca-pcm-578.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        w-exe-rca-pcm-ctr    >    1
                     subtract 1           from w-exe-rca-pcm-ctr      .
           go to     exe-rca-pcm-568.
       exe-rca-pcm-900.
      *              *-------------------------------------------------*
      *              * Ripristino video precedentemente salvato        *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-rca-pcm-999.
       exe-rca-pcm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Ti- *
      *    * po codice di magazzino                                    *
      *    *-----------------------------------------------------------*
       acc-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se gestione materie prime non attiva : for- *
      *                  * zatura del valore 01 e uscita               *
      *                  *---------------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     move  01             to   w-exe-rca-pcm-tpm
                     go to acc-tip-mag-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-tpm    to   w-sav-tip-mag          .
       acc-tip-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-mag-lun    to   v-car                  .
           move      w-exp-tip-mag-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-exp-tip-mag-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           if        w-exe-rca-pcm-tpm    =    01
                     move  01             to   v-num
           else if   w-exe-rca-pcm-tpm    =    03
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-tip-mag-999.
       acc-tip-mag-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  01             to   w-exe-rca-pcm-tpm
           else if   v-num                =    02
                     move  03             to   w-exe-rca-pcm-tpm
           else      move  zero           to   w-exe-rca-pcm-tpm      .
       acc-tip-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-exe-rca-pcm-tpm    not  = 01 and
                     w-exe-rca-pcm-tpm    not  = 03
                     go to acc-tip-mag-100.
       acc-tip-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        w-exe-rca-pcm-tpm    =    w-sav-tip-mag
                     go to acc-tip-mag-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice di magazzino         *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-cnm      .
           move      spaces               to   w-exe-rca-pcm-cam      .
           move      spaces               to   w-exe-rca-pcm-dem      .
           perform   vis-cod-mag-000      thru vis-cod-mag-999        .
           perform   vis-des-mag-000      thru vis-des-mag-999        .
       acc-tip-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Co- *
      *    * dice di magazzino                                         *
      *    *-----------------------------------------------------------*
       acc-cod-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo magazzino   *
      *                  *---------------------------------------------*
           if        w-exe-rca-pcm-tpm    =    01
                     go to acc-cod-mag-110
           else if   w-exe-rca-pcm-tpm    =    03
                     go to acc-cod-mag-130
           else      go to acc-cod-mag-999.
       acc-cod-mag-110.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino : Prodotto di vendita        *
      *                  *---------------------------------------------*
           perform   acc-cod-dcp-000      thru acc-cod-dcp-999        .
           go to     acc-cod-mag-400.
       acc-cod-mag-130.
      *                  *---------------------------------------------*
      *                  * Tipo magazzino : Materia prima              *
      *                  *---------------------------------------------*
           perform   acc-cod-dpm-000      thru acc-cod-dpm-999        .
           go to     acc-cod-mag-400.
       acc-cod-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-cod-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Co- *
      *    * dice prodotto [dcp]                                       *
      *    *-----------------------------------------------------------*
       acc-cod-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dcp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      w-exe-rca-pcm-cnm    to   w-cod-cod-dcp-num      .
           move      w-exe-rca-pcm-cam    to   w-cod-cod-dcp-alf      .
           move      15                   to   w-cod-cod-dcp-lin      .
           move      21                   to   w-cod-cod-dcp-pos      .
           move      15                   to   w-cod-cod-dcp-dln      .
           move      37                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
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
           move      w-cod-cod-dcp-num    to   v-num                  .
       acc-cod-dcp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cod-dcp-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-exe-rca-pcm-cam      .
           move      v-num                to   w-exe-rca-pcm-cnm      .
       acc-cod-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-cnm    to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-cod-dcp-100.
      *                  *---------------------------------------------*
      *                  * Valore a zero nom ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-exe-rca-pcm-cnm    not  = zero
                     go to acc-cod-dcp-600.
           if        v-key                =    "UP  "
                     go to acc-cod-dcp-600
           else      go to acc-cod-dcp-100.
       acc-cod-dcp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione descrizione prodotto    *
      *                      * [dcp]                                   *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-des    to   w-exe-rca-pcm-dem      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione prodotto    *
      *                      *-----------------------------------------*
           perform   vis-des-mag-000      thru vis-des-mag-999        .
       acc-cod-dcp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-cod-dcp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Co- *
      *    * dice prodotto [dpm]                                       *
      *    *-----------------------------------------------------------*
       acc-cod-dpm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dpm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpm-ope      .
           move      "A"                  to   w-cod-cod-dpm-tac      .
           move      w-exe-rca-pcm-cnm    to   w-cod-cod-dpm-num      .
           move      w-exe-rca-pcm-cam    to   w-cod-cod-dpm-alf      .
           move      15                   to   w-cod-cod-dpm-lin      .
           move      21                   to   w-cod-cod-dpm-pos      .
           move      15                   to   w-cod-cod-dpm-dln      .
           move      37                   to   w-cod-cod-dpm-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
       acc-cod-dpm-110.
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           if        w-cod-cod-dpm-ope    =    "F+"
                     go to acc-cod-dpm-115.
           if        w-cod-cod-dpm-ope    =    "AC"
                     go to acc-cod-dpm-120.
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-dpm-115.
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
           go to     acc-cod-dpm-110.
       acc-cod-dpm-120.
           move      w-cod-cod-dpm-alf    to   v-alf                  .
           move      w-cod-cod-dpm-num    to   v-num                  .
       acc-cod-dpm-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cod-dpm-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-exe-rca-pcm-cam      .
           move      v-num                to   w-exe-rca-pcm-cnm      .
       acc-cod-dpm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dpm]                      *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-cnm    to   w-let-arc-dpm-cod      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-dpm-flg    not  = spaces
                     go to acc-cod-dpm-100.
      *                  *---------------------------------------------*
      *                  * Valore a zero nom ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-exe-rca-pcm-cnm    not  = zero
                     go to acc-cod-dpm-600.
           if        v-key                =    "UP  "
                     go to acc-cod-dpm-600
           else      go to acc-cod-dpm-100.
       acc-cod-dpm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione descrizione prodotto    *
      *                      * [dpm]                                   *
      *                      *-----------------------------------------*
           move      w-let-arc-dpm-des    to   w-exe-rca-pcm-dem      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione prodotto    *
      *                      *-----------------------------------------*
           perform   vis-des-mag-000      thru vis-des-mag-999        .
       acc-cod-dpm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-cod-dpm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo per ricerca per codice magazzino :  *
      *    * Codice di magazzino                                       *
      *    *-----------------------------------------------------------*
       vis-cod-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-exe-rca-pcm-cam    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo per ricerca per codice magazzino :  *
      *    * Descrizione di magazzino                                  *
      *    *-----------------------------------------------------------*
       vis-des-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-exe-rca-pcm-dem    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Ca- *
      *    * rattere di presa visione                                  *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pre-vis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      16                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "RTRN"               to   v-pfk (19)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pre-vis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pre-vis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-pre-vis-999.
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
      *              * Ciclo di scansione buffer righe ordine          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-crc-wrk-ctr          .
       car-rig-cat-100.
      *                  *---------------------------------------------*
      *                  * Ciclo di caricamento                        *
      *                  *---------------------------------------------*
           add       1                    to   w-crc-wrk-ctr          .
      *                  *---------------------------------------------*
      *                  * Test su numero massimo elementi             *
      *                  *---------------------------------------------*
           if        w-crc-wrk-ctr        >    w-bro-num-ele
                     go to car-rig-cat-800.
       car-rig-cat-110.
      *                  *---------------------------------------------*
      *                  * Test se righe saldate con quantita' a zero  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se quantita' a zero                *
      *                      *-----------------------------------------*
           if        w-bro-qta-bfo
                    (w-crc-wrk-ctr)       not  = zero
                     go to car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Test su flag di saldo                   *
      *                      *-----------------------------------------*
           if        w-bro-flg-fzs
                    (w-crc-wrk-ctr)       =    spaces
                     go to car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Preparazione parametri per update       *
      *                      *-----------------------------------------*
           move      w-bro-flg-fzs
                    (w-crc-wrk-ctr)       to   w-rig-orf-fzs (1)      .
           move      w-gen-prt-orf        to   w-rig-orf-prt (1)      .
           move      w-bro-num-prg
                    (w-crc-wrk-ctr)       to   w-rig-orf-prg (1)      .
      *                      *-----------------------------------------*
      *                      * Update riga ordine                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-ofr-000      thru wrt-rec-ofr-999        .
      *                      *-----------------------------------------*
      *                      * A riga successiva                       *
      *                      *-----------------------------------------*
           go to     car-rig-cat-100.
       car-rig-cat-120.
      *                  *---------------------------------------------*
      *                  * Selezione su righe ordine nel buffer        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-bro-tip-rig-tpr
                    (w-crc-wrk-ctr)       =    "A" or
                     w-bro-tip-rig-tpr
                    (w-crc-wrk-ctr)       =    "C"
                     go to car-rig-cat-130
           else      go to car-rig-cat-140.
       car-rig-cat-130.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           if        w-bro-snx-aoc
                    (w-crc-wrk-ctr)       not  = "S"
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-140.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
           if        w-bro-qta-bfo
                    (w-crc-wrk-ctr)       =    zero
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-150.
      *                  *---------------------------------------------*
      *                  * Append in catena                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Append non possibile : uscita con    *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to  car-rig-cat-900.
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
      *                  * Normalizzazione record [ofr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ofr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-gen-prt-orf        to   rf-ofr-num-prt         .
           move      w-bro-num-prg
                    (w-crc-wrk-ctr)       to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       car-rig-cat-300.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
       car-rig-cat-400.
      *                      *-----------------------------------------*
      *                      * Valori contenuti direttamente in re-    *
      *                      * cord [ofr]                              *
      *                      *-----------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
           move      rf-ofr-bld-flb       to   w-rig-bld-flb (1)      .
           move      rf-ofr-bld-tpb       to   w-rig-bld-tpb (1)      .
           move      rf-ofr-bld-rgb       to   w-rig-bld-rgb (1)      .
           move      rf-ofr-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
           move      rf-ofr-tip-mag       to   w-rig-tip-mag (1)      .
           move      rf-ofr-num-mag       to   w-rig-num-mag (1)      .
           move      rf-ofr-alf-mag       to   w-rig-alf-mag (1)      .
           move      rf-ofr-sgl-vrn       to   w-rig-sgl-vrn (1)      .
           move      rf-ofr-fda-pif       to   w-rig-fda-pif (1)      .
           move      rf-ofr-cop-sfn       to   w-rig-cop-sfn (1)      .
           move      rf-ofr-snx-tum       to   w-rig-snx-tum (1)      .
           move      rf-ofr-umf-tum       to   w-rig-umf-tum (1)      .
           move      rf-ofr-nde-tum       to   w-rig-nde-tum (1)      .
           move      rf-ofr-cmo-tum       to   w-rig-cmo-tum (1)      .
           move      rf-ofr-cdi-tum       to   w-rig-cdi-tum (1)      .
           move      rf-ofr-des-ext       to   w-rig-des-ext (1)      .
           move      rf-ofr-des-rig       to   w-rig-des-rig (1)      .
           move      rf-ofr-tip-pro       to   w-rig-tip-pro (1)      .
           move      rf-ofr-umi-acq       to   w-rig-umi-acq (1)      .
           move      rf-ofr-dec-qta       to   w-rig-dec-qta (1)      .
           move      rf-ofr-snx-2qt       to   w-rig-snx-2qt (1)      .
           move      rf-ofr-dec-2qt       to   w-rig-dec-2qt (1)      .
           move      rf-ofr-qta-a02       to   w-rig-qta-a02 (1)      .
           move      rf-ofr-snx-3qt       to   w-rig-snx-3qt (1)      .
           move      rf-ofr-dec-3qt       to   w-rig-dec-3qt (1)      .
           move      rf-ofr-qta-a03       to   w-rig-qta-a03 (1)      .
           move      rf-ofr-oda-tip       to   w-rig-rdo-tip (1)      .
           move      rf-ofr-oda-dat       to   w-rig-rdo-dat (1)      .
           move      rf-ofr-oda-num       to   w-rig-rdo-num (1)      .
           move      rf-ofr-tmo-orf       to   w-rig-orf-tip (1)      .
           move      rf-ofr-dat-doc       to   w-rig-orf-dat (1)      .
           move      rf-ofr-num-doc       to   w-rig-orf-num (1)      .
           move      rf-ofr-num-prt       to   w-rig-orf-prt (1)      .
           move      rf-ofr-num-prg       to   w-rig-orf-prg (1)      .
      *                              *---------------------------------*
      *                              * Valori se per documento che in- *
      *                              * teressa la fatturazione         *
      *                              *---------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-500.
           move      rf-ofr-cod-iva       to   w-rig-civ-rig (1)      .
           if        w-tes-ctp-acq (1)    not  = zero
                     move  w-tes-ctp-acq (1)
                                          to   w-rig-ctp-rig (1)
           else      move  rf-ofr-ctp-acq to   w-rig-ctp-rig (1)      .
           move      rf-ofr-dec-prz       to   w-rig-dec-prz (1)      .
           move      rf-ofr-prz-acq       to   w-rig-prz-acq (1)      .
           move      rf-ofr-snx-2pz       to   w-rig-snx-2pz (1)      .
           move      rf-ofr-dec-2pz       to   w-rig-dec-2pz (1)      .
           move      rf-ofr-prz-a02       to   w-rig-prz-a02 (1)      .
           move      rf-ofr-sgl-vpl       to   w-rig-sgl-vpl (1)      .
           move      rf-ofr-dec-vpl       to   w-rig-dec-vpl (1)      .
           move      rf-ofr-tdc-vpl       to   w-rig-tdc-vpl (1)      .
           move      rf-ofr-prz-vpl       to   w-rig-prz-vpl (1)      .
           move      rf-ofr-ccr-vpl       to   w-rig-ccr-vpl (1)      .
           move      rf-ofr-plm-vpl       to   w-rig-plm-vpl (1)      .
           move      rf-ofr-tlm-vpl       to   w-rig-tlm-vpl (1)      .
           move      rf-ofr-map-vpl       to   w-rig-map-vpl (1)      .
           move      rf-ofr-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-ofr-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-ofr-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-ofr-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-ofr-per-scr (5)   to   w-rig-per-scr (1, 5)   .
           move      rf-ofr-prz-net       to   w-rig-prz-net (1)      .
           move      rf-ofr-iau-rig       to   w-rig-iau-rig (1)      .
           move      rf-ofr-cpv-aap       to   w-rig-cpv-aap (1)      .
           move      rf-ofr-ppv-aap (1)   to   w-rig-ppv-aap (1, 1)   .
           move      rf-ofr-ppv-aap (2)   to   w-rig-ppv-aap (1, 2)   .
           move      rf-ofr-ppv-aap (3)   to   w-rig-ppv-aap (1, 3)   .
           move      rf-ofr-fsp-rig       to   w-rig-fsp-rig (1)      .
           move      rf-ofr-cpv-rig       to   w-rig-cpv-rig (1)      .
           move      rf-ofr-ppv-rig (1)   to   w-rig-ppv-rig (1, 1)   .
           move      rf-ofr-ppv-rig (2)   to   w-rig-ppv-rig (1, 2)   .
           move      rf-ofr-ppv-rig (3)   to   w-rig-ppv-rig (1, 3)   .
           move      rf-ofr-pvf-rig       to   w-rig-pvf-rig (1)      .
       car-rig-cat-500.
      *                      *-----------------------------------------*
      *                      * Valori contenuti in buffer righe ordine *
      *                      *-----------------------------------------*
       car-rig-cat-505.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo riga  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-510
           else      go to car-rig-cat-515.
       car-rig-cat-510.
      *                          *-------------------------------------*
      *                          * Se riga di addebito o commento      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Quantita' secondo il formato di *
      *                              * acquisizione                    *
      *                              *---------------------------------*
           move      zero                 to   w-rig-qta-fda (1)      .
      *                              *---------------------------------*
      *                              * Quantita' secondo noi           *
      *                              *---------------------------------*
           move      zero                 to   w-rig-qta-acq (1)      .
      *                              *---------------------------------*
      *                              * Importo in riga                 *
      *                              *---------------------------------*
           move      rf-ofr-imp-rig       to   w-rig-imp-rig (1)      .
      *                              *---------------------------------*
      *                              * Flag di forzatura saldo         *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "C"
                     move  "S"            to   w-rig-orf-fzs (1)
           else      if    w-bro-snx-aoc
                          (w-crc-wrk-ctr) =    "S"
                           move  "S"      to   w-rig-orf-fzs (1)
                     else  move  spaces   to   w-rig-orf-fzs (1)      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     car-rig-cat-600.
       car-rig-cat-515.
      *                          *-------------------------------------*
      *                          * Se riga con quantita'               *
      *                          *-------------------------------------*
       car-rig-cat-520.
      *                              *---------------------------------*
      *                              * Richiamo routine per la deter-  *
      *                              * minazione della quantita' se-   *
      *                              * condaria                        *
      *                              *---------------------------------*
           move      w-bro-qta-bfo
                    (w-crc-wrk-ctr)       to   w-det-qta-sec-qim      .
           move      w-rig-dec-qta (1)    to   w-det-qta-sec-dgm      .
           move      w-rig-snx-tum (1)    to   w-det-qta-sec-snu      .
           move      w-rig-umf-tum (1)    to   w-det-qta-sec-umf      .
           move      w-rig-nde-tum (1)    to   w-det-qta-sec-ndu      .
           move      w-rig-cmo-tum (1)    to   w-det-qta-sec-cmu      .
           move      w-rig-cdi-tum (1)    to   w-det-qta-sec-cdu      .
           perform   det-qta-sec-000      thru det-qta-sec-999        .
       car-rig-cat-525.
      *                              *---------------------------------*
      *                              * Quantita' secondo il formato di *
      *                              * acquisizione                    *
      *                              *---------------------------------*
           if        w-rig-snx-tum (1)    =    "P"
                     move  w-det-qta-sec-qts
                                          to   w-rig-qta-fda (1)
           else      move  w-bro-qta-bfo
                          (w-crc-wrk-ctr) to   w-rig-qta-fda (1)      .
       car-rig-cat-530.
      *                              *---------------------------------*
      *                              * Quantita' secondo noi           *
      *                              *---------------------------------*
           if        w-rig-snx-tum (1)    =    "P"
                     move  w-bro-qta-bfo
                          (w-crc-wrk-ctr) to   w-rig-qta-acq (1)
           else      move  w-det-qta-sec-qts
                                          to   w-rig-qta-acq (1)      .
       car-rig-cat-540.
      *                              *---------------------------------*
      *                              * Flag di forzatura a saldo       *
      *                              *---------------------------------*
           move      w-bro-flg-fzs
                    (w-crc-wrk-ctr)       to   w-rig-orf-fzs (1)      .
       car-rig-cat-545.
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     car-rig-cat-600.
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Valori calcolati                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Trattamento legame valutario        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se da effettuare           *
      *                              *---------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-640.
           if        w-rig-sgl-vpl (1)    =    spaces
                     go to car-rig-cat-640.
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio effettivo                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ricerca sigla valuta in ta- *
      *                                  * bella valute per legame va- *
      *                                  * lutario                     *
      *                                  *-----------------------------*
           move      zero                 to   w-vpl-ctr-001          .
       car-rig-cat-610.
           add       1                    to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    w-vpl-num-ele
                     go to car-rig-cat-640.
           if        w-rig-sgl-vpl (1)    =    w-vpl-sgl-vpl
                                              (w-vpl-ctr-001)
                     go to car-rig-cat-620.
           go to     car-rig-cat-610.
       car-rig-cat-620.
      *                                  *-----------------------------*
      *                                  * Bufferizzazione coefficien- *
      *                                  * te di cambio da applicare   *
      *                                  *-----------------------------*
           move      w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       to   w-rig-cdc-vpl (1)      .
      *                                  *-----------------------------*
      *                                  * Salvataggio prezzo di ac-   *
      *                                  * quisto precedente           *
      *                                  *-----------------------------*
           move      w-rig-prz-acq (1)    to   w-sav-prz-acq          .
      *                                  *-----------------------------*
      *                                  * Applicazione cambio per le- *
      *                                  * game valutario              *
      *                                  *-----------------------------*
           move      w-rig-prz-vpl (1)    to   w-rig-prz-acq (1)      .
      *                                  *-----------------------------*
      *                                  * Parametri in input          *
      *                                  *-----------------------------*
           move      w-rig-prz-acq (1)    to   w-lvl-prz-prz          .
           move      w-rig-sgl-vpl (1)    to   w-lvl-prz-vlt          .
           move      w-rig-tdc-vpl (1)    to   w-lvl-prz-tdc          .
           move      w-rig-ccr-vpl (1)    to   w-lvl-prz-ccr          .
           move      w-rig-cdc-vpl (1)    to   w-lvl-prz-cdc          .
           move      w-rig-plm-vpl (1)    to   w-lvl-prz-plm          .
           move      w-rig-tlm-vpl (1)    to   w-lvl-prz-tlm          .
      *                                  *-----------------------------*
      *                                  * Determinazione              *
      *                                  *-----------------------------*
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
      *                                  *-----------------------------*
      *                                  * Valore determinato          *
      *                                  *-----------------------------*
           move      w-lvl-prz-prz        to   w-rig-prz-acq (1)      .
      *                                  *-----------------------------*
      *                                  * Se prezzo di acquisto inva- *
      *                                  * riato : oltre               *
      *                                  *-----------------------------*
           if        w-rig-prz-acq (1)    =    w-sav-prz-acq
                     go to car-rig-cat-640.
      *                                  *-----------------------------*
      *                                  * Determinazione prezzo netto *
      *                                  *-----------------------------*
           move      w-rig-prz-acq (1)    to   w-cal-prz-net-prz      .
           move      w-rig-per-scr (1, 1) to   w-cal-prz-net-psc (1)  .
           move      w-rig-per-scr (1, 2) to   w-cal-prz-net-psc (2)  .
           move      w-rig-per-scr (1, 3) to   w-cal-prz-net-psc (3)  .
           move      w-rig-per-scr (1, 4) to   w-cal-prz-net-psc (4)  .
           move      w-rig-per-scr (1, 5) to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   w-rig-prz-net (1)      .
       car-rig-cat-640.
      *                          *-------------------------------------*
      *                          * Importo in riga                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se da determinare          *
      *                              *---------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-650.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
       car-rig-cat-650.
      *                      *-----------------------------------------*
      *                      * Valori non contenuti ne' direttamente   *
      *                      * e ne' indirettamente in record [ofr]    *
      *                      *-----------------------------------------*
           move      zero                 to   w-rig-num-mag-plb (1)  .
           move      zero                 to   w-rig-epz-rgf (1)      .
           move      zero                 to   w-rig-orm-prt (1)      .
           move      spaces               to   w-rig-flg-rch (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
       car-rig-cat-700.
      *                      *-----------------------------------------*
      *                      * Valori contenuti indirettamente in re-  *
      *                      * cord [ofr]                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      w-rig-num-mag (1)    to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                          *-------------------------------------*
      *                          * Si/No esistenza formato di acquisi- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      w-rig-tip-mag (1)    to   w-let-arc-aaf-tpm      .
           move      w-rig-num-mag (1)    to   w-let-arc-aaf-cdm      .
           move      w-tes-cod-arc (1)    to   w-let-arc-aaf-fnt      .
           move      w-rig-fda-pif (1)    to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           if        w-let-arc-aaf-flg    =    spaces
                     move  "S"            to   w-rig-snx-fda (1)
           else      move  "N"            to   w-rig-snx-fda (1)      .
           if        w-let-arc-aaf-des    =    spaces
                     move  "N"            to   w-rig-snx-dep (1)
           else      move  "S"            to   w-rig-snx-dep (1)      .
      *                          *-------------------------------------*
      *                          * % di maggiorazione prezzo acquisto  *
      *                          *-------------------------------------*
           move      w-let-arc-aaf-mpa    to   w-rig-per-mpa (1)      .
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura preliminare file [ofx]  *
      *                              *---------------------------------*
           move      w-gen-prt-orf        to   w-let-arc-ofx-prt      .
           move      w-bro-num-prg
                    (w-crc-wrk-ctr)       to   w-let-arc-ofx-prg      .
           move      11                   to   w-let-arc-ofx-trc      .
           perform   let-arc-ofx-000      thru let-arc-ofx-999        .
      *
           if        w-let-arc-ofx-flg    not  = spaces
                     go to car-rig-cat-705.
      *
           move      w-let-arc-ofx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
      *
           go to     car-rig-cat-750.
       car-rig-cat-705.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di estensione                   *
      *                              *---------------------------------*
           if        w-rig-des-ext (1)    =    0
                     go to car-rig-cat-710
           else if   w-rig-des-ext (1)    =    1
                     go to car-rig-cat-720
           else if   w-rig-des-ext (1)    =    2
                     go to car-rig-cat-730
           else if   w-rig-des-ext (1)    =    3
                     go to car-rig-cat-740.
       car-rig-cat-710.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : nessuna *
      *                              * azione in quanto la descrizione *
      *                              * e' gia' stata bufferizzata      *
      *                              *---------------------------------*
           go to     car-rig-cat-750.
       car-rig-cat-720.
      *                              *---------------------------------*
      *                              * Se estensione nel file [ofx]:   *
      *                              * nessuna azione in quanto la     *
      *                              * descrizione e' gia' stata buf-  *
      *                              * ferizzata                       *
      *                              *---------------------------------*
           go to     car-rig-cat-750.
       car-rig-cat-730.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx]    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-mag (1)    to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-750.
       car-rig-cat-740.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx] ma *
      *                              * con tipo record 13 o 33         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-mag (1)    to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                                  *-----------------------------*
      *                                  * Lettura archivio [aaf]      *
      *                                  *-----------------------------*
           move      w-rig-tip-mag (1)    to   w-let-arc-aaf-tpm      .
           move      w-rig-num-mag (1)    to   w-let-arc-aaf-cdm      .
           move      w-tes-cod-arc (1)    to   w-let-arc-aaf-fnt      .
           move      w-rig-fda-pif (1)    to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           move      w-let-arc-aaf-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-750.
       car-rig-cat-750.
      *                              *---------------------------------*
      *                              * Si/No aggiornamenti di magazzi- *
      *                              * no                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Deviazione in funzione del  *
      *                                  * tipo magazzino              *
      *                                  *-----------------------------*
           if        w-rig-tip-mag (1)    =    01
                     go to car-rig-cat-752
           else if   w-rig-tip-mag (1)    =    03
                     go to car-rig-cat-754
           else if   w-rig-tip-mag (1)    =    04
                     go to car-rig-cat-756.
       car-rig-cat-752.
      *                                  *-----------------------------*
      *                                  * Tipo magazzino : Prodotto   *
      *                                  * di vendita                  *
      *                                  *-----------------------------*
           move      "S"                  to   w-rig-snx-mag (1)      .
           go to     car-rig-cat-760.
       car-rig-cat-754.
      *                                  *-----------------------------*
      *                                  * Tipo magazzino : Materia    *
      *                                  * prima                       *
      *                                  *-----------------------------*
           move      "S"                  to   w-rig-snx-mag (1)      .
           go to     car-rig-cat-760.
       car-rig-cat-756.
      *                                  *-----------------------------*
      *                                  * Tipo magazzino : Materiale  *
      *                                  * vario                       *
      *                                  *-----------------------------*
           move      w-rig-num-mag (1)    to   w-let-arc-mtv-cod      .
           perform   let-arc-mtv-000      thru let-arc-mtv-999        .
           move      w-let-arc-mtv-snm    to   w-rig-snx-mag (1)      .
           go to     car-rig-cat-760.
       car-rig-cat-760.
      *                          *-------------------------------------*
      *                          * Se il documento non interessa la    *
      *                          * fatturazione : a fine riga          *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-780.
      *                          *-------------------------------------*
      *                          * Anagrafica codici iva               *
      *                          *-------------------------------------*
           move      w-rig-civ-rig (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-rig-civ-rig-des (1)  .
      *                          *-------------------------------------*
      *                          * Anagrafica piano dei conti          *
      *                          *-------------------------------------*
           move      w-rig-ctp-rig (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-rig-ctp-rig-des (1)  .
       car-rig-cat-780.
      *                  *---------------------------------------------*
      *                  * Update catena                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su riga ordine successiva               *
      *              *-------------------------------------------------*
           go to     car-rig-cat-100.
       car-rig-cat-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     car-rig-cat-999.
       car-rig-cat-900.
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
           call      "pgm/bfo/prg/obj/pbfo3002"
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
           move      zero                 to   w-rig-num-mag (1)      .
           move      zero                 to   w-rig-num-mag-dlb (1)  .
           move      zero                 to   w-rig-num-mag-plb (1)  .
           move      spaces               to   w-rig-alf-mag (1)      .
           move      spaces               to   w-rig-sgl-vrn (1)      .
           move      spaces               to   w-rig-snx-fda (1)      .
           move      spaces               to   w-rig-snx-dep (1)      .
           move      spaces               to   w-rig-fda-pif (1)      .
           move      spaces               to   w-rig-cop-sfn (1)      .
           move      spaces               to   w-rig-snx-tum (1)      .
           move      spaces               to   w-rig-umf-tum (1)      .
           move      zero                 to   w-rig-nde-tum (1)      .
           move      zero                 to   w-rig-cmo-tum (1)      .
           move      zero                 to   w-rig-cdi-tum (1)      .
           move      zero                 to   w-rig-des-ext (1)      .
           move      spaces               to   w-rig-des-por (1)      .
           move      spaces               to   w-rig-des-rig (1)      .
           move      zero                 to   w-rig-tip-pro (1)      .
           move      zero                 to   w-rig-civ-rig (1)      .
           move      spaces               to   w-rig-civ-rig-des (1)  .
           move      zero                 to   w-rig-ctp-rig (1)      .
           move      spaces               to   w-rig-ctp-rig-des (1)  .
           move      spaces               to   w-rig-umi-acq (1)      .
           move      zero                 to   w-rig-dec-qta (1)      .
           move      spaces               to   w-rig-snx-mag (1)      .
           move      zero                 to   w-rig-qta-fda (1)      .
           move      zero                 to   w-rig-qta-acq (1)      .
           move      zero                 to   w-rig-snx-2qt (1)      .
           move      zero                 to   w-rig-dec-2qt (1)      .
           move      zero                 to   w-rig-qta-a02 (1)      .
           move      zero                 to   w-rig-snx-3qt (1)      .
           move      zero                 to   w-rig-dec-3qt (1)      .
           move      zero                 to   w-rig-qta-a03 (1)      .
           move      zero                 to   w-rig-dec-prz (1)      .
           move      zero                 to   w-rig-prz-acq (1)      .
           move      zero                 to   w-rig-snx-2pz (1)      .
           move      zero                 to   w-rig-dec-2pz (1)      .
           move      zero                 to   w-rig-prz-a02 (1)      .
           move      zero                 to   w-rig-per-mpa (1)      .
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
           move      zero                 to   w-rig-per-scr (1, 1)   .
           move      zero                 to   w-rig-per-scr (1, 2)   .
           move      zero                 to   w-rig-per-scr (1, 3)   .
           move      zero                 to   w-rig-per-scr (1, 4)   .
           move      zero                 to   w-rig-per-scr (1, 5)   .
           move      spaces               to   w-rig-per-scr-ffp (1)  .
           move      zero                 to   w-rig-prz-net (1)      .
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
           move      spaces               to   w-rig-rdo-tip (1)      .
           move      zero                 to   w-rig-rdo-dat (1)      .
           move      zero                 to   w-rig-rdo-num (1)      .
           move      spaces               to   w-rig-orf-tip (1)      .
           move      zero                 to   w-rig-orf-dat (1)      .
           move      zero                 to   w-rig-orf-num (1)      .
           move      zero                 to   w-rig-orf-prt (1)      .
           move      zero                 to   w-rig-orf-prg (1)      .
           move      spaces               to   w-rig-orf-fzs (1)      .
           move      zero                 to   w-rig-orm-prt (1)      .
           move      spaces               to   w-rig-flg-rch (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      zero                 to   w-rig-lot-dat (1)      .
           move      spaces               to   w-rig-lot-sgl (1)      .
      *
           move      zero                 to   w-rig-col-ele (1)      .
           move      zero                 to   I                      .
       nor-nok-rig-800.
           add       1                    to   I                      .
           if        I                    >    12
                     go to nor-nok-rig-810.
           move      spaces               to   w-rig-col-cts (1, I)   .
           move      zero                 to   w-rig-col-qta (1, I)   .
           go to     nor-nok-rig-800.
       nor-nok-rig-810.
           move      spaces               to   w-rig-alx-exp (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [ofr] per quanto riguarda il se-     *
      *    * gnale di riga ordine comunque considerata saldata in      *
      *    * fase di Inserimento di un nuovo record [bfr]              *
      *    *-----------------------------------------------------------*
       wrt-rec-ofr-000.
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
      *              * Test su valore del flag contenuto in w-rig      *
      *              *-------------------------------------------------*
           if        w-rig-orf-fzs (1)    not  = "S"
                     go to wrt-rec-ofr-999.
      *              *-------------------------------------------------*
      *              * Ottenimento record [ofr]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-orf-prt (1)    to   rf-ofr-num-prt         .
           move      w-rig-orf-prg (1)    to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to wrt-rec-ofr-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ofr]                      *
      *              *-------------------------------------------------*
           move      "#"                  to   rf-ofr-sdr-ccs         .
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ofr]                      *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [ofr]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       wrt-rec-ofr-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [ofr] per quanto riguarda il se-     *
      *    * gnale di riga ordine comunque considerata saldata in      *
      *    * fase di Modifica di un record [bfr]                       *
      *    *-----------------------------------------------------------*
       rew-rec-ofr-000.
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
      *              * Test su valore attuale e precedente del flag    *
      *              * contenuto in w-rig                              *
      *              *-------------------------------------------------*
           if        w-rig-orf-fzs (1)    =    w-rig-orf-fzs (2)
                     go to rew-rec-ofr-999.
      *              *-------------------------------------------------*
      *              * Ottenimento record [ofr]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-orf-prt (1)    to   rf-ofr-num-prt         .
           move      w-rig-orf-prg (1)    to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rew-rec-ofr-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ofr]                      *
      *              *-------------------------------------------------*
           if        w-rig-orf-fzs (1)    =    spaces
                     move  spaces         to   rf-ofr-sdr-ccs
           else      move  "#"            to   rf-ofr-sdr-ccs         .
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ofr]                      *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [ofr]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       rew-rec-ofr-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [ofr] per quanto riguarda il se-     *
      *    * gnale di riga ordine comunque considerata saldata in      *
      *    * fase di Cancellazione di un record [bfr]                  *
      *    *-----------------------------------------------------------*
       del-rec-ofr-000.
      *              *-------------------------------------------------*
      *              * Test su valore precedente del flag contenuto    *
      *              * in w-rig                                        *
      *              *-------------------------------------------------*
           if        w-rig-orf-fzs (2)    not  = "S"
                     go to del-rec-ofr-999.
      *              *-------------------------------------------------*
      *              * Ottenimento record [ofr]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-orf-prt (1)    to   rf-ofr-num-prt         .
           move      w-rig-orf-prg (1)    to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-rec-ofr-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ofr]                      *
      *              *-------------------------------------------------*
           move      spaces               to   rf-ofr-sdr-ccs         .
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ofr]                      *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [ofr]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       del-rec-ofr-999.
           exit.

      *    *===========================================================*
      *    * Routine di decomposizione tipo riga                       *
      *    *                                                           *
      *    * Input  : rf-ofr-tip-rig                                   *
      *    *                                                           *
      *    * Output : w-rig-tip-rig (1) e relativi valori di w-rig con-*
      *    *          nessi                                            *
      *    *-----------------------------------------------------------*
       dec-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo di lavoro ridefinito        *
      *              *-------------------------------------------------*
           move      rf-ofr-tip-rig       to    w-dec-tip-rig-str     .
      *              *-------------------------------------------------*
      *              * Normalizzazione area di w-rig relativa al tipo  *
      *              * riga                                            *
      *              *-------------------------------------------------*
           move      spaces               to    w-rig-tip-rig (1)     .
           move      spaces               to    w-rig-tip-rig-tpr (1) .
           move      spaces               to    w-rig-tip-rig-tfu (1) .
           move      zero                 to    w-rig-tip-rig-cac (1) .
           move      spaces               to    w-rig-tip-rig-ast (1) .
      *              *-------------------------------------------------*
      *              * Presenza o assenza dell'asterisco               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ispezione stringa                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-dec-tip-rig-inx      .
           inspect   w-dec-tip-rig-str
                                      tallying w-dec-tip-rig-inx
                                          for  all "*"                .
      *                  *---------------------------------------------*
      *                  * Se nessun asterisco trovato : oltre         *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-inx    =    zero
                     go to dec-tip-rig-100.
      *                  *---------------------------------------------*
      *                  * Segnale di presenza asterisco               *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rig-tip-rig-ast (1)  .
      *                  *---------------------------------------------*
      *                  * Pulizia stringa da asterisco                *
      *                  *---------------------------------------------*
           inspect   w-dec-tip-rig-str
                                     replacing all "*" by   spaces    .
       dec-tip-rig-100.
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo riga                        *
      *              *-------------------------------------------------*
           move      w-dec-tip-rig-chr (1)
                                          to   w-rig-tip-rig-tpr (1)  .
      *              *-------------------------------------------------*
      *              * Test se esiste un tipo funzionamento            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo per tipi riga P o L                    *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "L"
                     go to dec-tip-rig-200.
      *                  *---------------------------------------------*
      *                  * Se secondo carattere della stringa a spazi :*
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-chr (2)
                                          =    spaces
                     go to dec-tip-rig-200.
      *                  *---------------------------------------------*
      *                  * Memorizzazione tipo funzionamento           *
      *                  *---------------------------------------------*
           move      w-dec-tip-rig-chr (2)
                                          to   w-rig-tip-rig-tfu (1)  .
       dec-tip-rig-200.
      *              *-------------------------------------------------*
      *              * Ricerca di un codice addebito o commento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se tipo riga A o C                     *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "A" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "C"
                     go to dec-tip-rig-300.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione stringa a ritroso        *
      *                  *---------------------------------------------*
           move      zero                 to   w-dec-tip-rig-cod      .
           move      6                    to   w-dec-tip-rig-c01      .
           move      3                    to   w-dec-tip-rig-c02      .
       dec-tip-rig-210.
           subtract  1                    from w-dec-tip-rig-c01      .
           if        w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   =    w-rig-tip-rig-tpr (1)
                     go to dec-tip-rig-220.
           if        w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   =    spaces or
                     w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   <    "0"    or
                     w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   >    "9"
                     go to dec-tip-rig-210.
           move      w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   to   w-dec-tip-rig-num
                                              (w-dec-tip-rig-c02)     .
           subtract  1                    from w-dec-tip-rig-c02      .
           go to     dec-tip-rig-210.
       dec-tip-rig-220.
           move      w-dec-tip-rig-cod    to   w-rig-tip-rig-cac (1)  .
       dec-tip-rig-300.
      *              *-------------------------------------------------*
      *              * Preparazione stringa tipo riga per accettazione *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione puntatore per string       *
      *                  *---------------------------------------------*
           move      01                   to   w-dec-tip-rig-pnt      .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-rig-tip-rig-tpr (1)
                                          to   w-rig-tip-rig (1)      .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a spazi : oltre                      *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    spaces
                     go to dec-tip-rig-320.
      *                      *-----------------------------------------*
      *                      * String del tipo funzionamento           *
      *                      *-----------------------------------------*
           add       1                    to   w-dec-tip-rig-pnt      .
           string    w-rig-tip-rig-tfu (1)
                                delimited by   spaces
                                          into w-rig-tip-rig (1) 
                                  with pointer w-dec-tip-rig-pnt      .
       dec-tip-rig-320.
      *                  *---------------------------------------------*
      *                  * Asterisco                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non presente : oltre                 *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-ast (1)
                                          =    spaces
                     go to dec-tip-rig-400.
      *                      *-----------------------------------------*
      *                      * String asterisco                        *
      *                      *-----------------------------------------*
           add       1                    to   w-dec-tip-rig-pnt      .
           string    "*"        delimited by   size
                                          into w-rig-tip-rig (1) 
                                  with pointer w-dec-tip-rig-pnt      .
       dec-tip-rig-400.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dec-tip-rig-999.
       dec-tip-rig-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [oft]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-oft-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-oft-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "porf3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-oft-sel
                     go to  fnd-arc-oft-999.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per il  *
      *              * livello successivo per l'ammissibilita' del ta- *
      *              * sto Slct                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-acc-cod-dpz        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/orf/prg/obj/porf3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuali variabili di i.p.c. de- *
      *              * terminate da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tmo-orf"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oft-sel
                     go to fnd-arc-oft-999.
           move      s-alf                to   w-fnd-arc-oft-toc      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oft-sel
                     go to fnd-arc-oft-999.
           move      s-dat                to   w-fnd-arc-oft-dat      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oft-sel
                     go to fnd-arc-oft-999.
           move      s-num                to   w-fnd-arc-oft-num      .
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-oft-toc    to   w-acc-tmo-orf          .
           move      w-fnd-arc-oft-dat    to   w-acc-dat-doc          .
           move      w-fnd-arc-oft-num    to   w-acc-num-doc          .
       fnd-arc-oft-400.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [yof]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orf        to   w-let-arc-yof-cod      .
           perform   let-arc-yof-000      thru let-arc-yof-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yof-flg    not  = spaces
                     go to fnd-arc-oft-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orf        =    spaces
                     go to fnd-arc-oft-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini fornitori                  *
      *                  *---------------------------------------------*
           move      w-let-arc-yof-des    to   w-acc-tmo-orf-des      .
           move      w-let-arc-yof-vld    to   w-acc-tmo-orf-vld      .
           move      w-let-arc-yof-dpz    to   w-acc-tmo-orf-dpz      .
           move      w-let-arc-yof-ord    to   w-acc-tmo-orf-ord      .
           move      w-let-arc-yof-prd    to   w-acc-tmo-orf-prd      .
           move      w-let-arc-yof-sgl    to   w-acc-tmo-orf-sgl      .
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orf-vld    not  = 02
                     go to fnd-arc-oft-600.
           if        w-acc-cod-dpz        =    w-acc-tmo-orf-dpz
                     go to fnd-arc-oft-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-oft-900.
       fnd-arc-oft-600.
      *              *-------------------------------------------------*
      *              * Completamento visualizzazione campi di accet-   *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *              *-------------------------------------------------*
      *              * Uscita con successo                             *
      *              *-------------------------------------------------*
           go to     fnd-arc-oft-999.
       fnd-arc-oft-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-acc        to   w-acc                  .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-fnd-arc-oft-sel      .
       fnd-arc-oft-999.
           exit.

      *    *===========================================================*
      *    * Select archivio [oft] in base al numero documento         *
      *    *-----------------------------------------------------------*
       slc-num-oft-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oft-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oft-toc      .
           move      zero                 to   w-slc-num-oft-dat      .
           move      zero                 to   w-slc-num-oft-num      .
           move      zero                 to   w-slc-num-oft-crb      .
      *              *-------------------------------------------------*
      *              * Preparazione secolo, anno                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oft-dds    to   s-dat                  .
           move      s-saa                to   w-slc-num-oft-saa      .
       slc-num-oft-080.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
           move      w-slc-num-oft-saa    to   w-slc-num-oft-nsa      .
           move      w-slc-num-oft-dpz    to   w-slc-num-oft-ndp      .
       slc-num-oft-100.
      *              *-------------------------------------------------*
      *              * Start su file [oft]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CNTDEN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-oft-saa    to   rf-oft-scl-ann         .
           move      w-slc-num-oft-dpz    to   rf-oft-cod-dpz         .
           move      w-slc-num-oft-sgl    to   rf-oft-sgl-num         .
           move      w-slc-num-oft-nds    to   rf-oft-num-doc         .
           move      zero                 to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : a controllo contatore  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oft-800.
       slc-num-oft-200.
      *              *-------------------------------------------------*
      *              * Read-next su [oft]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oft-500.
       slc-num-oft-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-oft-scl-ann       not  = w-slc-num-oft-saa or
                     rf-oft-cod-dpz       not  = w-slc-num-oft-dpz or
                     rf-oft-sgl-num       not  = w-slc-num-oft-sgl or
                     rf-oft-num-doc       not  = w-slc-num-oft-nds
                     go to slc-num-oft-500.
       slc-num-oft-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oft-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-slc-num-oft-crb    >    30
                     go to slc-num-oft-520.
       slc-num-oft-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo ordine                           *
      *                  *---------------------------------------------*
           move      rf-oft-num-prt       to   w-slc-num-oft-bpt
                                              (w-slc-num-oft-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     slc-num-oft-200.
       slc-num-oft-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-slc-num-oft-crb    =    zero
                     go to slc-num-oft-800.
      *                  *---------------------------------------------*
      *                  * Se trovato un solo elemento uscita con      *
      *                  * quello                                      *
      *                  *---------------------------------------------*
           if        w-slc-num-oft-crb    >    1
                     go to slc-num-oft-520.
      *                      *-----------------------------------------*
      *                      * Lettura record [oft]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oft-bpt (1)
                                          to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oft-dat-doc
                     move  zero           to   rf-oft-num-doc
                     move  zero           to   rf-oft-cod-arc
                     move  spaces         to   rf-oft-dpz-arc         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati letti              *
      *                      *-----------------------------------------*
           move      rf-oft-dat-doc       to   w-slc-num-oft-dat      .
           move      rf-oft-num-doc       to   w-slc-num-oft-num      .
           move      rf-oft-tmo-orf       to   w-slc-num-oft-toc      .
      *                      *-----------------------------------------*
      *                      * Ad operazioni prima dell'uscita         *
      *                      *-----------------------------------------*
           go to     slc-num-oft-800.
       slc-num-oft-520.
      *                  *---------------------------------------------*
      *                  * Box di espansione                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-slc-num-oft-crb    to   w-slc-num-oft-cpb      .
           subtract  1                    from w-slc-num-oft-cpb      .
           divide    6                    into w-slc-num-oft-cpb      .
           add       1                    to   w-slc-num-oft-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-slc-num-oft-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       Selezionare l'ordine deside
      -              "rato                      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   slc-num-oft-950      thru slc-num-oft-989        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oft-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-slc-num-oft-c01    to   w-slc-num-oft-nli      .
       slc-num-oft-555.
           if        w-slc-num-oft-nli    >    6
                     subtract  6          from w-slc-num-oft-nli
                     go to slc-num-oft-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       09                   to   w-slc-num-oft-nli      .
       slc-num-oft-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       slc-num-oft-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-slc-num-oft-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-slc-num-oft-c01    <    w-slc-num-oft-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-slc-num-oft-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-slc-num-oft-cpa    <    w-slc-num-oft-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-slc-num-oft-nli    to   v-lin                  .
           move      09                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oft-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to slc-num-oft-582
           else if   v-key                =    "UP  "
                     go to slc-num-oft-584
           else if   v-key                =    "DOWN"
                     go to slc-num-oft-586
           else if   v-key                =    "EXIT"
                     go to slc-num-oft-598
           else if   v-key                =    "NXSC"
                     go to slc-num-oft-592
           else if   v-key                =    "PRSC"
                     go to slc-num-oft-594
           else      go to slc-num-oft-575.
       slc-num-oft-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore selezionato           *
      *                  *---------------------------------------------*
           move      w-slc-num-oft-bpt
                    (w-slc-num-oft-c01)   to   w-slc-num-oft-prt      .
      *                  *---------------------------------------------*
      *                  * Lettura record [oft]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oft-prt    to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oft-dat-doc
                     move  zero           to   rf-oft-num-doc
                     move  zero           to   rf-oft-cod-arc
                     move  spaces         to   rf-oft-dpz-arc         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dati letti                  *
      *                  *---------------------------------------------*
           move      rf-oft-dat-doc       to   w-slc-num-oft-dat      .
           move      rf-oft-num-doc       to   w-slc-num-oft-num      .
           move      rf-oft-tmo-orf       to   w-slc-num-oft-toc      .
      *                  *---------------------------------------------*
      *                  * Ad operazioni prima dell'uscita             *
      *                  *---------------------------------------------*
           go to     slc-num-oft-800.
       slc-num-oft-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oft-c01      .
           if        w-slc-num-oft-nli    =    10
                     go to slc-num-oft-590
           else      go to slc-num-oft-550.
       slc-num-oft-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-slc-num-oft-c01    <    w-slc-num-oft-crb
                     add   1              to   w-slc-num-oft-c01
                     go to slc-num-oft-588
           else      go to slc-num-oft-575.
       slc-num-oft-588.
           if        w-slc-num-oft-nli    =    15
                     go to slc-num-oft-590
           else      go to slc-num-oft-550.
       slc-num-oft-590.
           perform   slc-num-oft-950      thru slc-num-oft-989        .
           go to     slc-num-oft-550.
       slc-num-oft-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oft-cpa      .
           go to     slc-num-oft-596.
       slc-num-oft-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oft-cpa      .
       slc-num-oft-596.
           move      w-slc-num-oft-cpa    to   w-slc-num-oft-c01      .
           multiply  6                    by   w-slc-num-oft-c01      .
           subtract  5                    from w-slc-num-oft-c01      .
           go to     slc-num-oft-590.
       slc-num-oft-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oft-800.
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-slc-num-oft-toc    to   w-acc-tmo-orf          .
           move      w-slc-num-oft-dat    to   w-acc-dat-doc          .
           move      w-slc-num-oft-num    to   w-acc-num-doc          .
      *              *-------------------------------------------------*
      *              * Test su valori estratti                         *
      *              *-------------------------------------------------*
           if        w-acc-tmo-orf        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to slc-num-oft-900.
       slc-num-oft-820.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [yof]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orf        to   w-let-arc-yof-cod      .
           perform   let-arc-yof-000      thru let-arc-yof-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-yof-flg    not  = spaces
                     go to slc-num-oft-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orf        =    spaces
                     go to slc-num-oft-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-yof-des    to   w-acc-tmo-orf-des      .
           move      w-let-arc-yof-vld    to   w-acc-tmo-orf-vld      .
           move      w-let-arc-yof-dpz    to   w-acc-tmo-orf-dpz      .
           move      w-let-arc-yof-ord    to   w-acc-tmo-orf-ord      .
           move      w-let-arc-yof-prd    to   w-acc-tmo-orf-prd      .
           move      w-let-arc-yof-sgl    to   w-acc-tmo-orf-sgl      .
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orf-vld    not  = 02
                     go to slc-num-oft-840.
           if        w-acc-cod-dpz        =    w-acc-tmo-orf-dpz
                     go to slc-num-oft-840.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-oft-900.
       slc-num-oft-840.
      *                  *---------------------------------------------*
      *                  * Controllo tipo origine del documento        *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orf-ord    =   01
                     go to slc-num-oft-860.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento non gestibile da questo programma !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-oft-900.
       slc-num-oft-860.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     slc-num-oft-999.
       slc-num-oft-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-acc        to   w-acc                  .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-slc-num-oft-sel      .
       slc-num-oft-940.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     slc-num-oft-999.
       slc-num-oft-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oft-c01    to   w-slc-num-oft-c02      .
           add       5                    to   w-slc-num-oft-c02      .
           divide    6                    into w-slc-num-oft-c02      .
           move      w-slc-num-oft-c02    to   w-slc-num-oft-cpa      .
           subtract  1                    from w-slc-num-oft-c02      .
           multiply  6                    by   w-slc-num-oft-c02      .
           add       1                    to   w-slc-num-oft-c02      .
           add       5
                     w-slc-num-oft-c02  giving w-slc-num-oft-c03      .
           move      w-slc-num-oft-c03    to   w-slc-num-oft-c04      .
           if        w-slc-num-oft-c03    >    w-slc-num-oft-crb
                     move  w-slc-num-oft-crb
                                          to   w-slc-num-oft-c03      .
           move      10                   to   w-slc-num-oft-c05      .
       slc-num-oft-951.
      *              *-------------------------------------------------*
      *              * Lettura record [oft] per visualizzazione        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oft-bpt
                    (w-slc-num-oft-c02)   to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oft-dat-doc
                     move  zero           to   rf-oft-num-doc
                     move  zero           to   rf-oft-cod-arc
                     move  spaces         to   rf-oft-dpz-arc         .
       slc-num-oft-960.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-oft-tmo-orf       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      rf-oft-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rf-oft-num-doc       to   w-acc-num-doc          .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-oft-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           if        rf-oft-dpz-arc       =    spaces
                     go to slc-num-oft-965.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rf-oft-dpz-arc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oft-965.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcf]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      rf-oft-cod-arc       to   rf-dcf-cod-fnt         .
           move      rf-oft-dpz-arc       to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcf-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale archivio                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      39                   to   v-pos                  .
           move      rf-dcf-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-slc-num-oft-c02      .
           add       1                    to   w-slc-num-oft-c05      .
           if        w-slc-num-oft-c02    not  > w-slc-num-oft-c03
                     go to slc-num-oft-951.
       slc-num-oft-970.
           if        w-slc-num-oft-c02    >    w-slc-num-oft-c04
                     go to slc-num-oft-980.
           if        w-slc-num-oft-crb    not  > 6
                     go to slc-num-oft-980.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-slc-num-oft-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-slc-num-oft-c02      .
           add       1                    to   w-slc-num-oft-c05      .
           go to     slc-num-oft-970.
       slc-num-oft-980.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-slc-num-oft-cpa    to   w-slc-num-oft-lt1      .
           move      w-slc-num-oft-cpb    to   w-slc-num-oft-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-slc-num-oft-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oft-989.
           exit.
       slc-num-oft-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [yof]                         *
      *    *-----------------------------------------------------------*
       let-arc-yof-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yof-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-yof-cod    =    spaces
                     go to let-arc-yof-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOF"             to   f-key                  .
           move      w-let-arc-yof-cod    to   rf-yof-cod-tof         .
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-yof-400.
       let-arc-yof-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-yof-des-tof       to   w-let-arc-yof-des      .
           move      rf-yof-vld-dpz       to   w-let-arc-yof-vld      .
           move      rf-yof-cod-dpz       to   w-let-arc-yof-dpz      .
           move      rf-yof-org-doc       to   w-let-arc-yof-ord      .
           move      rf-yof-prv-doc       to   w-let-arc-yof-prd      .
           move      rf-yof-sgl-num       to   w-let-arc-yof-sgl      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-yof-999.
       let-arc-yof-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-yof-flg      .
           move      all   "."            to   w-let-arc-yof-des      .
           go to     let-arc-yof-600.
       let-arc-yof-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yof-des      .
       let-arc-yof-600.
           move      spaces               to   w-let-arc-yof-des      .
           move      zero                 to   w-let-arc-yof-vld      .
           move      zero                 to   w-let-arc-yof-dpz      .
           move      zero                 to   w-let-arc-yof-ord      .
           move      zero                 to   w-let-arc-yof-prd      .
           move      spaces               to   w-let-arc-yof-sgl      .
       let-arc-yof-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-fnt-400.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
           move      rf-fnt-prt-iva       to   w-let-arc-fnt-piv      .
           move      rf-fnt-cod-cge       to   w-let-arc-fnt-stc      .
           move      rf-fnt-cod-iva       to   w-let-arc-fnt-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fnt-flg      .
           move      all   "."            to   w-let-arc-fnt-rag      .
           go to     let-arc-fnt-520.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-520.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-piv      .
           move      zero                 to   w-let-arc-fnt-stc      .
           move      zero                 to   w-let-arc-fnt-ass      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [dcf]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici a zero o spazi                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-fnt    =    zero
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      w-let-arc-dcf-fnt    to   rf-dcf-cod-fnt         .
           move      w-let-arc-dcf-dpz    to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcf-400.
       let-arc-dcf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcf-rag-soc       to   w-let-arc-dcf-rag      .
           move      rf-dcf-via-dcf       to   w-let-arc-dcf-via      .
           move      rf-dcf-loc-dcf       to   w-let-arc-dcf-loc      .
           move      rf-dcf-cod-vlt       to   w-let-arc-dcf-vlt      .
           move      rf-dcf-cod-lng       to   w-let-arc-dcf-lng      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcf-999.
       let-arc-dcf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcf-flg      .
           move      all   "."            to   w-let-arc-dcf-rag      .
           go to     let-arc-dcf-520.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-520.
           move      spaces               to   w-let-arc-dcf-via      .
           move      spaces               to   w-let-arc-dcf-loc      .
           move      spaces               to   w-let-arc-dcf-vlt      .
           move      spaces               to   w-let-arc-dcf-lng      .
       let-arc-dcf-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zvl]                             *
      *    *-----------------------------------------------------------*
       let-arc-zvl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvl-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore della sigla va- *
      *              * luta                                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zvl-cod    =    c-sgl
                     go to let-arc-zvl-200
           else if   w-let-arc-zvl-cod    =    spaces
                     go to let-arc-zvl-400
           else      go to let-arc-zvl-600.
       let-arc-zvl-200.
      *              *-------------------------------------------------*
      *              * Se sigla valuta pari alla valuta base           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      c-des                to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      c-din                to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      c-dec                to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      c-tdc                to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-400.
      *              *-------------------------------------------------*
      *              * Se sigla valuta a Spaces                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-600.
      *              *-------------------------------------------------*
      *              * Se sigla valuta diversa dalla valuta base ed    *
      *              * anche diversa da Spaces                         *
      *              *-------------------------------------------------*
       let-arc-zvl-650.
      *                  *---------------------------------------------*
      *                  * Lettura per sigla valuta                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVLT    "         to   f-key                  .
           move      w-let-arc-zvl-cod    to   rf-zvl-sgl-vlt         .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zvl-750.
       let-arc-zvl-700.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      rf-zvl-des-ita       to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      rf-zvl-des-int       to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      rf-zvl-num-dec       to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      rf-zvl-def-tdc       to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-750.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zvl-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-999.
           exit.

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
      *    * Routine di lettura archivio [yfp]                         *
      *    *-----------------------------------------------------------*
       let-arc-yfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yfp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-yfp-cod    =    zero
                     go to let-arc-yfp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      w-let-arc-yfp-cod    to   rf-yfp-cod-fop         .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-yfp-400.
       let-arc-yfp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-yfp-des-fop       to   w-let-arc-yfp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-yfp-999.
       let-arc-yfp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-yfp-flg      .
           move      all   "."            to   w-let-arc-yfp-des      .
           go to     let-arc-yfp-999.
       let-arc-yfp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yfp-des      .
       let-arc-yfp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cbp]                         *
      *    *-----------------------------------------------------------*
       let-arc-cbp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-cbp-cod    =    spaces
                     go to let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP    "         to   f-key                  .
           move      w-let-arc-cbp-tip    to   rf-cbp-tip-cbp         .
           move      w-let-arc-cbp-cod    to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cbp-400.
       let-arc-cbp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cbp-des-cbp       to   w-let-arc-cbp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cbp-999.
       let-arc-cbp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cbp-flg      .
           move      all   "."            to   w-let-arc-cbp-des      .
           go to     let-arc-cbp-999.
       let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-des      .
       let-arc-cbp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axi]                         *
      *    *-----------------------------------------------------------*
       let-arc-axi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axi-cod    =    zero
                     go to let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI"             to   f-key                  .
           move      w-let-arc-axi-cod    to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axi-400.
       let-arc-axi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axi-den-abi       to   w-let-arc-axi-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axi-999.
       let-arc-axi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axi-flg      .
           move      all   "."            to   w-let-arc-axi-den      .
           go to     let-arc-axi-999.
       let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-den      .
       let-arc-axi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axs]                         *
      *    *-----------------------------------------------------------*
       let-arc-axs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice banca a zero                     *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-abi    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Test se codice agenzia a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-cab    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ABICAB    "         to   f-key                  .
           move      w-let-arc-axs-abi    to   rf-axs-cod-abi         .
           move      w-let-arc-axs-cab    to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axs-400.
       let-arc-axs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axs-den-spt       to   w-let-arc-axs-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axs-999.
       let-arc-axs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axs-flg      .
           move      all   "."            to   w-let-arc-axs-den      .
           go to     let-arc-axs-999.
       let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-den      .
       let-arc-axs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [yin]                         *
      *    *-----------------------------------------------------------*
       let-arc-yin-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yin-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-yin-cod    =    spaces
                     go to let-arc-yin-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-let-arc-yin-cod    to   rf-yin-cod-spi         .
           move      w-let-arc-yin-tpg    to   rf-yin-tip-pag         .
           move      "pgm/dcf/fls/ioc/obj/iofyin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yin                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-yin-400.
       let-arc-yin-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-yin-des-spi       to   w-let-arc-yin-des      .
           move      rf-yin-civ-spi       to   w-let-arc-yin-coi      .
           move      rf-yin-ccp-spi       to   w-let-arc-yin-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-yin-999.
       let-arc-yin-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-yin-flg      .
           move      all   "."            to   w-let-arc-yin-des      .
           go to     let-arc-yin-520.
       let-arc-yin-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-yin-des      .
       let-arc-yin-520.
           move      zero                 to   w-let-arc-yin-coi      .
           move      zero                 to   w-let-arc-yin-ccp      .
       let-arc-yin-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ybo]                         *
      *    *-----------------------------------------------------------*
       let-arc-ybo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ybo-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-ybo-cod    =    spaces
                     go to let-arc-ybo-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-let-arc-ybo-cod    to   rf-ybo-cod-spb         .
           move      w-let-arc-ybo-tpg    to   rf-ybo-tip-pag         .
           move      "pgm/dcf/fls/ioc/obj/iofybo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybo                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ybo-400.
       let-arc-ybo-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ybo-des-spb       to   w-let-arc-ybo-des      .
           move      rf-ybo-civ-spb       to   w-let-arc-ybo-coi      .
           move      rf-ybo-ccp-spb       to   w-let-arc-ybo-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ybo-999.
       let-arc-ybo-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ybo-flg      .
           move      all   "."            to   w-let-arc-ybo-des      .
           go to     let-arc-ybo-520.
       let-arc-ybo-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ybo-des      .
       let-arc-ybo-520.
           move      zero                 to   w-let-arc-ybo-coi      .
           move      zero                 to   w-let-arc-ybo-ccp      .
       let-arc-ybo-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-cod    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-cod    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-dui      .
           move      rf-dcp-tip-pro       to   w-let-arc-dcp-tpr      .
           move      rf-dcp-cod-iva       to   w-let-arc-dcp-civ      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
       let-arc-dcp-210.
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione della descrizione       *
      *                      *-----------------------------------------*
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Test se codice lingua diverso da "I  "  *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-lng    not  = "I  "
                     go to let-arc-dcp-350.
       let-arc-dcp-215.
      *                      *-----------------------------------------*
      *                      * Descrizione contenuta nel record        *
      *                      *-----------------------------------------*
           if        rf-dcp-des-pdx       =    0
                     go to let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Descrizione in file di estensione       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-220.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      01                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-220.
       let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-350.
      *                  *---------------------------------------------*
      *                  * Se lingua diversa da "I  "                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione in lingua                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-370.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      02                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      w-let-arc-dcp-lng    to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-370.
       let-arc-dcp-380.
      *                      *-----------------------------------------*
      *                      * Test in uscita                          *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-des    =    spaces
                     move  rf-dcp-des-pro to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      spaces               to   w-let-arc-dcp-des      .
           move      all   "."            to   w-let-arc-dcp-drg (1)  .
           move      all   "."            to   w-let-arc-dcp-dui      .
           move      all   "."            to   w-let-arc-dcp-alf      .
           go to     let-arc-dcp-520.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
           move      spaces               to   w-let-arc-dcp-alf      .
           move      spaces               to   w-let-arc-dcp-dui      .
       let-arc-dcp-520.
           move      zero                 to   w-let-arc-dcp-tpr      .
           move      zero                 to   w-let-arc-dcp-civ      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
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
      *    * Routine di lettura archivio [mtv]                         *
      *    *-----------------------------------------------------------*
       let-arc-mtv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-mtv-cod    =    zero  
                     go to let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMTV"             to   f-key                  .
           move      w-let-arc-mtv-cod    to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-mtv-400.
       let-arc-mtv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-mtv-des-mtv       to   w-let-arc-mtv-des      .
           move      rf-mtv-umi-gst       to   w-let-arc-mtv-umi      .
           move      rf-mtv-dec-qta       to   w-let-arc-mtv-deq      .
           move      rf-mtv-snx-mag       to   w-let-arc-mtv-snm      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-mtv-999.
       let-arc-mtv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-mtv-flg      .
           move      all   "."            to   w-let-arc-mtv-des      .
           go to     let-arc-mtv-600.
       let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-des      .
       let-arc-mtv-600.
           move      spaces               to   w-let-arc-mtv-umi      .
           move      zero                 to   w-let-arc-mtv-deq      .
           move      spaces               to   w-let-arc-mtv-snm      .
       let-arc-mtv-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [aaq]                         *
      *    *-----------------------------------------------------------*
       let-arc-aaq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaq-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-aaq-cdm    =    zero  
                     go to let-arc-aaq-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-aaq-tpm    to   rf-aaq-tip-mag         .
           move      w-let-arc-aaq-cdm    to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaq-400.
       let-arc-aaq-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-aaq-sgl-vlt       to   w-let-arc-aaq-svl      .
           move      rf-aaq-dec-vlt       to   w-let-arc-aaq-dvl      .
           move      rf-aaq-dec-prz       to   w-let-arc-aaq-dep      .
           move      rf-aaq-prz-acr       to   w-let-arc-aaq-prz      .
           move      rf-aaq-cod-iva       to   w-let-arc-aaq-civ      .
           move      rf-aaq-ctp-acq       to   w-let-arc-aaq-ctp      .
           move      rf-aaq-epz-rgf       to   w-let-arc-aaq-epz      .
           move      rf-aaq-cdp-pdt       to   w-let-arc-aaq-cdp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-aaq-999.
       let-arc-aaq-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-aaq-flg      .
           go to     let-arc-aaq-500.
       let-arc-aaq-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaq-svl      .
           move      zero                 to   w-let-arc-aaq-dvl      .
           move      zero                 to   w-let-arc-aaq-dep      .
           move      zero                 to   w-let-arc-aaq-prz      .
           move      zero                 to   w-let-arc-aaq-civ      .
           move      zero                 to   w-let-arc-aaq-ctp      .
           move      zero                 to   w-let-arc-aaq-epz      .
           move      spaces               to   w-let-arc-aaq-cdp      .
       let-arc-aaq-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [aaf]                         *
      *    *-----------------------------------------------------------*
       let-arc-aaf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaf-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record [aaf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Lettura record [aaf]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM"             to   f-key                  .
           move      w-let-arc-aaf-tpm    to   rf-aaf-tip-mag         .
           move      w-let-arc-aaf-cdm    to   rf-aaf-num-pro         .
           move      w-let-arc-aaf-fnt    to   rf-aaf-cod-dcf         .
           move      w-let-arc-aaf-fda    to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaf-400.
       let-arc-aaf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-aaf-cop-sfn       to   w-let-arc-aaf-csf      .
           move      rf-aaf-snx-tum       to   w-let-arc-aaf-snu      .
           move      rf-aaf-umf-tum       to   w-let-arc-aaf-umf      .
           move      rf-aaf-nde-tum       to   w-let-arc-aaf-ndu      .
           move      rf-aaf-cmo-tum       to   w-let-arc-aaf-cmu      .
           move      rf-aaf-cdi-tum       to   w-let-arc-aaf-cdu      .
           move      rf-aaf-tip-pza       to   w-let-arc-aaf-tpa      .
           move      rf-aaf-sgl-vlt       to   w-let-arc-aaf-vlt      .
           move      rf-aaf-dec-vlt       to   w-let-arc-aaf-dcv      .
           move      rf-aaf-dec-prz       to   w-let-arc-aaf-ndp      .
           move      rf-aaf-lot-acq       to   w-let-arc-aaf-lda      .
           move      rf-aaf-tap-pes       to   w-let-arc-aaf-tap      .
           move      rf-aaf-lgv-vlt       to   w-let-arc-aaf-lvv      .
           move      rf-aaf-lgv-dcv       to   w-let-arc-aaf-lvd      .
           move      rf-aaf-lgv-tdc       to   w-let-arc-aaf-lvt      .
           move      rf-aaf-lgv-cdc       to   w-let-arc-aaf-lvc      .
           move      rf-aaf-lgv-pdt       to   w-let-arc-aaf-lvp      .
           move      rf-aaf-per-mpa       to   w-let-arc-aaf-mpa      .
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-202.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    6
                     go to let-arc-aaf-204.
           move      rf-aaf-qta-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-qtp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-prz-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-pzp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-csr-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-csp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 1)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 1)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 2)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 2)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 3)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 3)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 4)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 4)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 5)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 5)  .
           go to     let-arc-aaf-202.
       let-arc-aaf-204.
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione contenuta nel record        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su flag di estensione          *
      *                          *-------------------------------------*
           if        rf-aaf-xdp-sfn       not  = 0
                     go to let-arc-aaf-210.
      *                          *-------------------------------------*
      *                          * Test se a spaces                    *
      *                          *-------------------------------------*
           if        rf-aaf-dep-sfn       not  = spaces
                     go to let-arc-aaf-208.
           if        w-let-arc-aaf-dmg    =    spaces
                     go to let-arc-aaf-208.
      *                          *-------------------------------------*
      *                          * Se la descrizione e' a spaces si    *
      *                          * utilizza quella letta dalla scheda  *
      *                          * anagrafica del codice magazzino     *
      *                          *-------------------------------------*
           move      w-let-arc-aaf-dmg    to   w-let-arc-aaf-des      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     let-arc-aaf-300.
       let-arc-aaf-208.
      *                          *-------------------------------------*
      *                          * Descrizione per il fornitore        *
      *                          *-------------------------------------*
           move      rf-aaf-dep-sfn       to   w-let-arc-aaf-des      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     let-arc-aaf-300.
       let-arc-aaf-210.
      *                      *-----------------------------------------*
      *                      * Descrizione in file di estensione       *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-220.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    10
                     go to let-arc-aaf-300.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           if        w-let-arc-aaf-tpm    =    01
                     move  13             to   rf-pdx-tip-rec
           else if   w-let-arc-aaf-tpm    =    03
                     move  33             to   rf-pdx-tip-rec
           else if   w-let-arc-aaf-tpm    =    04
                     move  43             to   rf-pdx-tip-rec         .
           move      w-let-arc-aaf-fnt    to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-let-arc-aaf-cdm    to   rf-pdx-cod-num         .
           move      w-let-arc-aaf-fda    to   rf-pdx-for-mat         .
           move      w-let-arc-aaf-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaf-300.
           move      rf-pdx-des-pro       to   w-let-arc-aaf-drg
                                              (w-let-arc-aaf-ctr)     .
           go to     let-arc-aaf-220.
       let-arc-aaf-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-aaf-999.
       let-arc-aaf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-aaf-flg      .
           move      spaces               to   w-let-arc-aaf-des      .
           move      all   "."            to   w-let-arc-aaf-drg (1)  .
           go to     let-arc-aaf-520.
       let-arc-aaf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaf-des      .
       let-arc-aaf-520.
           move      spaces               to   w-let-arc-aaf-dmg      .
           move      spaces               to   w-let-arc-aaf-csf      .
           move      spaces               to   w-let-arc-aaf-snu      .
           move      spaces               to   w-let-arc-aaf-umf      .
           move      zero                 to   w-let-arc-aaf-ndu      .
           move      zero                 to   w-let-arc-aaf-cmu      .
           move      zero                 to   w-let-arc-aaf-cdu      .
           move      zero                 to   w-let-arc-aaf-tpa      .
           move      spaces               to   w-let-arc-aaf-vlt      .
           move      zero                 to   w-let-arc-aaf-dcv      .
           move      zero                 to   w-let-arc-aaf-ndp      .
           move      zero                 to   w-let-arc-aaf-lda      .
           move      zero                 to   w-let-arc-aaf-tap      .
           move      spaces               to   w-let-arc-aaf-lvv      .
           move      zero                 to   w-let-arc-aaf-lvd      .
           move      zero                 to   w-let-arc-aaf-lvt      .
           move      zero                 to   w-let-arc-aaf-lvc      .
           move      zero                 to   w-let-arc-aaf-lvp      .
           move      zero                 to   w-let-arc-aaf-mpa      .
      *
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-522.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    6
                     go to let-arc-aaf-524.
           move      zero                 to   w-let-arc-aaf-qtp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-pzp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-csp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 1)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 2)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 3)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 4)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 5)  .
           go to     let-arc-aaf-522.
       let-arc-aaf-524.
       let-arc-aaf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ofx]                         *
      *    *-----------------------------------------------------------*
       let-arc-ofx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ofx-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-ofx-prt    =    zero   or
                     w-let-arc-ofx-prg    =    zero   or
                     w-let-arc-ofx-trc    =    zero
                     go to let-arc-ofx-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-ofx-prt    to   rf-ofx-num-prt         .
           move      w-let-arc-ofx-prg    to   rf-ofx-num-prg         .
           move      w-let-arc-ofx-trc    to   rf-ofx-tip-rec         .
           move      "pgm/orf/fls/ioc/obj/iofofx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ofx-400.
       let-arc-ofx-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ofx-des-400       to   w-let-arc-ofx-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ofx-999.
       let-arc-ofx-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ofx-flg      .
           move      spaces               to   w-let-arc-ofx-des      .
           move      all   "."            to   w-let-arc-ofx-drg (1)  .
           go to     let-arc-ofx-999.
       let-arc-ofx-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ofx-des      .
       let-arc-ofx-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se documento gia' esistente oppure no      *
      *    *-----------------------------------------------------------*
       det-doc-ges-000.
      *              *-------------------------------------------------*
      *              * Tipo movimento trovato per il caso che la de-   *
      *              * terminazione dia esito pari a 'X' : a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-tmt      .
      *              *-------------------------------------------------*
      *              * Start su file [oft]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-acc-dat-doc        to   rf-oft-dat-doc         .
           move      w-acc-cod-dpz        to   rf-oft-cod-dpz         .
           move      w-acc-num-doc        to   rf-oft-num-doc         .
           move      spaces               to   rf-oft-tmo-orf         .
           move      zero                 to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a documento non esistente     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-900.
       det-doc-ges-100.
      *              *-------------------------------------------------*
      *              * Next su [oft]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a determinazione finale           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Max su [oft], se non superato : a determinazio- *
      *              * ne finale                                       *
      *              *-------------------------------------------------*
           if        rf-oft-dat-doc       not  = w-acc-dat-doc or
                     rf-oft-cod-dpz       not  = w-acc-cod-dpz or
                     rf-oft-num-doc       not  = w-acc-num-doc
                     go to det-doc-ges-800.
       det-doc-ges-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo movimento letto   *
      *              *-------------------------------------------------*
           if        rf-oft-tmo-orf       =    w-acc-tmo-orf
                     go to det-doc-ges-300
           else      go to det-doc-ges-400.
       det-doc-ges-300.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto pari al tipo movimento  *
      *              * da cercare                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di documento esistente e trovato    *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-doc-ges-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-doc-ges-999.
       det-doc-ges-400.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto diverso dal tipo movi-  *
      *              * mento da cercare                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se sigla numerazione per il tipo movimento  *
      *                  * letto diversa da sigla numerazione del ti-  *
      *                  * po movimento cercato : si ignora il docu-   *
      *                  * mento e si passa ad esaminare il documento  *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        rf-oft-sgl-num       not  = w-acc-tmo-orf-sgl
                     go to det-doc-ges-100.
      *                  *---------------------------------------------*
      *                  * Se invece la sigla numerazione e' la stessa *
      *                  * si memorizza il codice tipo movimento letto *
      *                  * per il caso in cui la determinazione doves- *
      *                  * se dare esito pari a 'X'                    *
      *                  *---------------------------------------------*
           move      rf-oft-tmo-orf       to   w-det-doc-ges-tmt      .
      *                  *---------------------------------------------*
      *                  * Quindi si ricicla per esaminare eventuali   *
      *                  * documenti successivi                        *
      *                  *---------------------------------------------*
           go to     det-doc-ges-100.
       det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Determinazioni finali per il caso in cui il do- *
      *              * cumento non sia stato sicuramente trovato       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * documento con la data cercata, il numero    *
      *                  * cercato, e la sigla numerazione cercata :   *
      *                  * si esce con 'X', e con il codice del tipo   *
      *                  * movimento trovato gia' preparato; altri-    *
      *                  * menti si esce per documento non trovato     *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-tmt    not  = spaces
                     move  "X"            to   w-det-doc-ges-snx
                     go to det-doc-ges-999
           else      go to det-doc-ges-900.
       det-doc-ges-900.
      *              *-------------------------------------------------*
      *              * Uscita per documento non trovato                *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-doc-ges-snx      .
       det-doc-ges-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori testata indotti da record [dcf]     *
      *    * principale                                                *
      *    *-----------------------------------------------------------*
       det-vlt-dcf-000.
      *                  *---------------------------------------------*
      *                  * Codice e descrizione nostra banca di appog- *
      *                  * gio associati al fornitore                  *
      *                  *---------------------------------------------*
           move      rf-dcf-nos-ban       to   w-tes-nsb-aaf (1)      .
      *                  *---------------------------------------------*
      *                  * Codice e descrizione nostra banca di appog- *
      *                  * gio per il documento                        *
      *                  *---------------------------------------------*
           move      w-tes-nsb-aaf (1)    to   w-tes-nos-ban (1)      .
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ban (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ban-des (1)  .
      *                  *---------------------------------------------*
      *                  * Codice e descrizione ABI del fornitore      *
      *                  *---------------------------------------------*
           move      rf-dcf-cod-abi       to   w-tes-abi-fnt (1)      .
           move      w-tes-abi-fnt (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-fnt-den (1)  .
      *                  *---------------------------------------------*
      *                  * Codice e descrizione CAB del fornitore      *
      *                  *---------------------------------------------*
           move      rf-dcf-cod-cab       to   w-tes-cab-fnt (1)      .
           move      w-tes-abi-fnt (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-fnt (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-fnt-den (1)  .
      *                  *---------------------------------------------*
      *                  * C/c appoggio del fornitore                  *
      *                  *---------------------------------------------*
           move      rf-dcf-ccc-app       to   w-tes-cca-fnt (1)      .
      *                  *---------------------------------------------*
      *                  * C/c postale del fornitore                   *
      *                  *---------------------------------------------*
           move      rf-dcf-ccp-app       to   w-tes-ccp-fnt (1)      .
      *                  *---------------------------------------------*
      *                  * Modalita' di calcolo importo                *
      *                  *---------------------------------------------*
           move      rf-dcf-tip-mci       to   w-tes-tip-mci (1)      .
       det-vlt-dcf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori testata indotti da record [dcf]     *
      *    * relativo alla dipendenza del fornitore                    *
      *    *-----------------------------------------------------------*
       det-vlt-dcd-000.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza a spazi : uscita           *
      *              *-------------------------------------------------*
           if        w-tes-dpz-arc (1)    =    spaces
                     go to det-vlt-dcd-999.
      *              *-------------------------------------------------*
      *              * Codice e descrizione ABI dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcf-cod-abi       to   w-tes-abi-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-dpz-den (1)  .
      *              *-------------------------------------------------*
      *              * Codice e descrizione CAB dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcf-cod-cab       to   w-tes-cab-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-dpz (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-dpz-den (1)  .
      *              *-------------------------------------------------*
      *              * C/c bancario della dipendenza                   *
      *              *-------------------------------------------------*
           move      rf-dcf-ccc-app       to   w-tes-cca-dpz (1)      .
      *              *-------------------------------------------------*
      *              * C/c postale della dipendenza                    *
      *              *-------------------------------------------------*
           move      rf-dcf-ccp-app       to   w-tes-ccp-dpz (1)      .
       det-vlt-dcd-999.
           exit.

      *    *===========================================================*
      *    * Determinazione quantita' secondaria per l'acquisto        *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/wdetqts0.wks"                   .

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

      *    *===========================================================*
      *    * Editing di una quantita' da incolonnare                   *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       det-imp-rig-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-acq-tip-ope      .
           move      w-rig-qta-fda (1)    to   d-imp-acq-qta-acq      .
           move      w-rig-prz-acq (1)    to   d-imp-acq-prz-uni      .
           move      w-rig-dec-prz (1)    to   d-imp-acq-dec-prz      .
           move      w-rig-per-scr (1, 1) to   d-imp-acq-per-scr (1)  .
           move      w-rig-per-scr (1, 2) to   d-imp-acq-per-scr (2)  .
           move      w-rig-per-scr (1, 3) to   d-imp-acq-per-scr (3)  .
           move      w-rig-per-scr (1, 4) to   d-imp-acq-per-scr (4)  .
           move      w-rig-per-scr (1, 5) to   d-imp-acq-per-scr (5)  .
           move      w-tes-tip-mci (1)    to   d-imp-acq-tip-clc      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-acq-cll-000  thru det-imp-acq-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-acq-imp-rig    to   w-rig-imp-rig (1)      .
       det-imp-rig-999.
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
      *    * Ricerca su buffer documenti movimentati                   *
      *    *-----------------------------------------------------------*
       buf-doc-mvm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-doc-mvm-fds      .
           move      spaces               to   w-buf-doc-mvm-tds      .
           move      zero                 to   w-buf-doc-mvm-nds      .
           move      zero                 to   w-buf-doc-mvm-dds      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-doc-mvm-crb      .
       buf-doc-mvm-100.
      *              *-------------------------------------------------*
      *              * Start su [oft]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATSYS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-buf-doc-mvm-drc    to   rf-oft-ide-dat         .
           move      zero                 to   rf-oft-dat-doc         .
           move      zero                 to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-buf-doc-mvm-fds
                     go to  buf-doc-mvm-999.
       buf-doc-mvm-200.
      *              *-------------------------------------------------*
      *              * Lettura [oft]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-doc-mvm-500.
       buf-doc-mvm-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-oft-ide-dat       not  = w-buf-doc-mvm-drc
                     go to buf-doc-mvm-500.
       buf-doc-mvm-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [oft]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice utente                       *
      *                  *---------------------------------------------*
           if        rf-oft-ide-ute       not  = w-buf-doc-mvm-ute
                     go to buf-doc-mvm-200.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-oft-cod-dpz       not  = w-buf-doc-mvm-dpz
                     go to buf-doc-mvm-200.
       buf-doc-mvm-410.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-doc-mvm-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-buf-doc-mvm-crb    >    w-buf-doc-mvm-max
                     go to buf-doc-mvm-500.
       buf-doc-mvm-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      rf-oft-tmo-orf       to   w-buf-doc-mvm-tmo
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      rf-oft-num-doc       to   w-buf-doc-mvm-num
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      rf-oft-dat-doc       to   w-buf-doc-mvm-dat
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Intestatario documento                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura [fnt]                           *
      *                      *-----------------------------------------*
           move      rf-oft-cod-arc       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
           move      w-let-arc-fnt-rag    to   w-buf-doc-mvm-rsa
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     buf-doc-mvm-200.
       buf-doc-mvm-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-buf-doc-mvm-crb    =    zero
                     move  "#"            to   w-buf-doc-mvm-fds
                     go to buf-doc-mvm-999.
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-doc-mvm-crb    to   w-buf-doc-mvm-cpb      .
           subtract  1                    from w-buf-doc-mvm-cpb      .
           divide    6                    into w-buf-doc-mvm-cpb      .
           add       1                    to   w-buf-doc-mvm-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-doc-mvm-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Editing data                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-doc-mvm-drc    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione fincatura                  *
      *                      *-----------------------------------------*
           move      60                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "DOCUMENTI INSERITI O MODIFICATI"
                                          to   w-all-str-cat (1)      .
      *
           move      w-buf-doc-mvm-drc    to   s-dat                  .
           if        s-gio                =    01 or
                     s-gio                =    08 or
                     s-gio                =    11
                     move  "L'"           to   w-all-str-cat (2)
           else      move  "IL"           to   w-all-str-cat (2)      .
      *
           move      v-edt                to   w-all-str-cat (3)      .
           move      "DA ["               to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      w-buf-doc-mvm-ute    to   w-all-str-cat (2)      .
           move      "]"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   buf-doc-mvm-950      thru buf-doc-mvm-959        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-buf-doc-mvm-c01    to   w-buf-doc-mvm-nli      .
       buf-doc-mvm-555.
           if        w-buf-doc-mvm-nli    >    6
                     subtract  6          from w-buf-doc-mvm-nli
                     go to buf-doc-mvm-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       06                   to   w-buf-doc-mvm-nli      .
       buf-doc-mvm-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       buf-doc-mvm-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-doc-mvm-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-doc-mvm-c01    <    w-buf-doc-mvm-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-doc-mvm-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-doc-mvm-cpa    <    w-buf-doc-mvm-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-doc-mvm-nli    to   v-lin                  .
           move      11                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to buf-doc-mvm-582
           else if   v-key                =    "UP  "
                     go to buf-doc-mvm-584
           else if   v-key                =    "DOWN"
                     go to buf-doc-mvm-586
           else if   v-key                =    "EXIT"
                     go to buf-doc-mvm-598
           else if   v-key                =    "NXSC"
                     go to buf-doc-mvm-592
           else if   v-key                =    "PRSC"
                     go to buf-doc-mvm-594
           else      go to buf-doc-mvm-575.
       buf-doc-mvm-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori selezionati           *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-tmo
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-tds      .
           move      w-buf-doc-mvm-num
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-nds      .
           move      w-buf-doc-mvm-dat
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-dds      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     buf-doc-mvm-999.
       buf-doc-mvm-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-doc-mvm-c01      .
           if        w-buf-doc-mvm-nli    =    07
                     go to buf-doc-mvm-590
           else      go to buf-doc-mvm-550.
       buf-doc-mvm-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-buf-doc-mvm-c01    <    w-buf-doc-mvm-crb
                     add   1              to   w-buf-doc-mvm-c01
                     go to buf-doc-mvm-588
           else      go to buf-doc-mvm-575.
       buf-doc-mvm-588.
           if        w-buf-doc-mvm-nli    =    12
                     go to buf-doc-mvm-590
           else      go to buf-doc-mvm-550.
       buf-doc-mvm-590.
           perform   buf-doc-mvm-950      thru buf-doc-mvm-959        .
           go to     buf-doc-mvm-550.
       buf-doc-mvm-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-doc-mvm-cpa      .
           go to     buf-doc-mvm-596.
       buf-doc-mvm-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-doc-mvm-cpa      .
       buf-doc-mvm-596.
           move      w-buf-doc-mvm-cpa    to   w-buf-doc-mvm-c01      .
           multiply  6                    by   w-buf-doc-mvm-c01      .
           subtract  5                    from w-buf-doc-mvm-c01      .
           go to     buf-doc-mvm-590.
       buf-doc-mvm-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-buf-doc-mvm-fds      .
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-doc-mvm-999.
       buf-doc-mvm-950.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-buf-doc-mvm-c01    to   w-buf-doc-mvm-c02      .
           add       5                    to   w-buf-doc-mvm-c02      .
           divide    6                    into w-buf-doc-mvm-c02      .
           move      w-buf-doc-mvm-c02    to   w-buf-doc-mvm-cpa      .
           subtract  1                    from w-buf-doc-mvm-c02      .
           multiply  6                    by   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c02      .
           add       5
                     w-buf-doc-mvm-c02  giving w-buf-doc-mvm-c03      .
           move      w-buf-doc-mvm-c03    to   w-buf-doc-mvm-c04      .
           if        w-buf-doc-mvm-c03    >    w-buf-doc-mvm-crb
                     move  w-buf-doc-mvm-crb
                                          to   w-buf-doc-mvm-c03      .
           move      07                   to   w-buf-doc-mvm-c05      .
       buf-doc-mvm-951.
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-buf-doc-mvm-tmo
                    (w-buf-doc-mvm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-buf-doc-mvm-num
                    (w-buf-doc-mvm-c02)
                    (6 : 6)               to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-buf-doc-mvm-dat
                    (w-buf-doc-mvm-c02)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale intestatario documento      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-buf-doc-mvm-rsa
                    (w-buf-doc-mvm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c05      .
           if        w-buf-doc-mvm-c02    not  > w-buf-doc-mvm-c03
                     go to buf-doc-mvm-951.
       buf-doc-mvm-952.
           if        w-buf-doc-mvm-c02    >    w-buf-doc-mvm-c04
                     go to buf-doc-mvm-955.
           if        w-buf-doc-mvm-crb    not  > 6
                     go to buf-doc-mvm-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c05      .
           go to     buf-doc-mvm-952.
       buf-doc-mvm-955.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-cpa    to   w-buf-doc-mvm-lt1      .
           move      w-buf-doc-mvm-cpb    to   w-buf-doc-mvm-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-doc-mvm-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-959.
           exit.
       buf-doc-mvm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per ordini  *
      *    * fornitori                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/acdeyof0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice materia prima 'dpm'   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione importo in riga acquisti   *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dimpacq0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione ubicazione di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine fornitore                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

