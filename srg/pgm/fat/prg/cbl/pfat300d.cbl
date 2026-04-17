       Identification Division.
       Program-Id.                                 pfat300d           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:    mov                 *
      *                                   Fase:    fat300e             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Generazione fatture Pro-Forma a fronte      *
      *                    ordine cliente                              *
      *                                                                *
      *                    Richiamata in fat300 da funzione 'PF4'      *
      *                                                                *
      *================================================================*
      *                                                                *
      * Generazione fatture Pro-Forma a fronte ordine cliente          *
      *                                                                *
      *================================================================*
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "OP"                       *
      *                 l-fat-pfo-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-fat-pfo-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "DI" - Dichiarazione di inizio fatturazione differita          *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "DI"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "OC" - Accettazione dati identificativi ordine cliente         *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "BC"                       *
      *                 l-fat-pfo-tmo-orc = codice tipo movimento      *
      *                                     gestione ordini da pro-    *
      *                                     porre come default         *
      *                                                                *
      *        Output : l-fat-pfo-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "T1" - Bufferizzazione testata primo ordine cliente richiamato *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "T1"                       *
      *                                                                *
      *        Output : l-fat-pfo-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "T+" - Confronto dati testata ordine cliente in esame con dati *
      *        di testata del documento                                *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "T+"                       *
      *                                                                *
      *        Output : l-fat-pfo-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "SC" - Saldaconto per inclusione ordine cliente                *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-fat-pfo-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe ordine selezionate in catena          *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "CC"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "WR" - Aggiornamento records ordine per quanto riguarda i dati *
      *        relativi alla fattura differita durante la fase di      *
      *        Inserimento di un nuovo record [fir]                    *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "WR"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "RW" - Aggiornamento records ordine per quanto riguarda i dati *
      *        relativi alla fattura differita durante la fase di      *
      *        Modifica di un nuovo record [fir]                       *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "RW"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "DE" - Aggiornamento records ordine per quanto riguarda i dati *
      *        relativi alla fattura differita durante la fase di      *
      *        Cancellazione di un nuovo record [fir]                  *
      *                                                                *
      *        Input  : l-fat-pfo-tip-ope = "DE"                       *
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
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ocx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocx"                          .
      *        *-------------------------------------------------------*
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [zbo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzbo"                          .
      *        *-------------------------------------------------------*
      *        * [zcs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzcs"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzin"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [lic]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rflic"                          .
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
      *    * Work-area generica per il programma                       *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-gen-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine cliente in corso di tratta-  *
      *        * mento                                                 *
      *        *-------------------------------------------------------*
           05  w-gen-prt-orc              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Flag di fatturazione separata                         *
      *        *-------------------------------------------------------*
           05  w-gen-fat-sep              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo conferma d'ordine a cliente         *
      *        *-------------------------------------------------------*
           05  w-gen-coc-prt              pic  9(11)                  .

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
      *        * Tipo movimento ordini clienti                         *
      *        *-------------------------------------------------------*
           05  w-acc-cod-tmo              pic  x(05)                  .
           05  w-acc-cod-tmo-des          pic  x(30)                  .
           05  w-acc-cod-tmo-ord          pic  9(02)                  .
           05  w-acc-cod-tmo-sgl          pic  x(03)                  .
           05  w-acc-cod-tmo-tvd          pic  x(01)                  .
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

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-oct.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-dds      pic  9(07)                  .
               10  w-slc-num-oct-nds      pic  9(11)                  .
               10  w-slc-num-oct-nds-r redefines
                   w-slc-num-oct-nds.
                   15  w-slc-num-oct-nsa  pic  9(03)                  .
                   15  w-slc-num-oct-ndp  pic  9(02)                  .
                   15  w-slc-num-oct-npg  pic  9(06)                  .
               10  w-slc-num-oct-dpz      pic  9(02)                  .
               10  w-slc-num-oct-sgl      pic  x(03)                  .
               10  w-slc-num-oct-saa      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-sel      pic  x(01)                  .
               10  w-slc-num-oct-toc      pic  x(05)                  .
               10  w-slc-num-oct-dat      pic  9(07)                  .
               10  w-slc-num-oct-num      pic  9(11)                  .
               10  w-slc-num-oct-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-c01      pic  9(02)                  .
               10  w-slc-num-oct-c02      pic  9(02)                  .
               10  w-slc-num-oct-c03      pic  9(02)                  .
               10  w-slc-num-oct-c04      pic  9(02)                  .
               10  w-slc-num-oct-c05      pic  9(02)                  .
               10  w-slc-num-oct-nli      pic  9(02)                  .
               10  w-slc-num-oct-crb      pic  9(02)                  .
               10  w-slc-num-oct-cpb      pic  9(02)                  .
               10  w-slc-num-oct-cpa      pic  9(02)                  .
               10  w-slc-num-oct-buf
                               occurs 30.
                   15  w-slc-num-oct-bpt  pic  9(11)                  .
               10  w-slc-num-oct-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-slc-num-oct-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-slc-num-oct-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per routine acc-cod-tmo-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-act.
           05  w-acc-cod-tmo-inx          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-val-acc.
               10  filler   occurs 200    pic  x(01)                  .
           05  w-sav-qta-fat              pic s9(10)v9(03)            .
           05  w-sav-flg-fzs              pic  x(01)                  .
           05  w-sav-snx-aoc              pic  x(01)                  .
           05  w-sav-prz-ven              pic  9(09)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione protocolli ordini clienti   *
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
      *    * Work-area per bufferizzazione righe ordini cliente        *
      *    *-----------------------------------------------------------*
       01  w-brb.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-brb-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-brb-max-ele              pic  9(05) value 500        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-brb-tbl.
               10  w-brb-sng-ele occurs 900.
                   15  w-brb-num-prg      pic  9(05)       comp-3     .
                   15  w-brb-tip-rig      pic  x(01)                  .
                   15  w-brb-des-rig      pic  x(40)                  .
                   15  w-brb-dec-qta      pic  9(01)                  .
                   15  w-brb-qta-orc      pic s9(10)v9(03) comp-3     .
                   15  w-brb-qta-fat      pic s9(10)v9(03) comp-3     .
                   15  w-brb-flg-idr      pic  x(01)                  .
                   15  w-brb-flg-ids      pic  x(01)                  .
                   15  w-brb-flg-fzs      pic  x(01)                  .
                   15  w-brb-qta-res      pic s9(10)v9(03) comp-3     .
                   15  w-brb-snx-aoc      pic  x(01)                  .

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
           05  w-sdc-ctr-rig              pic  9(03)                  .
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
           05  w-qss-qta-orc              pic  9(06)v9(03)            .
           05  w-qss-qta-fat              pic  9(06)v9(03)            .

      *    *===========================================================*
      *    * Work-area per caricamento righe ordine in catena          *
      *    *-----------------------------------------------------------*
       01  w-crc.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-crc-wrk-ctr              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-tes-orc-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-tes-orc.
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-buf-tes-orc-ctr          pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine ags-ass-iva-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-ags-ass-iva.
      *        *-------------------------------------------------------*
      *        * Assoggettamento iva originale                 [Input] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-aii          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente                                [Input] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-cli          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data documento                                [Input] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-dat          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita                               [Output] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Assoggettamento iva determinato              [Output] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-aio          pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-orc-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-orc.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine cliente                      *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-prt          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo prima conferma d'ordine cliente in- *
      *        * contrata nella scansione                              *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-pro          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tipo riga                    *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-wtr.
               10  w-buf-rig-orc-wtp      pic  x(01)                  .
               10  w-buf-rig-orc-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione in riga                                   *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-wde          pic  x(40)                  .

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
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [oct]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-oct.
               10  w-fnd-arc-oct-sel      pic  x(01)                  .
               10  w-fnd-arc-oct-tmo      pic  x(05)                  .
               10  w-fnd-arc-oct-dat      pic  9(07)                  .
               10  w-fnd-arc-oct-num      pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zoc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zoc.
               10  w-let-arc-zoc-flg      pic  x(01)                  .
               10  w-let-arc-zoc-cod      pic  x(05)                  .
               10  w-let-arc-zoc-des      pic  x(30)                  .
               10  w-let-arc-zoc-ord      pic  9(02)                  .
               10  w-let-arc-zoc-sgl      pic  x(03)                  .
               10  w-let-arc-zoc-dpz      pic  9(02)                  .
               10  w-let-arc-zoc-tvd      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-piv      pic  9(11)                  .
               10  w-let-arc-cli-stc      pic  9(07)                  .
               10  w-let-arc-cli-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-vlt      pic  x(03)                  .
               10  w-let-arc-dcc-lng      pic  x(03)                  .
               10  w-let-arc-dcc-tai      pic  9(02)                  .
               10  w-let-arc-dcc-blo      pic  9(02)                  .
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
      *        * Work per Let su archivio [zvf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvf.
               10  w-let-arc-zvf-flg      pic  x(01)                  .
               10  w-let-arc-zvf-num      pic  9(03)                  .
               10  w-let-arc-zvf-cod      pic  x(03)                  .
               10  w-let-arc-zvf-des      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcs.
               10  w-let-arc-zcs-flg      pic  x(01)                  .
               10  w-let-arc-zcs-tip      pic  9(02)                  .
               10  w-let-arc-zcs-cod      pic  9(05)                  .
               10  w-let-arc-zcs-des      pic  x(20)                  .
               10  w-let-arc-zcs-per occurs 5
                                          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-rag      pic  x(40)                  .
               10  w-let-arc-age-spa      pic  9(07)                  .
               10  w-let-arc-age-cpv      pic  9(05)                  .
               10  w-let-arc-age-ppv occurs 03
                                          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfp.
               10  w-let-arc-zfp-flg      pic  x(01)                  .
               10  w-let-arc-zfp-cod      pic  9(07)                  .
               10  w-let-arc-zfp-des      pic  x(40)                  .
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
      *        * Work per Let su archivio [zin]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zin.
               10  w-let-arc-zin-flg      pic  x(01)                  .
               10  w-let-arc-zin-cod      pic  x(03)                  .
               10  w-let-arc-zin-tpg      pic  9(02)                  .
               10  w-let-arc-zin-des      pic  x(40)                  .
               10  w-let-arc-zin-coi      pic  9(05)                  .
               10  w-let-arc-zin-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zbo]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zbo.
               10  w-let-arc-zbo-flg      pic  x(01)                  .
               10  w-let-arc-zbo-cod      pic  x(03)                  .
               10  w-let-arc-zbo-tpg      pic  9(02)                  .
               10  w-let-arc-zbo-des      pic  x(40)                  .
               10  w-let-arc-zbo-coi      pic  9(05)                  .
               10  w-let-arc-zbo-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ocx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-bix.
               10  w-let-arc-bix-flg      pic  x(01)                  .
               10  w-let-arc-bix-prt      pic  9(11)                  .
               10  w-let-arc-bix-prg      pic  9(05)                  .
               10  w-let-arc-bix-trc      pic  9(02)                  .
               10  w-let-arc-bix-des.
                   15  w-let-arc-bix-drg occurs 10
                                          pic  x(40)                  .
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

      *    *===========================================================*
      *    * Work per Let su archivio [zls]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/larczls0.ltw"                   .

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
      *        *-------------------------------------------------------*
      *        * Work per Det valori testata dipendenti da [dcc]       *
      *        *-------------------------------------------------------*
           05  w-det-vlt-dcc.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcc-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcc-ctr      pic  9(02)                  .
               10  w-det-vlt-dcc-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det funzionamento spese in fattura           *
      *        *-------------------------------------------------------*
           05  w-det-fun-spe.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-fun-spe-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-fun-spe-ctr      pic  9(02)                  .
               10  w-det-fun-spe-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori testata dipendenti da [dcc] della *
      *        * dipendenza                                            *
      *        *-------------------------------------------------------*
           05  w-det-vlt-dcd.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcd-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcd-dpz      pic  x(04)                  .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Agg                               *
      *    *-----------------------------------------------------------*
       01  w-agg.
      *        *-------------------------------------------------------*
      *        * Dati identificativi ordine                            *
      *        *-------------------------------------------------------*
           05  w-agg-bcc-tip              pic  x(05)                  .
           05  w-agg-bcc-dat              pic  9(07)                  .
           05  w-agg-bcc-num              pic  9(11)                  .
           05  w-agg-bcc-num-r redefines
               w-agg-bcc-num.
               10  w-agg-bcc-num-saa      pic  9(03)                  .
               10  w-agg-bcc-num-dpz      pic  9(02)                  .
               10  w-agg-bcc-num-prg      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo ordine in fattura                         *
      *        *-------------------------------------------------------*
           05  w-agg-npr-bif              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per prezzo sottoposto a legame valutario        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per calcolo prezzo netto                        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

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
      *    * Link-area per accettazione codice tipo movimento ordine   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

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
      *    * Area di comunicazione per Fattura Pro-Forma a fronte or-  *
      *    * dine cliente                                              *
      *    *-----------------------------------------------------------*
       01  l-fat-pfo.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-fat-pfo-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-fat-pfo-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-fat-pfo-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento ordini clienti      *
      *        *-------------------------------------------------------*
           05  l-fat-pfo-tmo-orc          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento ordine cliente                    *
      *        *-------------------------------------------------------*
           05  l-fat-pfo-flg-orc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di function-key Tab in corso                     *
      *        *-------------------------------------------------------*
           05  l-fat-pfo-fky-tab          pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pfat3000       *
      *    *                                                           *
      *    * La link-area comprende :                                  *
      *    *                                                           *
      *    *  - 'w-prs'     : Work-area per personalizzazioni          *
      *    *  - 'w-tes'     : Work-area per bufferizzazione testata    *
      *    *  - 'w-pie'     : Work-area per bufferizzazione piede      *
      *    *  - 'w-rig'     : Work-area per bufferizzazione riga       *
      *    *  - 'w-cat-rig' : Work-area di comunicazione per gestione  *
      *    *                  catena righe                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/pfat3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-fat-pfo
                                               w-prs
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
           move      spaces               to   l-fat-pfo-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dichiarazione di inizio ciclo di fattura-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        l-fat-pfo-tip-ope    =    "DI"
                     perform dic-ini-cic-000
                                          thru dic-ini-cic-999
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi ordine     *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "OC"
                     perform acc-dti-orc-000
                                          thru acc-dti-orc-999
      *                  *---------------------------------------------*
      *                  * Bufferizzazione testata primo ordine clien- *
      *                  * te richiamata                               *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "T1"
                     perform buf-tes-orc-000
                                          thru buf-tes-orc-999
      *                  *---------------------------------------------*
      *                  * Confronto testata ordine cliente richiamato *
      *                  * con i dati di testata del documento         *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "T+"
                     perform cnf-tes-doc-000
                                          thru cnf-tes-doc-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per fatturazione differita ma-   *
      *                  * nuale righe ordine                          *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "SC"
                     perform sdc-fat-dfm-000
                                          thru sdc-fat-dfm-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe ordine in catena          *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento records ordine per quanto ri- *
      *                  * guarda i dati relativi alla fattura diffe-  *
      *                  * rita in fase di Inserimento di un nuovo     *
      *                  * record [fir]                                *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "WR"
                     perform wrt-rec-orc-000
                                          thru wrt-rec-orc-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento records ordine per quanto ri- *
      *                  * guarda i dati relativi alla fattura diffe-  *
      *                  * rita in fase di Modifica di un nuovo record *
      *                  * [fir]                                       *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "RW"
                     perform rew-rec-orc-000
                                          thru rew-rec-orc-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento records ordine per quanto ri- *
      *                  * guarda i dati relativi alla fattura diffe-  *
      *                  * rita in fase di Cancellazione di un nuovo   *
      *                  * record [fir]                                *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "DE"
                     perform del-rec-orc-000
                                          thru del-rec-orc-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-fat-pfo-tip-ope    =    "C?"
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
           move      l-fat-pfo-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo movimento                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-cod-tmo          .
       exe-fun-opn-100.
      *              *-------------------------------------------------*
      *              * Apertura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * [zoc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *                  *---------------------------------------------*
      *                  * [lic]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione su gestione   *
      *                      * lettere d'intento clienti attiva        *
      *                      *-----------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to exe-fun-opn-120.
      *                      *-----------------------------------------*
      *                      * Open file                               *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       exe-fun-opn-120.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento ordine  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-opn-000  thru cod-des-zoc-opn-999    .
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
       exe-fun-cls-100.
      *              *-------------------------------------------------*
      *              * Chiusura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * [zoc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *                  *---------------------------------------------*
      *                  * [lic]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione su gestione   *
      *                      * lettere d'intento clienti attiva        *
      *                      *-----------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to exe-fun-cls-120.
      *                      *-----------------------------------------*
      *                      * Open file                               *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       exe-fun-cls-120.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento        *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-cls-000  thru cod-des-zoc-cls-999    .
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
                     move  spaces         to   l-fat-pfo-exi-sts
           else      move  "#"            to   l-fat-pfo-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio ciclo di evasione                 *
      *    *-----------------------------------------------------------*
       dic-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Azzeramento contatore numero elementi in ta-    *
      *              * bella protocolli ordini clienti                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-poc-num-ele          .
      *              *-------------------------------------------------*
      *              * Inizializzazione work per aggiornamenti         *
      *              *-------------------------------------------------*
           move      spaces               to   w-agg-bcc-tip          .
           move      zero                 to   w-agg-bcc-dat          .
           move      zero                 to   w-agg-bcc-num          .
           move      zero                 to   w-agg-npr-bif          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di fatturazione separata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-gen-fat-sep          .
      *              *-------------------------------------------------*
      *              * Normalizzazione protocollo conferma d'ordine    *
      *              * cliente in corso di fatturazione                *
      *              *-------------------------------------------------*
           move      zero                 to   w-gen-coc-prt          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di Function-Key Tab in     *
      *              * corso                                           *
      *              *-------------------------------------------------*
           move      spaces               to   l-fat-pfo-fky-tab      .
       dic-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione dati identificativi ordine cliente           *
      *    *-----------------------------------------------------------*
       acc-dti-orc-000.
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
           move      23                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      "RICHIAMO ORDINE CLIENTE"
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
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se flag di fatturazione separata a "S", vuol    *
      *              * dire che e' gia' stata richiamato un ordine che *
      *              * indicava una fatturazione separata, per cui non *
      *              * possono essere richiamate altri ordini          *
      *              *-------------------------------------------------*
           if        w-gen-fat-sep        not  = "S"
                     go to acc-dti-orc-020.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "L'ordine appena richiamato prevede la fatturazione
      -              " separata, per "    to   w-err-box-err-msg      .
           move      "cui non possono essere richiamati altri ordini !  
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * A uscita per Exit                           *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-800.
       acc-dti-orc-020.
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo movi-   *
      *              * mento ordini clienti                            *
      *              *-------------------------------------------------*
           move      l-fat-pfo-tmo-orc    to   w-acc-cod-tmo          .
           if        w-acc-cod-tmo        =    spaces
                     go to acc-dti-orc-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
           move      w-let-arc-zoc-des    to   w-acc-cod-tmo-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice                      *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
      *                  *---------------------------------------------*
      *                  * Ad accettazione data documento              *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-200.
       acc-dti-orc-100.
      *              *-------------------------------------------------*
      *              * Accettazione dati identificativi ordine         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           perform   acc-cod-tmo-000      thru acc-cod-tmo-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
       acc-dti-orc-200.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   acc-dat-doc-000      thru acc-dat-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orc-100.
       acc-dti-orc-300.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   acc-num-doc-000      thru acc-num-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orc-200.
       acc-dti-orc-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori di iden-  *
      *              * tificazione della ordine                        *
      *              *-------------------------------------------------*
           if        w-acc-cod-tmo        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to acc-dti-orc-100.
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
      *              * Determinazione se documento esistente           *
      *              *-------------------------------------------------*
           perform   det-doc-ges-000      thru det-doc-ges-999        .
      *                  *---------------------------------------------*
      *                  * Se documento non esistente                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "N"
                     go to acc-dti-orc-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Movimento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-420.
      *                  *---------------------------------------------*
      *                  * Se documento esistente ma con tipo movimen- *
      *                  * to diverso                                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "X"
                     go to acc-dti-orc-440.
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
           go to     acc-dti-orc-100.
       acc-dti-orc-440.
      *              *-------------------------------------------------*
      *              * Test se ordine gia' fatturato                   *
      *              *                                                 *
      *              * Attualmente non viene eseguito nessun test      *
      *              *-------------------------------------------------*
       acc-dti-orc-450.
      *              *-------------------------------------------------*
      *              * Test su data ordine                             *
      *              *-------------------------------------------------*
           if        rf-oct-dat-doc       not  > w-tes-dat-doc
                     go to acc-dti-orc-460.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Data ordine superiore alla data della fattura !   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-460.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo               *
      *              *-------------------------------------------------*
           move      rf-oct-num-prt       to   w-acc-num-prt          .
       acc-dti-orc-500.
      *              *-------------------------------------------------*
      *              * Test se l'ordine e' gia' stato richiamato       *
      *              *-------------------------------------------------*
           move      zero                 to   w-poc-ctr-001          .
       acc-dti-orc-520.
           add       1                    to   w-poc-ctr-001          .
           if        w-poc-ctr-001        >    w-poc-num-ele
                     go to acc-dti-orc-540.
           if        w-acc-num-prt        not  = w-poc-num-prt
                                                (w-poc-ctr-001)
                     go to acc-dti-orc-520.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Ordine gia' richiamato !                          
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-540.
      *              *-------------------------------------------------*
      *              * Incremento contatore protocolli ordini clienti  *
      *              *-------------------------------------------------*
           add       1                    to   w-poc-num-ele          .
           if        w-poc-num-ele        not  >  w-poc-max-ele
                     go to acc-dti-orc-600.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo                         *
      *                  *---------------------------------------------*
           move      "Numero ordini trattati oltre il massimo !         
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita per Exit                           *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-800.
       acc-dti-orc-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe ordine da trattare        *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-buf-rig-orc-prt      .
           perform   buf-rig-orc-000      thru buf-rig-orc-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-buf-rig-orc-flg    =    spaces
                     go to acc-dti-orc-620.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Nessuna riga ordine da caricare !                 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-620.
      *              *-------------------------------------------------*
      *              * Se fatturazione separata per l'ordine, control- *
      *              * lo che l'ordine richiamato appartenga alla      *
      *              * stessa conferma d'ordine                        *
      *              *-------------------------------------------------*
           if        w-gen-fat-sep        not  = "O"
                     go to acc-dti-orc-640.
           if        w-buf-rig-orc-pro    =    w-gen-coc-prt
                     go to acc-dti-orc-640.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "L'ordine richiamato non appartiene all'ordine in c
      -              "orso di        "    to   w-err-box-err-msg      .
           move      "fatturazione !                                    
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-640.
      *              *-------------------------------------------------*
      *              * Test su segnale di si/no ordine chiuso          *
      *              *-------------------------------------------------*
           if        rf-oct-flg-och       =    spaces
                     go to acc-dti-orc-660.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "L'ordine richiamato e' gia' stato chiuso !        
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-660.
       acc-dti-orc-660.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo in tabella    *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-poc-num-prt
                                              (w-poc-num-ele)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo ordine cliente in cor- *
      *              * so di trattamento                               *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-gen-prt-orc          .
      *              *-------------------------------------------------*
      *              * A uscita                                        *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-900.
       acc-dti-orc-800.
      *              *-------------------------------------------------*
      *              * Se uscita per Exit                              *
      *              *-------------------------------------------------*
           move      "#"                  to   l-fat-pfo-exi-sts      .
       acc-dti-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-999.
       acc-dti-orc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo movimento                       *
      *    *-----------------------------------------------------------*
       acc-cod-tmo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmo-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zoc-ope      .
           move      w-acc-cod-tmo        to   w-cod-des-zoc-cod      .
           move      14                   to   w-cod-des-zoc-lin      .
           move      09                   to   w-cod-des-zoc-pos      .
           move      14                   to   w-cod-des-zoc-dln      .
           move      15                   to   w-cod-des-zoc-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
       acc-cod-tmo-110.
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           if        w-cod-des-zoc-ope    =    "F+"
                     go to acc-cod-tmo-115.
           if        w-cod-des-zoc-ope    =    "AC"
                     go to acc-cod-tmo-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cod-tmo-115.
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
           go to     acc-cod-tmo-110.
       acc-cod-tmo-120.
           move      w-cod-des-zoc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cod-tmo-999.
       acc-cod-tmo-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-cod-tmo          .
       acc-cod-tmo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-tmo-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
           move      w-let-arc-zoc-des    to   w-acc-cod-tmo-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to acc-cod-tmo-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmo        =    spaces
                     go to acc-cod-tmo-100.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-ord    to   w-acc-cod-tmo-ord      .
           move      w-let-arc-zoc-sgl    to   w-acc-cod-tmo-sgl      .
           move      w-let-arc-zoc-tvd    to   w-acc-cod-tmo-tvd      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
       acc-cod-tmo-410.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmo-tvd    =    spaces
                     go to acc-cod-tmo-411
           else if   w-acc-cod-tmo-tvd    =    "S"
                     go to acc-cod-tmo-412
           else if   w-acc-cod-tmo-tvd    =    "D"
                     go to acc-cod-tmo-413
           else if   w-acc-cod-tmo-tvd    =    "X"
                     go to acc-cod-tmo-414.
       acc-cod-tmo-411.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmo-420.
       acc-cod-tmo-412.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       not  = 1
                     go to acc-cod-tmo-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmo-420.
       acc-cod-tmo-413.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       =    1
                     go to acc-cod-tmo-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmo-420.
       acc-cod-tmo-414.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to acc-cod-tmo-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmo-420.
       acc-cod-tmo-418.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-tmo-100.
       acc-cod-tmo-420.
       acc-cod-tmo-440.
       acc-cod-tmo-460.
       acc-cod-tmo-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   l-fat-pfo-tmo-orc      .
       acc-cod-tmo-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-cod-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo movimento                    *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-cod-tmo        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione                       *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-acc-cod-tmo-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data documento                       *
      *    *-----------------------------------------------------------*
       acc-dat-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
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
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
                     go to acc-dat-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
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
           move      "SLCT"               to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-num-doc-prg    to   v-num                  .
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
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-doc-350.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
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
                     go to acc-num-doc-400.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-acc-num-doc-prg    =    zero
                     go to acc-num-doc-100.
           if        w-acc-dat-doc        =    zero
                     go to acc-num-doc-100.
           if        w-acc-cod-tmo        =    spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      w-acc-num-doc-prg    to   w-slc-num-oct-npg      .
           move      w-acc-cod-dpz        to   w-slc-num-oct-dpz      .
           move      w-acc-cod-tmo-sgl    to   w-slc-num-oct-sgl      .
           move      w-acc-dat-doc        to   w-slc-num-oct-dds      .
      *                  *---------------------------------------------*
      *                  * Routine di selezione                        *
      *                  *---------------------------------------------*
           perform   slc-num-oct-000      thru slc-num-oct-999        .
      *                  *---------------------------------------------*
      *                  * Se non selezionato alcun elemento : a       *
      *                  * reimpostazione                              *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-num-doc-400.
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
      *    * Select archivio [oct] in base al numero documento         *
      *    *-----------------------------------------------------------*
       slc-num-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-toc      .
           move      zero                 to   w-slc-num-oct-dat      .
           move      zero                 to   w-slc-num-oct-num      .
           move      zero                 to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Preparazione secolo, anno                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-dds    to   s-dat                  .
           move      s-saa                to   w-slc-num-oct-saa      .
       slc-num-oct-080.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-saa    to   w-slc-num-oct-nsa      .
           move      w-slc-num-oct-dpz    to   w-slc-num-oct-ndp      .
       slc-num-oct-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CNTDEN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-oct-saa    to   rf-oct-scl-ann         .
           move      w-slc-num-oct-dpz    to   rf-oct-cod-dpz         .
           move      w-slc-num-oct-sgl    to   rf-oct-sgl-num         .
           move      w-slc-num-oct-nds    to   rf-oct-num-doc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : a controllo contatore  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-800.
       slc-num-oct-200.
      *              *-------------------------------------------------*
      *              * Read-next su [oct]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-500.
       slc-num-oct-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-oct-scl-ann       not  = w-slc-num-oct-saa or
                     rf-oct-cod-dpz       not  = w-slc-num-oct-dpz or
                     rf-oct-sgl-num       not  = w-slc-num-oct-sgl or
                     rf-oct-num-doc       not  = w-slc-num-oct-nds
                     go to slc-num-oct-500.
       slc-num-oct-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-crb    >    30
                     go to slc-num-oct-520.
       slc-num-oct-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo ordine                           *
      *                  *---------------------------------------------*
           move      rf-oct-num-prt       to   w-slc-num-oct-bpt
                                              (w-slc-num-oct-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     slc-num-oct-200.
       slc-num-oct-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    =    zero
                     go to slc-num-oct-800.
      *                  *---------------------------------------------*
      *                  * Se trovato un solo elemento uscita con      *
      *                  * quello                                      *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    >    1
                     go to slc-num-oct-520.
      *                      *-----------------------------------------*
      *                      * Lettura record [oct]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt (1)
                                          to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati letti              *
      *                      *-----------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                      *-----------------------------------------*
      *                      * Ad operazioni prima dell'uscita         *
      *                      *-----------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-520.
      *                  *---------------------------------------------*
      *                  * Box di espansione                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-crb    to   w-slc-num-oct-cpb      .
           subtract  1                    from w-slc-num-oct-cpb      .
           divide    6                    into w-slc-num-oct-cpb      .
           add       1                    to   w-slc-num-oct-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-slc-num-oct-c01      .
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
           perform   slc-num-oct-950      thru slc-num-oct-989        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-nli      .
       slc-num-oct-555.
           if        w-slc-num-oct-nli    >    6
                     subtract  6          from w-slc-num-oct-nli
                     go to slc-num-oct-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       09                   to   w-slc-num-oct-nli      .
       slc-num-oct-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       slc-num-oct-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-slc-num-oct-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-slc-num-oct-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-slc-num-oct-cpa    <    w-slc-num-oct-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-slc-num-oct-nli    to   v-lin                  .
           move      09                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to slc-num-oct-582
           else if   v-key                =    "UP  "
                     go to slc-num-oct-584
           else if   v-key                =    "DOWN"
                     go to slc-num-oct-586
           else if   v-key                =    "EXIT"
                     go to slc-num-oct-598
           else if   v-key                =    "NXSC"
                     go to slc-num-oct-592
           else if   v-key                =    "PRSC"
                     go to slc-num-oct-594
           else      go to slc-num-oct-575.
       slc-num-oct-582.
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
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c01)   to   w-slc-num-oct-prt      .
      *                  *---------------------------------------------*
      *                  * Lettura record [oct]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-prt    to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dati letti                  *
      *                  *---------------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                  *---------------------------------------------*
      *                  * Ad operazioni prima dell'uscita             *
      *                  *---------------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-c01      .
           if        w-slc-num-oct-nli    =    10
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     add   1              to   w-slc-num-oct-c01
                     go to slc-num-oct-588
           else      go to slc-num-oct-575.
       slc-num-oct-588.
           if        w-slc-num-oct-nli    =    15
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-590.
           perform   slc-num-oct-950      thru slc-num-oct-989        .
           go to     slc-num-oct-550.
       slc-num-oct-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-cpa      .
           go to     slc-num-oct-596.
       slc-num-oct-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-cpa      .
       slc-num-oct-596.
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-c01      .
           multiply  6                    by   w-slc-num-oct-c01      .
           subtract  5                    from w-slc-num-oct-c01      .
           go to     slc-num-oct-590.
       slc-num-oct-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-800.
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-tes-val-key        to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-toc    to   w-acc-cod-tmo          .
           move      w-slc-num-oct-dat    to   w-acc-dat-doc          .
           move      w-slc-num-oct-num    to   w-acc-num-doc          .
      *              *-------------------------------------------------*
      *              * Test su valori estratti                         *
      *              *-------------------------------------------------*
           if        w-acc-cod-tmo        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to slc-num-oct-900.
       slc-num-oct-820.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati ed effettuazione  *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
           move      w-let-arc-zoc-des    to   w-acc-cod-tmo-des      .
      *                  *---------------------------------------------*
      *                  * Se codice errato : uscita con errore        *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : uscita con errore             *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmo        =    spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-ord    to   w-acc-cod-tmo-ord      .
           move      w-let-arc-zoc-sgl    to   w-acc-cod-tmo-sgl      .
           move      w-let-arc-zoc-tvd    to   w-acc-cod-tmo-tvd      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
       slc-num-oct-830.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmo-tvd    =    spaces
                     go to slc-num-oct-840
           else if   w-acc-cod-tmo-tvd    =    "S"
                     go to slc-num-oct-842
           else if   w-acc-cod-tmo-tvd    =    "D"
                     go to slc-num-oct-844
           else if   w-acc-cod-tmo-tvd    =    "X"
                     go to slc-num-oct-846.
       slc-num-oct-840.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-oct-850.
       slc-num-oct-842.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       not  = 1
                     go to slc-num-oct-848.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-oct-850.
       slc-num-oct-844.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       =    1
                     go to slc-num-oct-848.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-oct-850.
       slc-num-oct-846.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to slc-num-oct-848.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-oct-850.
       slc-num-oct-848.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita con errore                         *
      *                  *---------------------------------------------*
           go to     slc-num-oct-900.
       slc-num-oct-850.
       slc-num-oct-856.
       slc-num-oct-858.
       slc-num-oct-860.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-acc        to   w-tes-val-key          .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-slc-num-oct-sel      .
       slc-num-oct-940.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-c02      .
           add       5                    to   w-slc-num-oct-c02      .
           divide    6                    into w-slc-num-oct-c02      .
           move      w-slc-num-oct-c02    to   w-slc-num-oct-cpa      .
           subtract  1                    from w-slc-num-oct-c02      .
           multiply  6                    by   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c02      .
           add       5
                     w-slc-num-oct-c02  giving w-slc-num-oct-c03      .
           move      w-slc-num-oct-c03    to   w-slc-num-oct-c04      .
           if        w-slc-num-oct-c03    >    w-slc-num-oct-crb
                     move  w-slc-num-oct-crb
                                          to   w-slc-num-oct-c03      .
           move      10                   to   w-slc-num-oct-c05      .
       slc-num-oct-951.
      *              *-------------------------------------------------*
      *              * Lettura record [oct] per visualizzazione        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c02)   to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
       slc-num-oct-960.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-oct-tmo-orc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      rf-oct-dat-doc       to   v-dat                  .
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
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rf-oct-num-doc (6:6) to   v-num                  .
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
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-oct-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           if        rf-oct-dpz-arc       =    spaces
                     go to slc-num-oct-965.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rf-oct-dpz-arc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-965.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-oct-cod-arc       to   rf-dcc-cod-cli         .
           move      rf-oct-dpz-arc       to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale archivio                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      39                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           if        w-slc-num-oct-c02    not  > w-slc-num-oct-c03
                     go to slc-num-oct-951.
       slc-num-oct-970.
           if        w-slc-num-oct-c02    >    w-slc-num-oct-c04
                     go to slc-num-oct-980.
           if        w-slc-num-oct-crb    not  > 6
                     go to slc-num-oct-980.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           go to     slc-num-oct-970.
       slc-num-oct-980.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-lt1      .
           move      w-slc-num-oct-cpb    to   w-slc-num-oct-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-slc-num-oct-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-989.
           exit.
       slc-num-oct-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione work-area di accettazione                 *
      *    *-----------------------------------------------------------*
       nor-wrk-acc-000.
      *              *-------------------------------------------------*
      *              * Numero protocollo                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-num-prt          .
      *              *-------------------------------------------------*
      *              * Tipo movimento ordini clienti                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-cod-tmo-des      .
           move      zero                 to   w-acc-cod-tmo-ord      .
           move      spaces               to   w-acc-cod-tmo-sgl      .
           move      spaces               to   w-acc-cod-tmo-tvd      .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-num-doc          .
       nor-wrk-acc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *-----------------------------------------------------------*
       buf-rig-orc-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita ad errore       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-buf-rig-orc-flg      .
      *              *-------------------------------------------------*
      *              * Azzeramento numero righe nel buffer             *
      *              *-------------------------------------------------*
           move      zero                 to   w-brb-num-ele          .
      *              *-------------------------------------------------*
      *              * Normalizzazione numero protocollo prima confer- *
      *              * ma d'ordine incontrata                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orc-pro      .
      *              *-------------------------------------------------*
      *              * Start su file [ocr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-orc-prt    to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orc-500.
       buf-rig-orc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [ocr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orc-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-ocr-num-prt       not  = w-buf-rig-orc-prt
                     go to buf-rig-orc-500.
       buf-rig-orc-260.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-ocr-qta-ord       to   w-edt-qta-inc-qta      .
           move      rf-ocr-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-din    to   w-tes-dec-qta          .
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to   w-buf-rig-orc-wtr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orc-flg      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo prima confer- *
      *              * ma d'ordine cliente incontrata                  *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-pro    =    zero
                     move  rf-oct-num-prt to   w-buf-rig-orc-pro      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga ordine                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore elementi nel buffer    *
      *                  *---------------------------------------------*
           if        w-brb-num-ele        <    w-brb-max-ele
                     add   1              to   w-brb-num-ele          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      rf-ocr-num-prg       to   w-brb-num-prg
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-brb-tip-rig
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del flag di e-   *
      *                      * stensione alla descrizione              *
      *                      *-----------------------------------------*
           if        rf-ocr-des-ext       =    0
                     go to buf-rig-orc-310
           else if   rf-ocr-des-ext       =    1
                     go to buf-rig-orc-320
           else if   rf-ocr-des-ext       =    2
                     go to buf-rig-orc-330.
       buf-rig-orc-310.
      *                      *-----------------------------------------*
      *                      * Se nessuna estensione : bufferizzazione *
      *                      * descrizione contenuta nel record [ocr]  *
      *                      *-----------------------------------------*
           move      rf-ocr-des-rig       to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-320.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [ocx]            *
      *                      *-----------------------------------------*
           move      rf-ocr-num-prt       to   w-let-arc-bix-prt      .
           move      rf-ocr-num-prg       to   w-let-arc-bix-prg      .
           move      11                   to   w-let-arc-bix-trc      .
           perform   let-arc-bix-000      thru let-arc-bix-999        .
           move      w-let-arc-bix-des    to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-330.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx]            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-ocr-num-pro       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-350.
      *                      *-----------------------------------------*
      *                      * Composizione descrizione per riga di    *
      *                      * scroll                                  *
      *                      *-----------------------------------------*
           perform   cpz-des-rgs-000      thru cpz-des-rgs-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-brb-des-rig
                                              (w-brb-num-ele)         .
       buf-rig-orc-356.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           move      rf-ocr-dec-qta       to   w-brb-dec-qta
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' in ordine                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento : zero   *
      *                      *-----------------------------------------*
           if        w-buf-rig-orc-wtp    =    "C" or
                     w-buf-rig-orc-wtp    =    "A"
                     move  zero           to   w-brb-qta-orc
                                              (w-brb-num-ele)
                     go to buf-rig-orc-360.
      *                      *-----------------------------------------*
      *                      * Altrimenti                              *
      *                      *-----------------------------------------*
           move      rf-ocr-qta-ord       to   w-brb-qta-orc
                                              (w-brb-num-ele)         .
       buf-rig-orc-360.
      *                  *---------------------------------------------*
      *                  * Quantita' in fattura pari a qunatita' in    *
      *                  * ordine                                      *
      *                  *---------------------------------------------*
           move      w-brb-qta-orc
                    (w-brb-num-ele)       to   w-brb-qta-fat
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di residuo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-brb-flg-idr
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di saldo                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-brb-flg-ids
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag di forzatura a saldo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' residua pari alla quantita' da e- *
      *                  * vadere                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-brb-qta-res
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      "S"                  to   w-brb-snx-aoc
                                              (w-brb-num-ele)         .
       buf-rig-orc-400.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [ocr]       *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-200.
       buf-rig-orc-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-999.
       buf-rig-orc-999.
           exit.

      *    *===========================================================*
      *    * Composizione descrizione per riga di scroll               *
      *    *-----------------------------------------------------------*
       cpz-des-rgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si sta trattando un co- *
      *              * dice di magazzino oppure no                     *
      *              *-------------------------------------------------*
           if       (w-buf-rig-orc-wtp    =    "P" or
                     w-buf-rig-orc-wtp    =    "S" or
                     w-buf-rig-orc-wtp    =    "M"  ) and
                     w-buf-rig-orc-wtf    =    spaces
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
           else      go to cpz-des-rgs-400.
       cpz-des-rgs-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 40 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-000-d40      .
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
           move      w-buf-rig-orc-wde    to   w-des-scr-001-d25      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-001-cod      .
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
           move      w-buf-rig-orc-wde    to   w-des-scr-002-d21      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-002-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-des-scr-002-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-002-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-002-cod      .
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
           move      rf-ocr-alf-pro       to   w-des-scr-003-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-003-d25      .
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
           move      w-buf-rig-orc-wtp    to   w-des-scr-004-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-004-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-004-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-004-d21      .
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
           move      rf-ocr-alf-pro       to   w-des-scr-005-cod      .
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
           move      w-buf-rig-orc-wtp    to   w-des-scr-006-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-006-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-006-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-700.
      *                  *---------------------------------------------*
      *                  * Composizione eseguita in area di destina-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-des-scr-000        to   w-buf-rig-orc-wde      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione testata primo ordine cliente richiamato   *
      *    *-----------------------------------------------------------*
       buf-tes-orc-000.
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
      *              * Test su tipo archivio ordine                    *
      *              *-------------------------------------------------*
           if        rf-oct-tip-arc       =    "C"
                     go to buf-tes-orc-050.
      *                  *---------------------------------------------*
      *                  * Composizione messaggio di errore            *
      *                  *---------------------------------------------*
           move      "Documento non sottoponibile a fatturazione !      
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * A trattamento errore                        *
      *                  *---------------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-050.
      *              *-------------------------------------------------*
      *              * Test su data ordine                             *
      *              *-------------------------------------------------*
           if        rf-oct-dat-doc       not  > w-tes-dat-doc
                     go to buf-tes-orc-100.
      *                  *---------------------------------------------*
      *                  * Composizione messaggio di errore            *
      *                  *---------------------------------------------*
           move      "Data ordine superiore alla data della fattura !   
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * A trattamento errore                        *
      *                  *---------------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-100.
      *              *-------------------------------------------------*
      *              * Valori contenuti direttamente in record [oct]   *
      *              *-------------------------------------------------*
           move      rf-oct-cod-arc       to   w-tes-cod-cli (1)      .
           move      rf-oct-dpz-arc       to   w-tes-dpz-cli (1)      .
           move      rf-oct-cod-lng       to   w-tes-cod-lng (1)      .
       buf-tes-orc-300.
      *              *-------------------------------------------------*
      *              * Prelievo elementi testata da record [oct]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di accettazione pagina testata 1       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-fda-pt1 (1)      .
      *                  *---------------------------------------------*
      *                  * Flag di accettazione pagina testata 2       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-fda-pt2 (1)      .
      *                  *---------------------------------------------*
      *                  * Flag di accettazione sigla valuta           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-fda-vpf (1)      .
      *                  *---------------------------------------------*
      *                  * Prelievo elementi testata                   *
      *                  *---------------------------------------------*
           move      rf-oct-tip-frn       to   w-tes-tip-frn (1)      .
           move      rf-oct-arc-plf       to   w-tes-cli-plf (1)      .
           move      rf-oct-dpz-plf       to   w-tes-dpc-plf (1)      .
           move      rf-oct-inl-pgt       to   w-tes-inl-pgt (1)      .
           move      rf-oct-sgl-vpf       to   w-tes-sgl-vpf (1)      .
           move      rf-oct-dec-vpf       to   w-tes-dec-vpf (1)      .
           move      rf-oct-tdc-vpf       to   w-tes-tdc-vpf (1)      .
           move      rf-oct-ass-iva       to   w-tes-ass-iva (1)      .
           move      rf-oct-ctp-ven       to   w-tes-ctp-ven (1)      .
           if        rf-oct-fat-sep       =    "S"
                     move  "S"            to   w-gen-fat-sep
           else if   rf-oct-fat-sep       =    "O"
                     move  "O"            to   w-gen-fat-sep
                     move  w-buf-rig-orc-pro
                                          to   w-gen-coc-prt          .
           move      rf-oct-voc-des (1)   to   w-tes-voc-des (1, 1)   .
           move      rf-oct-voc-des (2)   to   w-tes-voc-des (1, 2)   .
           move      rf-oct-voc-des (3)   to   w-tes-voc-des (1, 3)   .
           move      rf-oct-voc-des (4)   to   w-tes-voc-des (1, 4)   .
           move      rf-oct-voc-des (5)   to   w-tes-voc-des (1, 5)   .
           move      rf-oct-voc-des (6)   to   w-tes-voc-des (1, 6)   .
           move      rf-oct-cod-lst       to   w-tes-cod-lst (1)      .
           move      rf-oct-csr-aac       to   w-tes-csr-aac (1)      .
           move      rf-oct-psr-aac (1)   to   w-tes-psr-aac (1, 1)   .
           move      rf-oct-psr-aac (2)   to   w-tes-psr-aac (1, 2)   .
           move      rf-oct-psr-aac (3)   to   w-tes-psr-aac (1, 3)   .
           move      rf-oct-psr-aac (4)   to   w-tes-psr-aac (1, 4)   .
           move      rf-oct-psr-aac (5)   to   w-tes-psr-aac (1, 5)   .
           move      rf-oct-csc-aac       to   w-tes-csc-aac (1)      .
           move      rf-oct-psc-aac       to   w-tes-psc-aac (1)      .
           move      rf-oct-cpv-aac       to   w-tes-cpv-aac (1)      .
           move      rf-oct-ppv-aac (1)   to   w-tes-ppv-aac (1, 1)   .
           move      rf-oct-ppv-aac (2)   to   w-tes-ppv-aac (1, 2)   .
           move      rf-oct-ppv-aac (3)   to   w-tes-ppv-aac (1, 3)   .
           move      rf-oct-cod-age       to   w-tes-cod-age (1)      .
      *                      *-----------------------------------------*
      *                      * Forzatura significativita' provvigioni  *
      *                      *                                         *
      *                      *  - '02' : no provvigioni                *
      *                      *-----------------------------------------*
           move      rf-oct-fsp-doc       to   w-tes-fsp-doc (1)      .
           move      02                   to   w-tes-fsp-doc (1)      .
           move      rf-oct-pvf-age       to   w-tes-pvf-age (1)      .
      *                      *-----------------------------------------*
      *                      * Forzatura tipo vendita per l'agente     *
      *                      *                                         *
      *                      *  - '01' : diretta                       *
      *                      *-----------------------------------------*
           move      rf-oct-tip-vpa       to   w-tes-tip-vpa (1)      .
           move      01                   to   w-tes-tip-vpa (1)      .
           move      rf-oct-cpv-aaa       to   w-tes-cpv-aaa (1)      .
           move      rf-oct-ppv-aaa (1)   to   w-tes-ppv-aaa (1, 1)   .
           move      rf-oct-ppv-aaa (2)   to   w-tes-ppv-aaa (1, 2)   .
           move      rf-oct-ppv-aaa (3)   to   w-tes-ppv-aaa (1, 3)   .
           move      rf-oct-cod-ime       to   w-tes-cod-ime (1)      .
           move      rf-oct-pvf-ime       to   w-tes-pvf-ime (1)      .
           move      rf-oct-cod-fop       to   w-tes-cod-fop (1)      .
           move      rf-oct-scp-aap       to   w-tes-scp-aap (1)      .
      *                      *-----------------------------------------*
      *                      * Subroutine di preparazione dei codici   *
      *                      * ABI e CAB e del conto corrente per      *
      *                      * l'appoggio in base alla personalizza-   *
      *                      * zione                                   *
      *                      *                                         *
      *                      * I valori interessati sono :             *
      *                      *  - 'cod-abi' = Codice Abi               *
      *                      *  - 'cod-cab' = Codice Cab               *
      *                      *  - 'ccc-app' = Conto corrente di appog- *
      *                      *                gio                      *
      *                      *-----------------------------------------*
           perform   buf-tes-orc-abi-000  thru buf-tes-orc-abi-999    .
           move      rf-oct-nos-ban       to   w-tes-nos-ban (1)      .
           move      rf-oct-nos-ccp       to   w-tes-nos-ccp (1)      .
           move      rf-oct-add-spi       to   w-tes-add-spi (1)      .
           move      rf-oct-add-spb       to   w-tes-add-spb (1)      .
           move      rf-oct-pag-dsm       to   w-tes-pag-dsm (1)      .
           move      rf-oct-pag-qaf       to   w-tes-pag-qaf (1)      .
           move      rf-oct-pag-act       to   w-tes-pag-act (1)      .
           move      rf-oct-tot-scc       to   w-pie-tot-scc (1)      .
           move      rf-oct-per-scc       to   w-pie-per-scc (1)      .
           move      rf-oct-tot-scp       to   w-pie-tot-scp (1)      .
           move      rf-oct-per-scp       to   w-pie-per-scp (1)      .
           move      zero                 to   w-buf-tes-orc-ctr      .
       buf-tes-orc-310.
           add       1                    to   w-buf-tes-orc-ctr      .
           if        w-buf-tes-orc-ctr    >    6
                     go to buf-tes-orc-320.
           move      rf-oct-spe-snx
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-snx
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-mad
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-mad
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-per
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-per
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-ibl
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-ibl
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-ibt-spe
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-ibt
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-imp
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-imp
                                              (1, w-buf-tes-orc-ctr)  .
           go to     buf-tes-orc-310.
       buf-tes-orc-320.
           go to     buf-tes-orc-400.
       buf-tes-orc-400.
      *              *-------------------------------------------------*
      *              * Eventuale aggiustamento assoggettamento iva     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      w-tes-ass-iva (1)    to   w-ags-ass-iva-aii      .
           move      rf-oct-arc-plf       to   w-ags-ass-iva-cli      .
           move      rf-oct-dat-doc       to   w-ags-ass-iva-dat      .
           perform   ags-ass-iva-000      thru ags-ass-iva-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-ags-ass-iva-flg    not  = spaces
                     go to buf-tes-orc-950.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      w-ags-ass-iva-aio    to   w-tes-ass-iva (1)      .
       buf-tes-orc-500.
      *              *-------------------------------------------------*
      *              * Valori indiretti                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anagrafica archivio                         *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-cod-cli-rag (1)  .
           move      w-let-arc-cli-via    to   w-tes-cod-cli-via (1)  .
           move      w-let-arc-cli-loc    to   w-tes-cod-cli-loc (1)  .
           move      w-let-arc-cli-piv    to   w-tes-cod-cli-piv (1)  .
           move      w-let-arc-cli-stc    to   w-tes-cod-cli-stc (1)  .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc] principale             *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli (1)    to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to buf-tes-orc-540.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-540.
      *                  *---------------------------------------------*
      *                  * Determinazione valori testata indotti da    *
      *                  * record [dcc] principale                     *
      *                  *---------------------------------------------*
           move      "C"                  to   w-det-vlt-dcc-tdt      .
           perform   det-vlt-dcc-000      thru det-vlt-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Anagrafica dipendenza cliente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli (1)    to   w-let-arc-dcc-cli      .
           move      w-tes-dpz-cli (1)    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-tes-dpz-cli-rag (1)  .
           move      w-let-arc-dcc-via    to   w-tes-dpz-cli-via (1)  .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-cli-loc (1)  .
      *                  *---------------------------------------------*
      *                  * Determinazione valori testata indotti da    *
      *                  * record [dcc] relativo alla dipendenza del   *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to buf-tes-orc-542.
           move      "C"                  to   w-det-vlt-dcd-tdt      .
           move      w-tes-dpz-cli (1)    to   w-det-vlt-dcd-dpz      .
           perform   det-vlt-dcd-000      thru det-vlt-dcd-999        .
       buf-tes-orc-542.
      *                  *---------------------------------------------*
      *                  * Archivio per la fatturazione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se fornitura diretta : oltre            *
      *                      *-----------------------------------------*
           if        w-tes-tip-frn (1)    =    01
                     go to buf-tes-orc-546.
      *                      *-----------------------------------------*
      *                      * Anagrafica cliente per fatturazione     *
      *                      *-----------------------------------------*
           move      w-tes-cli-plf (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-cli-plf-rag (1)  .
           move      w-let-arc-cli-via    to   w-tes-cli-plf-via (1)  .
           move      w-let-arc-cli-loc    to   w-tes-cli-plf-loc (1)  .
           move      w-let-arc-cli-stc    to   w-tes-cod-cli-stc (1)  .
      *                      *-----------------------------------------*
      *                      * Anagrafica commerciale principale cli-  *
      *                      * ente per fatturazione                   *
      *                      *-----------------------------------------*
           move      w-tes-cli-plf (1)    to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Se lettura OK : oltre               *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to buf-tes-orc-544.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale del cliente per fat
      -              "turazione !    "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-544.
      *                      *-----------------------------------------*
      *                      * Determinazione valori testata indotti   *
      *                      * da record [dcc] principale              *
      *                      *-----------------------------------------*
           move      "F"                  to   w-det-vlt-dcc-tdt      .
           perform   det-vlt-dcc-000      thru det-vlt-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Anagrafica commerciale dipendenza cli-  *
      *                      * ente per fatturazione                   *
      *                      *-----------------------------------------*
           move      w-tes-cli-plf (1)    to   w-let-arc-dcc-cli      .
           move      w-tes-dpc-plf (1)    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-tes-dpc-plf-rag (1)  .
           move      w-let-arc-dcc-via    to   w-tes-dpc-plf-via (1)  .
           move      w-let-arc-dcc-loc    to   w-tes-dpc-plf-loc (1)  .
      *                      *-----------------------------------------*
      *                      * Determinazione valori testata indotti   *
      *                      * da record [dcc] relativo alla dipenza   *
      *                      * del cliente per fatturazione            *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to buf-tes-orc-546.
           move      "C"                  to   w-det-vlt-dcd-tdt      .
           move      w-tes-dpc-plf (1)    to   w-det-vlt-dcd-dpz      .
           perform   det-vlt-dcd-000      thru det-vlt-dcd-999        .
       buf-tes-orc-546.
      *                      *-----------------------------------------*
      *                      * Test su codice sottoconto associato al  *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
           if        w-tes-cod-cli-stc (1)
                                          not  = zero
                     go to buf-tes-orc-548.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca il sottoconto contabile per il cliente !    
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-548.
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
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                      *-----------------------------------------*
      *                      * Test su esito determinazione            *
      *                      *-----------------------------------------*
           if        w-coe-cmb-vlt-ope    =    "CC"
                     go to buf-tes-orc-549.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Coefficiente di cambio indeterminato !            
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-549.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione coefficiente di cambio  *
      *                      *-----------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tes-cdc-vpf (1)      .
       buf-tes-orc-550.
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
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica voci descrittive                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tes-orc-ctr      .
       buf-tes-orc-560.
           add       1                    to   w-buf-tes-orc-ctr      .
           if        w-buf-tes-orc-ctr    >    6
                     go to buf-tes-orc-570.
           move      w-buf-tes-orc-ctr    to   w-let-arc-zvf-num      .
           move      w-tes-voc-des
                    (1, w-buf-tes-orc-ctr)
                                          to   w-let-arc-zvf-cod      .
           perform   let-arc-zvf-000      thru let-arc-zvf-999        .
           move      w-let-arc-zvf-des    to   w-tes-voc-des-des
                                              (1, w-buf-tes-orc-ctr)  .
           go to     buf-tes-orc-560.
       buf-tes-orc-570.
      *                  *---------------------------------------------*
      *                  * Anagrafica listini                          *
      *                  *---------------------------------------------*
           move      w-tes-cod-lst (1)    to   w-let-arc-zls-cod      .
           perform   let-arc-zls-000      thru let-arc-zls-999        .
           move      w-let-arc-zls-des    to   w-tes-cod-lst-des (1)  .
           move      w-let-arc-zls-drg    to   w-tes-cod-lst-drg (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria di sconto in riga      *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zcs-tip      .
           move      w-tes-csr-aac (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
           move      w-let-arc-zcs-des    to   w-tes-csr-aac-des (1)  .
           move      w-let-arc-zcs-per (1)
                                          to   w-tes-psr-acc (1, 1)   .
           move      w-let-arc-zcs-per (2)
                                          to   w-tes-psr-acc (1, 2)   .
           move      w-let-arc-zcs-per (3)
                                          to   w-tes-psr-acc (1, 3)   .
           move      w-let-arc-zcs-per (4)
                                          to   w-tes-psr-acc (1, 4)   .
           move      w-let-arc-zcs-per (5)
                                          to   w-tes-psr-acc (1, 5)   .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria di sconto in chiusura  *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zcs-tip      .
           move      w-tes-csc-aac (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
           move      w-let-arc-zcs-des    to   w-tes-csc-aac-des (1)  .
           move      w-let-arc-zcs-per (1)
                                          to   w-tes-psc-acc (1)      .
      *                  *---------------------------------------------*
      *                  * Anagrafica agenti                           *
      *                  *---------------------------------------------*
           move      w-tes-cod-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-rag    to   w-tes-cod-age-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica forme di pagamento               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fop (1)    to   w-let-arc-zfp-cod      .
           perform   let-arc-zfp-000      thru let-arc-zfp-999        .
           move      w-let-arc-zfp-des    to   w-tes-cod-fop-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica nostra banca per bonifico        *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ban (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ban-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica nostro c/c postale per pagamento *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ccp (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ccp-des (1)  .
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
           move      w-tes-add-spi (1)    to   w-let-arc-zin-cod      .
           move      00                   to   w-let-arc-zin-tpg      .
           perform   let-arc-zin-000      thru let-arc-zin-999        .
           move      w-let-arc-zin-des    to   w-tes-add-spi-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria spese bollo            *
      *                  *---------------------------------------------*
           move      w-tes-add-spb (1)    to   w-let-arc-zbo-cod      .
           move      00                   to   w-let-arc-zbo-tpg      .
           perform   let-arc-zbo-000      thru let-arc-zbo-999        .
           move      w-let-arc-zbo-des    to   w-tes-add-spb-des (1)  .
       buf-tes-orc-600.
      *                  *---------------------------------------------*
      *                  * Codici iva e contropartite spese in fattura *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tes-orc-ctr      .
       buf-tes-orc-610.
           add       1                    to   w-buf-tes-orc-ctr      .
           if        w-buf-tes-orc-ctr    >    6
                     go to buf-tes-orc-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [zsf]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [zsf]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-buf-tes-orc-ctr    to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori                  *
      *                      *-----------------------------------------*
           move      rf-zsf-civ-spe       to   w-pie-spe-civ
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-zsf-ccp-spe       to   w-pie-spe-ccp
                                              (1, w-buf-tes-orc-ctr)  .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     buf-tes-orc-610.
       buf-tes-orc-620.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-tes-orc-999.
       buf-tes-orc-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       buf-tes-orc-950.
      *                  *---------------------------------------------*
      *                  * Uscita con status ad errore                 *
      *                  *---------------------------------------------*
           move      "#"                  to   l-fat-pfo-exi-sts      .
       buf-tes-orc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione testata primo ordine cliente richiamato   *
      *    *                                                           *
      *    * Subroutine per la modalita' di utilizzo dei codici ABI e  *
      *    * CAB come definito dall'apposita personalizzazione         *
      *    *-----------------------------------------------------------*
       buf-tes-orc-abi-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore della perso-  *
      *              * nalizzazione                                    *
      *              *-------------------------------------------------*
           if        w-prs-dcc-abi        =    01
                     go to buf-tes-orc-abi-100
           else      go to buf-tes-orc-abi-200.
       buf-tes-orc-abi-100.
      *              *-------------------------------------------------*
      *              * Se utilizzo dei codici ABI - CAB provenienti    *
      *              * dall'anagrafica commerciale cliente             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Lettura                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-oct-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Codice istituto di credito per l'appoggio   *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-cod-abi (1)      .
      *                  *---------------------------------------------*
      *                  * Codice agenzia istituto di credito per      *
      *                  * l'appoggio                                  *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cod-cab (1)      .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente per l'appoggio        *
      *                  *---------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-ccc-app (1)      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-tes-orc-abi-900.
       buf-tes-orc-abi-200.
      *              *-------------------------------------------------*
      *              * Se utilizzo dei codici ABI - CAB provenienti    *
      *              * dal documento                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice istituto di credito per l'appoggio   *
      *                  *---------------------------------------------*
           move      rf-oct-cod-abi       to   w-tes-cod-abi (1)      .
      *                  *---------------------------------------------*
      *                  * Codice agenzia istituto di credito per      *
      *                  * l'appoggio                                  *
      *                  *---------------------------------------------*
           move      rf-oct-cod-cab       to   w-tes-cod-cab (1)      .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente per l'appoggio        *
      *                  *---------------------------------------------*
           move      rf-oct-ccc-app       to   w-tes-ccc-app (1)      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-tes-orc-abi-900.
       buf-tes-orc-abi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-tes-orc-abi-999.
       buf-tes-orc-abi-999.
           exit.

      *    *===========================================================*
      *    * Eventuale aggiustamento assoggettamento iva del documento *
      *    * in base ai dati anagrafici del cliente                    *
      *    *-----------------------------------------------------------*
       ags-ass-iva-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ags-ass-iva-flg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione valore in uscita               *
      *              *-------------------------------------------------*
           move      w-ags-ass-iva-aii    to   w-ags-ass-iva-aio      .
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        w-prs-trt-aic        not  = 01
                     go to ags-ass-iva-999.
      *              *-------------------------------------------------*
      *              * Lettura record [cli]                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-ags-ass-iva-cli    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Lettura record [dcc]                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-ags-ass-iva-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       ags-ass-iva-100.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo assogget-   *
      *                  * tamento iva del cliente                     *
      *                  *---------------------------------------------*
           if        rf-dcc-tas-ivc       =    01
                     go to ags-ass-iva-120
           else if   rf-dcc-tas-ivc       =    02
                     go to ags-ass-iva-140
           else if   rf-dcc-tas-ivc       =    03
                     go to ags-ass-iva-160
           else if   rf-dcc-tas-ivc       =    04
                     go to ags-ass-iva-180
           else      go to ags-ass-iva-900.
       ags-ass-iva-120.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : sempre   *
      *                  * soggetto ad iva                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura a zero                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-140.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : sempre   *
      *                  * esente da iva                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo che il codice iva in anagra-  *
      *                      * fica cliente sia un titolo di esenzione *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7
                     go to ags-ass-iva-142.
      *                          *-------------------------------------*
      *                          * Composizione messaggio              *
      *                          *-------------------------------------*
           move      "Assoggettamento iva errato in anagrafica contabile
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     ags-ass-iva-900.
       ags-ass-iva-142.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione assoggettamento iva     *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-160.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : con pla- *
      *                  * fond                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione relativa alla *
      *                      * gestione lettera d'intenti              *
      *                      *-----------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to ags-ass-iva-162.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [lic]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [lic]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-ags-ass-iva-cli    to   rf-lic-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
      *                      *-----------------------------------------*
      *                      * Se data documento non compresa tra data *
      *                      * riferimento iniziale e finale, si pro-  *
      *                      * cede come per cliente sempre soggetto   *
      *                      *-----------------------------------------*
           if        w-ags-ass-iva-dat    not  < rf-lic-drf-ini and
                     w-ags-ass-iva-dat    not  > rf-lic-drf-fin
                     go to ags-ass-iva-162.
      *                          *-------------------------------------*
      *                          * Emissione messaggio                 *
      *                          *-------------------------------------*
           move      "Il documento non ricade entro gli estremi della le
      -              "ttera d'intenti"    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * Si procede come per cliente sempre  *
      *                          * soggetto                            *
      *                          *-------------------------------------*
           go to     ags-ass-iva-120.
       ags-ass-iva-162.
      *                      *-----------------------------------------*
      *                      * Controllo che il codice iva in anagra-  *
      *                      * fica cliente sia un titolo di esenzione *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7
                     go to ags-ass-iva-164.
      *                          *-------------------------------------*
      *                          * Composizione messaggio              *
      *                          *-------------------------------------*
           move      "Assoggettamento iva errato in anagrafica contabile
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     ags-ass-iva-900.
       ags-ass-iva-164.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione assoggettamento iva     *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-180.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : in regi- *
      *                  * me di scissione pagamenti                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assoggettamento Iva                     *
      *                      *-----------------------------------------*
           move      00000                to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ags-ass-iva-999.
       ags-ass-iva-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ags-ass-iva-flg      .
       ags-ass-iva-999.
           exit.

      *    *===========================================================*
      *    * Confronto tra dati testata ordine cliente in esame con    *
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
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           if        rf-oct-cod-arc       =    w-tes-cod-cli (1)
                     go to cnf-tes-doc-002.
           move      "Codice cliente diverso da quello impostato !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-002.
      *                      *-----------------------------------------*
      *                      * Codice lingua                           *
      *                      *-----------------------------------------*
           if        rf-oct-cod-lng       =    w-tes-cod-lng (1)
                     go to cnf-tes-doc-010.
           move      "Codice lingua diverso da quello impostato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-010.
       cnf-tes-doc-043.
      *                      *-----------------------------------------*
      *                      * Aggiustamento assoggettamento iva del   *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      rf-oct-ass-iva       to   w-ags-ass-iva-aii      .
           move      rf-oct-arc-plf       to   w-ags-ass-iva-cli      .
           move      rf-oct-dat-doc       to   w-ags-ass-iva-dat      .
           perform   ags-ass-iva-000      thru ags-ass-iva-999        .
      *                          *-------------------------------------*
      *                          * Test su esito operazione            *
      *                          *-------------------------------------*
           if        w-ags-ass-iva-flg    not  = spaces
                     go to cnf-tes-doc-950.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-ags-ass-iva-aio    to   rf-oct-ass-iva         .
      *                      *-----------------------------------------*
      *                      * Test su assoggettamento iva             *
      *                      *-----------------------------------------*
           if        rf-oct-ass-iva       =    w-tes-ass-iva (1)
                     go to cnf-tes-doc-044.
           move      "Assoggettamento iva diverso da quello impostato ! 
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-044.
      *                      *-----------------------------------------*
      *                      * Test su contropartita vendite           *
      *                      *-----------------------------------------*
           if        rf-oct-ctp-ven       =    w-tes-ctp-ven (1)
                     go to cnf-tes-doc-046.
           move      "Contropartite vendite diverso da quello impostato 
      -              "!              "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-046.
      *                      *-----------------------------------------*
      *                      * Test su inoltro pagamenti               *
      *                      *-----------------------------------------*
           if        rf-oct-inl-pgt       =    w-tes-inl-pgt (1)
                     go to cnf-tes-doc-048.
           move      "Inoltro pagamenti diverso da quello impostato !   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-048.
      *                      *-----------------------------------------*
      *                      * Test su sigla valuta                    *
      *                      *-----------------------------------------*
           if        rf-oct-sgl-vpf       =    w-tes-sgl-vpf (1)
                     go to cnf-tes-doc-050.
           move      "Sigla valuta diverso da quello impostato !        
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-050.
      *                      *-----------------------------------------*
      *                      * Test su decimali valuta                 *
      *                      *-----------------------------------------*
           if        rf-oct-dec-vpf       =    w-tes-dec-vpf (1)
                     go to cnf-tes-doc-051.
           move      "Decimali valuta diverso da quelli della valuta imp
      -              "ostata !       "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-051.
      *                      *-----------------------------------------*
      *                      * Test su tipo di cambio valuta           *
      *                      *-----------------------------------------*
           if        rf-oct-tdc-vpf       =    w-tes-tdc-vpf (1)
                     go to cnf-tes-doc-052.
           move      "Tipo di cambio valuta diverso da quello della valu
      -              "ta impostata ! "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-052.
      *                      *-----------------------------------------*
      *                      * Test su data scadenza pagamento manuale *
      *                      *-----------------------------------------*
           if        rf-oct-pag-dsm       =    w-tes-pag-dsm (1)
                     go to cnf-tes-doc-054.
           move      "Data scadenza pagamento manuale diversa da quella 
      -              "impostata !    "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-054.
      *                      *-----------------------------------------*
      *                      * Test su quota a forfait pagamento       *
      *                      *-----------------------------------------*
           if        rf-oct-pag-qaf       =    w-tes-pag-qaf (1)
                     go to cnf-tes-doc-062.
           move      "Quota a forfait del pagamento diversa da quella im
      -              "postata !      "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-062.
      *                      *-----------------------------------------*
      *                      * Test su fatturazione separata per il    *
      *                      * documento                               *
      *                      *-----------------------------------------*
           if        rf-oct-fat-sep       not  = "S"
                     go to cnf-tes-doc-064.
           move      "L'ordine richiamato prevede che debba essere fattu
      -              "rata a parte ! "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-064.
      *                      *-----------------------------------------*
      *                      * Test su spese per fatturazione          *
      *                      *-----------------------------------------*
           if        w-prs-spe-fat-tac    not  = 01
                     go to cnf-tes-doc-066.
           if        rf-oct-spe-snx (1)   =    w-pie-spe-snx (1, 1) and
                     rf-oct-spe-snx (2)   =    w-pie-spe-snx (1, 2) and
                     rf-oct-spe-snx (3)   =    w-pie-spe-snx (1, 3) and
                     rf-oct-spe-snx (4)   =    w-pie-spe-snx (1, 4) and
                     rf-oct-spe-snx (5)   =    w-pie-spe-snx (1, 5) and
                     rf-oct-spe-snx (6)   =    w-pie-spe-snx (1, 6)
                     go to cnf-tes-doc-066.
           move      "L'ordine richiamato presenta modalita' di addebito
      -              " delle spese di"    to   w-err-box-err-msg      .
           move      "chiusura diverse da quelle del primo documento ric
      -              "hiamato !      "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           go to     cnf-tes-doc-950.
       cnf-tes-doc-066.
           go to     cnf-tes-doc-100.
       cnf-tes-doc-100.
      *              *-------------------------------------------------*
      *              * Valori non-bloccanti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori comunque non bloccanti               *
      *                  *---------------------------------------------*
           if        rf-oct-dpz-arc       =    w-tes-dpz-cli (1)
                     go to cnf-tes-doc-120.
      *                      *-----------------------------------------*
      *                      * Messaggio all'operatore                 *
      *                      *-----------------------------------------*
           move      "Dipendenza cliente diversa da quella impostata !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-120.
      *                  *---------------------------------------------*
      *                  * Se ordine fatturabile                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su dipendenza archivio per fattu-  *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           if        rf-oct-dpz-plf       =    w-tes-dpc-plf (1)
                     go to cnf-tes-doc-162.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Dipendenza cliente per fatturazione diversa da que
      -              "lla impostata !"    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-162.
      *                      *-----------------------------------------*
      *                      * Test su forma di pagamento              *
      *                      *-----------------------------------------*
           if        rf-oct-cod-fop       =    w-tes-cod-fop (1)
                     go to cnf-tes-doc-164.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Forma di pagamento diversa da quella impostata !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-164.
      *                      *-----------------------------------------*
      *                      * Test su codice ABI d'appoggio           *
      *                      *-----------------------------------------*
           if        w-tes-cod-abi (1)    =    zero
                     go to cnf-tes-doc-166.
           if        rf-oct-cod-abi       =    w-tes-cod-abi (1)
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
           if        rf-oct-cod-cab       =    w-tes-cod-cab (1)
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
           if        rf-oct-ccc-app       =    w-tes-ccc-app (1)
                     go to cnf-tes-doc-170.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "C/C del cliente per l'appoggio diverso da quello i
      -              "mpostato !     "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-170.
      *                      *-----------------------------------------*
      *                      * Test su nostra banca per bonifico       *
      *                      *-----------------------------------------*
           if        w-tes-nos-ban (1)    =    spaces
                     go to cnf-tes-doc-172.
           if        rf-oct-nos-ban       =    w-tes-nos-ban (1)
                     go to cnf-tes-doc-172.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Nostra banca per appoggio bonifico diversa da quel
      -              "la impostata ! "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-172.
      *                      *-----------------------------------------*
      *                      * Test su nostro c/c postale per bonifico *
      *                      *-----------------------------------------*
           if        w-tes-nos-ccp (1)    =    spaces
                     go to cnf-tes-doc-174.
           if        rf-oct-nos-ccp       =    w-tes-nos-ccp (1)
                     go to cnf-tes-doc-174.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Nostro c/c postale diverso da quello impostato !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-174.
      *                      *-----------------------------------------*
      *                      * Test su codice agente                   *
      *                      *-----------------------------------------*
       cnf-tes-doc-176.
      *                      *-----------------------------------------*
      *                      * Test su trattamento provvigioni         *
      *                      *-----------------------------------------*
       cnf-tes-doc-178.
      *                      *-----------------------------------------*
      *                      * Test su tipo vendita per l'agente       *
      *                      *-----------------------------------------*
       cnf-tes-doc-180.
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
           add       rf-oct-pag-act       to   w-tes-pag-act (1)      .
      *                  *---------------------------------------------*
      *                  * Provvigioni a forfait per il documento      *
      *                  *---------------------------------------------*
           if        rf-oct-fsp-doc       =    03
                     add  rf-oct-pvf-age  to   w-tes-pvf-age (1)      .
       cnf-tes-doc-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi per piede documento   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale sconto in chiusura                   *
      *                  *---------------------------------------------*
           add       rf-oct-tot-scc       to   w-pie-tot-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-oct-per-scc       not  = w-pie-per-scc (1)
                     move  zero           to   w-pie-per-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Totale sconto pagamento                     *
      *                  *---------------------------------------------*
           add       rf-oct-tot-scp       to   w-pie-tot-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-oct-per-scp       not  = w-pie-per-scp (1)
                     move  zero           to   w-pie-per-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Totale spese in fattura                     *
      *                  *---------------------------------------------*
           add       rf-oct-spe-imp (1)   to   w-pie-spe-imp (1, 1)   .
           add       rf-oct-spe-imp (2)   to   w-pie-spe-imp (1, 2)   .
           add       rf-oct-spe-imp (3)   to   w-pie-spe-imp (1, 3)   .
           add       rf-oct-spe-imp (4)   to   w-pie-spe-imp (1, 4)   .
           add       rf-oct-spe-imp (5)   to   w-pie-spe-imp (1, 5)   .
           add       rf-oct-spe-imp (6)   to   w-pie-spe-imp (1, 6)   .
      *                  *---------------------------------------------*
      *                  * Percentuali spese in fattura                *
      *                  *---------------------------------------------*
           if        rf-oct-spe-snx (1)   =    0 or
                     rf-oct-spe-per (1)   not  = w-pie-spe-per (1, 1)
                     move  zero           to   w-pie-spe-per (1, 1)   .
           if        rf-oct-spe-snx (2)   =    0 or
                     rf-oct-spe-per (2)   not  = w-pie-spe-per (1, 2)
                     move  zero           to   w-pie-spe-per (1, 2)   .
           if        rf-oct-spe-snx (3)   =    0 or
                     rf-oct-spe-per (3)   not  = w-pie-spe-per (1, 3)
                     move  zero           to   w-pie-spe-per (1, 3)   .
           if        rf-oct-spe-snx (4)   =    0 or
                     rf-oct-spe-per (4)   not  = w-pie-spe-per (1, 4)
                     move  zero           to   w-pie-spe-per (1, 4)   .
           if        rf-oct-spe-snx (5)   =    0 or
                     rf-oct-spe-per (5)   not  = w-pie-spe-per (1, 5)
                     move  zero           to   w-pie-spe-per (1, 5)   .
           if        rf-oct-spe-snx (6)   =    0 or
                     rf-oct-spe-per (6)   not  = w-pie-spe-per (1, 6)
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
           move      "#"                  to   l-fat-pfo-exi-sts      .
       cnf-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine cliente              *
      *    *-----------------------------------------------------------*
       sdc-fat-dfm-000.
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
                     move  "#"            to   l-fat-pfo-exi-sts      .
       sdc-fat-dfm-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine cliente              *
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
      *              * Numero riga da trattare                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-ctr-rig          .
       acc-fun-sdc-200.
           add       1                    to   w-sdc-ctr-rig          .
           if        w-sdc-ctr-rig        >    w-brb-num-ele
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
           if        l-fat-pfo-fky-tab    =    spaces
                     go to acc-fun-sdc-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di Tab in corso        *
      *                  *---------------------------------------------*
           move      spaces               to   l-fat-pfo-fky-tab      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Quantita' in fattura                        *
      *                  *---------------------------------------------*
           perform   acc-qta-fat-000      thru acc-qta-fat-999        .
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
           go to     acc-fun-sdc-999.
       acc-fun-sdc-920.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
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
           move      w-acc-cod-tmo        to   v-alf                  .
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
           move      w-acc-cod-tmo-des    to   v-alf                  .
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
           move      "               Descrizione              | In ordin
      -              "e | In fattura| I |  Residuo  "
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
           go to     pmt-fun-sdc-700.
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
           move      "       Descrizione            Codice    | In ordin
      -              "e | In fattura| I |  Residuo  "
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
           go to     pmt-fun-sdc-700.
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
           move      "     Descrizione            Codice      | In ordin
      -              "e | In fattura| I |  Residuo  "
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
           go to     pmt-fun-sdc-700.
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
           move      "    Codice            Descrizione       | In ordin
      -              "e | In fattura| I |  Residuo  "
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
           go to     pmt-fun-sdc-700.
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
           move      "      Codice            Descrizione     | In ordin
      -              "e | In fattura| I |  Residuo  "
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
           go to     pmt-fun-sdc-700.
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
           move      "       Descrizione            Codice    | In ordin
      -              "e | In fattura| I |  Residuo  "
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
           go to     pmt-fun-sdc-700.
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
           move      "     Descrizione            Codice      | In ordin
      -              "e | In fattura| I |  Residuo  "
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
           move      w-brb-des-rig
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
           move      w-brb-qta-orc
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-brb-dec-qta
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
           if        w-brb-tip-rig
                    (w-sdc-wrk-rig)       not  = "C" and
                     w-brb-tip-rig
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
           if        w-brb-snx-aoc
                    (w-sdc-wrk-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
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
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing quantita'               *
      *                              *---------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-fat
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-brb-dec-qta
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      54                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
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
           move      w-brb-flg-idr
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
           move      w-brb-flg-ids
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Quantita' residua                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing quantita'               *
      *                              *---------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-res
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-brb-dec-qta
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      70                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
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
      *    * Accettazione campo saldaconto : Quantita' in fattura      *
      *    *-----------------------------------------------------------*
       acc-qta-fat-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-brb-tip-rig
                    (w-sdc-ctr-rig)       =    "A" or
                     w-brb-tip-rig
                    (w-sdc-ctr-rig)       =    "C"
                     go to acc-qta-fat-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-brb-qta-fat
                    (w-sdc-ctr-rig)       to   w-sav-qta-fat          .
           move      w-brb-flg-fzs
                    (w-sdc-ctr-rig)       to   w-sav-flg-fzs          .
       acc-qta-fat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-brb-dec-qta
                    (w-sdc-ctr-rig)       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-brb-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-brb-qta-fat
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-fat-999.
       acc-qta-fat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)         .
       acc-qta-fat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo compatibilita' fra segno quantita'*
      *                  * in fattura e segno quantita' in ordine      *
      *                  *---------------------------------------------*
           if        w-brb-qta-orc
                    (w-sdc-ctr-rig)       >    zero and
                     w-brb-qta-fat
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-fat-100
           else if   w-brb-qta-orc
                    (w-sdc-ctr-rig)       <    zero and
                     w-brb-qta-fat
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-fat-100.
      *                  *---------------------------------------------*
      *                  * Se Pf1 : controllo che il valore impostato  *
      *                  * non sia zero                                *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-qta-fat-420.
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-fat-100.
       acc-qta-fat-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-fat-600.
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)       not  = w-sav-qta-fat
                     move   w-sav-qta-fat to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)
                     go to  acc-qta-fat-100.
       acc-qta-fat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' in ordine e quantita' in fattura  *
      *                  * in work di comodo senza segno               *
      *                  *---------------------------------------------*
           move      w-brb-qta-orc
                    (w-sdc-ctr-rig)       to   w-qss-qta-orc          .
           move      w-brb-qta-fat
                    (w-sdc-ctr-rig)       to   w-qss-qta-fat          .
      *                  *---------------------------------------------*
      *                  * Se Slct                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-qta-fat-620.
      *                      *-----------------------------------------*
      *                      * Se valore gia' presente                 *
      *                      *-----------------------------------------*
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-fat-610.
      *                          *-------------------------------------*
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
           move      zero                 to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)         .
           move      zero                 to   w-qss-qta-fat          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
           move      w-brb-qta-orc
                    (w-sdc-ctr-rig)       to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)         .
           move      w-brb-qta-orc
                    (w-sdc-ctr-rig)       to   w-qss-qta-fat          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-620.
      *                  *---------------------------------------------*
      *                  * Se Pf1                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to  acc-qta-fat-640.
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se quantita' in fattura maggiore o  *
      *                          * uguale a quella in ordine : spaces  *
      *                          *-------------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-orc
                     go to  acc-qta-fat-630.
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-630.
      *                          *-------------------------------------*
      *                          * Altrimenti : si inverte il valore   *
      *                          * attuale del flag                    *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     move  "S"            to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)
           else      move  spaces         to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-640.
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
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)        not  = w-sav-qta-fat
                     go to  acc-qta-fat-650.
           go to     acc-qta-fat-700.
       acc-qta-fat-650.
      *                          *-------------------------------------*
      *                          * Se valore impostato diverso dal     *
      *                          * precedente                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in ordine  *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-orc
                     go to  acc-qta-fat-655.
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-655.
      *                              *---------------------------------*
      *                              * Altrimenti : valore inalterato  *
      *                              *---------------------------------*
           go to     acc-qta-fat-700.
       acc-qta-fat-700.
      *                  *---------------------------------------------*
      *                  * Trattamento altri valori riga               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag indicatore di residuo              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-fat-710.
           move      spaces               to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-720.
       acc-qta-fat-710.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in ordine  *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-orc
                     go to  acc-qta-fat-715.
           move      spaces               to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-720.
       acc-qta-fat-715.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a '<'                   *
      *                              *---------------------------------*
           move      "<"                  to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-720.
       acc-qta-fat-720.
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-fat-730.
           move      "S"                  to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-740.
       acc-qta-fat-730.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura minore  *
      *                              * o uguale a quella in ordine :   *
      *                              * viene forzato il valore spaces  *
      *                              *---------------------------------*
           if        w-qss-qta-fat        >    w-qss-qta-orc
                     go to  acc-qta-fat-735.
           move      spaces               to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-740.
       acc-qta-fat-735.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a "+"                   *
      *                              *---------------------------------*
           move      "+"                  to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-740.
       acc-qta-fat-740.
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-fat-750.
           move      zero                 to   w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-760.
       acc-qta-fat-750.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in ordine  *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-orc
                     go to  acc-qta-fat-755.
           move      zero                 to   w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-760.
       acc-qta-fat-755.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore e' pari  *
      *                              * alla differenza tra quantita'   *
      *                              * in ordine e qta' in fattura     *
      *                              *---------------------------------*
           subtract  w-brb-qta-fat
                    (w-sdc-ctr-rig)       from w-brb-qta-orc
                                              (w-sdc-ctr-rig)
                                        giving w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
      *                              *---------------------------------*
      *                              * Avvertimento                    *
      *                              *---------------------------------*
      *                              *---------------------------------*
      *                              * A visualizzazione valori riga   *
      *                              *---------------------------------*
           go to     acc-qta-fat-760.
       acc-qta-fat-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' in fattura                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-brb-qta-fat
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
           move      w-brb-flg-idr
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
           move      w-brb-flg-ids
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita'                   *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-res
                    (w-sdc-ctr-rig)       to   w-edt-qta-inc-qta      .
           move      w-brb-dec-qta
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
       acc-qta-fat-800.
       acc-qta-fat-999.
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
           if        w-brb-tip-rig
                    (w-sdc-ctr-rig)       not  = "A" and
                     w-brb-tip-rig
                    (w-sdc-ctr-rig)       not  = "C"
                     go to acc-snx-aoc-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-brb-snx-aoc
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
           if        w-sdc-ctr-rig        <    w-brb-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
       acc-snx-aoc-150.
           move      "SLCT"               to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-brb-snx-aoc
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
           move      v-alf                to   w-brb-snx-aoc
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
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = w-sav-snx-aoc
                     move   w-sav-snx-aoc to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)
                     go to  acc-snx-aoc-100.
      *                  *---------------------------------------------*
      *                  * Se valore presente lo si abblenca, altri-   *
      *                  * menti lo si forza a 'S'                     *
      *                  *---------------------------------------------*
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     move  spaces         to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)
           else      move  "S"            to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)         .
       acc-snx-aoc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = "S" and
                     w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = "N" and
                     w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-snx-aoc-100.
       acc-snx-aoc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore                      *
      *                  *---------------------------------------------*
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       =    "N"
                     move  spaces         to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Si/No addebito o commento   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      59                   to   v-pos                  .
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-snx-aoc-800.
       acc-snx-aoc-999.
           exit.

      *    *===========================================================*
      *    * Caricamento righe in catena                               *
      *    *-----------------------------------------------------------*
       car-rig-cat-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione buffer righe ordine          *
      *              *-------------------------------------------------*
           move      zero                 to   w-crc-wrk-ctr          .
       car-rig-cat-100.
           add       1                    to   w-crc-wrk-ctr          .
           if        w-crc-wrk-ctr        >    w-brb-num-ele
                     go to car-rig-cat-750.
      *                  *---------------------------------------------*
      *                  * Selezione su righe ordine nel buffer        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-brb-tip-rig
                    (w-crc-wrk-ctr)       =    "A" or
                     w-brb-tip-rig
                    (w-crc-wrk-ctr)       =    "C"
                     go to car-rig-cat-110
           else      go to car-rig-cat-120.
       car-rig-cat-110.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           if        w-brb-snx-aoc
                    (w-crc-wrk-ctr)       not  = "S"
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
           if        w-brb-qta-fat
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
      *                  * Normalizzazione record [ocr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ocr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-gen-prt-orc        to   rf-ocr-num-prt         .
           move      w-brb-num-prg
                    (w-crc-wrk-ctr)       to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       car-rig-cat-600.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori contenuti direttamente in re-    *
      *                      * cord [ocr]                              *
      *                      *-----------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
           move      rf-ocr-bld-flb       to   w-rig-bld-flb (1)      .
           move      rf-ocr-bld-tpb       to   w-rig-bld-tpb (1)      .
           move      rf-ocr-bld-rgb       to   w-rig-bld-rgb (1)      .
           move      rf-ocr-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
           move      rf-ocr-tip-mag       to   w-rig-tip-mag (1)      .
           move      rf-ocr-num-pro       to   w-rig-num-pro (1)      .
           move      rf-ocr-alf-pro       to   w-rig-alf-pro (1)      .
           move      rf-ocr-sgl-vrn       to   w-rig-sgl-vrn (1)      .
           move      rf-ocr-cop-scl       to   w-rig-cop-scl (1)      .
           move      rf-ocr-des-ext       to   w-rig-des-ext (1)      .
           move      rf-ocr-des-rig       to   w-rig-des-rig (1)      .
           move      rf-ocr-tip-pro       to   w-rig-tip-pro (1)      .
           move      rf-ocr-cod-iva       to   w-rig-coi-rig (1)      .
           move      rf-ocr-ctp-ven       to   w-rig-ctv-rig (1)      .
           move      rf-ocr-umi-ven       to   w-rig-umi-ven (1)      .
           move      rf-ocr-dec-qta       to   w-rig-dec-qta (1)      .
           move      rf-ocr-snx-2qt       to   w-rig-snx-2qt (1)      .
           move      rf-ocr-dec-2qt       to   w-rig-dec-2qt (1)      .
           move      rf-ocr-qta-a02       to   w-rig-qta-a02 (1)      .
           move      rf-ocr-snx-3qt       to   w-rig-snx-3qt (1)      .
           move      rf-ocr-dec-3qt       to   w-rig-dec-3qt (1)      .
           move      rf-ocr-qta-a03       to   w-rig-qta-a03 (1)      .
           move      rf-ocr-dec-prz       to   w-rig-dec-prz (1)      .
           move      rf-ocr-sgl-vps       to   w-rig-sgl-vps (1)      .
           move      rf-ocr-dec-vps       to   w-rig-dec-vps (1)      .
           move      rf-ocr-tdc-vps       to   w-rig-tdc-vps (1)      .
           move      rf-ocr-prz-lrs       to   w-rig-prz-lrs (1)      .
           move      rf-ocr-prz-nts       to   w-rig-prz-nts (1)      .
           move      rf-ocr-snx-2pz       to   w-rig-snx-2pz (1)      .
           move      rf-ocr-prz-a02       to   w-rig-prz-a02 (1)      .
           move      rf-ocr-sgl-vpl       to   w-rig-sgl-vpl (1)      .
           move      rf-ocr-dec-vpl       to   w-rig-dec-vpl (1)      .
           move      rf-ocr-tdc-vpl       to   w-rig-tdc-vpl (1)      .
           move      rf-ocr-prz-vpl       to   w-rig-prz-vpl (1)      .
           move      rf-ocr-cdc-vpl       to   w-rig-cdc-vpl (1)      .
           move      rf-ocr-ccr-vpl       to   w-rig-ccr-vpl (1)      .
           move      rf-ocr-plm-vpl       to   w-rig-plm-vpl (1)      .
           move      rf-ocr-tlm-vpl       to   w-rig-tlm-vpl (1)      .
           move      rf-ocr-map-vpl       to   w-rig-map-vpl (1)      .
           move      rf-ocr-epz-rgf       to   w-rig-epz-rgf (1)      .
           move      rf-ocr-csr-aap       to   w-rig-csr-aap (1)      .
           move      rf-ocr-psr-aap (1)   to   w-rig-psr-aap (1, 1)   .
           move      rf-ocr-psr-aap (2)   to   w-rig-psr-aap (1, 2)   .
           move      rf-ocr-psr-aap (3)   to   w-rig-psr-aap (1, 3)   .
           move      rf-ocr-psr-aap (4)   to   w-rig-psr-aap (1, 4)   .
           move      rf-ocr-psr-aap (5)   to   w-rig-psr-aap (1, 5)   .
           move      rf-ocr-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-ocr-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-ocr-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-ocr-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-ocr-per-scr (5)   to   w-rig-per-scr (1, 5)   .
           move      rf-ocr-epz-pes       to   w-rig-epz-pes (1)      .
           move      rf-ocr-sgl-vpc       to   w-rig-sgl-vpc (1)      .
           move      rf-ocr-dec-vpc       to   w-rig-dec-vpc (1)      .
           move      rf-ocr-tdc-vpc       to   w-rig-tdc-vpc (1)      .
           move      rf-ocr-cdc-vpc       to   w-rig-cdc-vpc (1)      .
           move      rf-ocr-dec-cos       to   w-rig-dec-cos (1)      .
           move      rf-ocr-cos-rif       to   w-rig-cos-rif (1)      .
           move      rf-ocr-iau-rig       to   w-rig-iau-rig (1)      .
           move      rf-ocr-cpv-aap       to   w-rig-cpv-aap (1)      .
           move      rf-ocr-ppv-aap (1)   to   w-rig-ppv-aap (1, 1)   .
           move      rf-ocr-ppv-aap (2)   to   w-rig-ppv-aap (1, 2)   .
           move      rf-ocr-ppv-aap (3)   to   w-rig-ppv-aap (1, 3)   .
           move      rf-ocr-fsp-rig       to   w-rig-fsp-rig (1)      .
           move      rf-ocr-cpv-rig       to   w-rig-cpv-rig (1)      .
           move      rf-ocr-ppv-rig (1)   to   w-rig-ppv-rig (1, 1)   .
           move      rf-ocr-ppv-rig (2)   to   w-rig-ppv-rig (1, 2)   .
           move      rf-ocr-ppv-rig (3)   to   w-rig-ppv-rig (1, 3)   .
           move      rf-ocr-pvf-rig       to   w-rig-pvf-rig (1)      .
           move      rf-ocr-ocl-dat       to   w-rig-ocl-dat (1)      .
           move      rf-ocr-ocl-num       to   w-rig-ocl-num (1)      .
           move      rf-ocr-cmc-tip       to   w-rig-cmc-tip (1)      .
           move      rf-ocr-cmc-dat       to   w-rig-cmc-dat (1)      .
           move      rf-ocr-cmc-num       to   w-rig-cmc-num (1)      .
           move      rf-oct-tmo-orc       to   w-rig-coc-tip (1)      .
           move      rf-oct-dat-doc       to   w-rig-coc-dat (1)      .
           move      rf-oct-num-doc       to   w-rig-coc-num (1)      .
           move      spaces               to   w-rig-bcc-tip (1)      .
           move      zero                 to   w-rig-bcc-dat (1)      .
           move      zero                 to   w-rig-bcc-num (1)      .
           move      rf-ocr-flg-puq       to   w-rig-flg-puq (1)      .
       car-rig-cat-400.
      *                      *-----------------------------------------*
      *                      * Valori contenuti in buffer righe ordine *
      *                      *-----------------------------------------*
       car-rig-cat-420.
      *                          *-------------------------------------*
      *                          * Quantita' per la vendita            *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A"
                     move  zero           to   w-rig-qta-ven (1)
           else      move  w-brb-qta-fat
                          (w-crc-wrk-ctr) to   w-rig-qta-ven (1)      .
       car-rig-cat-425.
      *                          *-------------------------------------*
      *                          * Trattamento della seconda quantita' *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se trattamento necessario  *
      *                              *---------------------------------*
           if        w-rig-snx-2qt (1)    not  = 1
                     go to car-rig-cat-430.
           if        w-rig-qta-ven (1)    =    zero
                     move  zero           to   w-rig-qta-a02 (1)
                     go to car-rig-cat-430.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
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
       car-rig-cat-430.
      *                          *-------------------------------------*
      *                          * Coefficiente di cambio valuta per   *
      *                          * i prezzi standard alla data del do- *
      *                          * cumento                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-tes-cdc-vpf (1)    to   w-rig-cdc-vps (1)      .
       car-rig-cat-440.
      *                          *-------------------------------------*
      *                          * Prezzo di vendita e prezzo netto    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se valuta per il prezzo conte-  *
      *                              * nuta nella riga ordine e' pari  *
      *                              * alla valuta per fatturazione    *
      *                              * del documento, vuol dire che    *
      *                              * l'eventuale cambio e' gia' sta- *
      *                              * to eseguito al momento della    *
      *                              * ordine e quindi il prezzo rima- *
      *                              * ne quello contenuto nel record  *
      *                              * [ocr]                           *
      *                              *---------------------------------*
           if        rf-ocr-sgl-vpp       =    w-tes-sgl-vpf (1) and
                     rf-ocr-dec-vpp       =    w-tes-dec-vpf (1) and
                     rf-ocr-tdc-vpp       =    w-tes-tdc-vpf (1)
                     move  rf-ocr-prz-ven to   w-rig-prz-ven (1)
                     move  rf-ocr-prz-net to   w-rig-prz-net (1)
                     go to car-rig-cat-460.
      *                              *---------------------------------*
      *                              * Trasformazione prezzo di ven-   *
      *                              * dita espresso nella valuta per  *
      *                              * il prezzo nel valore espresso   *
      *                              * nella valuta di fatturazione    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione coefficien-  *
      *                                  * te di cambio per la valuta  *
      *                                  * per il prezzo alla data del *
      *                                  * documento                   *
      *                                  *-----------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-ocr-sgl-vpp       to   w-coe-cmb-vlt-sdv      .
           move      rf-ocr-tdc-vpp       to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                                  *-----------------------------*
      *                                  * Se esito determinazione ne- *
      *                                  * gativo : bufferizzazione    *
      *                                  * coefficiente di cambio con- *
      *                                  * tenuto nel record [ocr]     *
      *                                  *-----------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                                  *-----------------------------*
      *                                  * Conversione da valuta per   *
      *                                  * il prezzo a valuta base     *
      *                                  *-----------------------------*
           move      rf-ocr-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-ocr-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-ocr-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-ocr-prz-ven       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                                  *-----------------------------*
      *                                  * Conversione da valuta base  *
      *                                  * a valuta per fatturazione   *
      *                                  *-----------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                                  *-----------------------------*
      *                                  * Bufferizzazione nuovo prez- *
      *                                  * zo                          *
      *                                  *-----------------------------*
           move      w-cvs-vlt-aav        to   w-rig-prz-ven (1)      .
      *                                  *-----------------------------*
      *                                  * Determinazione prezzo netto *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-cal-prz-net-prz      .
           move      w-rig-per-scr (1, 1) to   w-cal-prz-net-psc (1)  .
           move      w-rig-per-scr (1, 2) to   w-cal-prz-net-psc (2)  .
           move      w-rig-per-scr (1, 3) to   w-cal-prz-net-psc (3)  .
           move      w-rig-per-scr (1, 4) to   w-cal-prz-net-psc (4)  .
           move      w-rig-per-scr (1, 5) to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   w-rig-prz-net (1)      .
       car-rig-cat-460.
      *                          *-------------------------------------*
      *                          * Trattamento legame valutario        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se da effettuare           *
      *                              *---------------------------------*
           if        w-tes-mac-lvl (1)    =    00
                     go to car-rig-cat-480.
           if        w-rig-sgl-vpl (1)    =    spaces
                     go to car-rig-cat-480.
      *                              *---------------------------------*
      *                              * Deviazione a seconda se cambio  *
      *                              * da applicare o meno             *
      *                              *---------------------------------*
           if        w-tes-mac-lvl (1)    =    21
                     go to car-rig-cat-462
           else      go to car-rig-cat-466.
       car-rig-cat-462.
      *                              *---------------------------------*
      *                              * Se cambio da applicare          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione coefficiente *
      *                                  * di cambio effettivo per il  *
      *                                  * legame valutario alla data  *
      *                                  * del documento               *
      *                                  *-----------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vpl (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-tdc-vpl (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                                  *-----------------------------*
      *                                  * Se esito determinazione ne- *
      *                                  * gativo : bufferizzazione    *
      *                                  * coefficiente di cambio con- *
      *                                  * tenuto nel record [ocr]     *
      *                                  *-----------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                                  *-----------------------------*
      *                                  * Bufferizzazione coefficien- *
      *                                  * te di cambio determinato    *
      *                                  *-----------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vpl (1)      .
      *                                  *-----------------------------*
      *                                  * Salvataggio prezzo di ven-  *
      *                                  * dita precedente             *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-sav-prz-ven          .
      *                                  *-----------------------------*
      *                                  * Applicazione cambio per le- *
      *                                  * game valutario              *
      *                                  *-----------------------------*
           move      w-rig-prz-vpl (1)    to   w-rig-prz-ven (1)      .
      *                                  *-----------------------------*
      *                                  * Parametri in input          *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-lvl-prz-prz          .
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
           move      w-lvl-prz-prz        to   w-rig-prz-ven (1)      .
      *                                  *-----------------------------*
      *                                  * Se prezzo di vendita inva-  *
      *                                  * riato : oltre               *
      *                                  *-----------------------------*
           if        w-rig-prz-ven (1)    =    w-sav-prz-ven
                     go to car-rig-cat-464.
      *                                  *-----------------------------*
      *                                  * Determinazione prezzo netto *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-cal-prz-net-prz      .
           move      w-rig-per-scr (1, 1) to   w-cal-prz-net-psc (1)  .
           move      w-rig-per-scr (1, 2) to   w-cal-prz-net-psc (2)  .
           move      w-rig-per-scr (1, 3) to   w-cal-prz-net-psc (3)  .
           move      w-rig-per-scr (1, 4) to   w-cal-prz-net-psc (4)  .
           move      w-rig-per-scr (1, 5) to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   w-rig-prz-net (1)      .
      *                                  *-----------------------------*
      *                                  * Provvigioni in riga         *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Se prodotto similare :  *
      *                                      * oltre                   *
      *                                      *-------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-464.
      *                                      *-------------------------*
      *                                      * Test se da trattare     *
      *                                      *-------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to car-rig-cat-464.
      *                                      *-------------------------*
      *                                      * Determinazione          *
      *                                      *-------------------------*
           perform   det-ppv-rig-000      thru det-ppv-rig-999        .
       car-rig-cat-464.
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-480.
       car-rig-cat-466.
      *                              *---------------------------------*
      *                              * Se cambio da non applicare      *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-480.
       car-rig-cat-480.
      *                          *-------------------------------------*
      *                          * Importo in riga                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-482
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-484
           else      go to car-rig-cat-486.
       car-rig-cat-482.
      *                              *---------------------------------*
      *                              * Se tipo riga : Commento         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-484.
      *                              *---------------------------------*
      *                              * Se tipo riga : Addebito         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore contenuto in record  *
      *                                  * [ocr]                       *
      *                                  *-----------------------------*
           move      rf-ocr-imp-rig       to   w-rig-imp-rig (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-486.
      *                              *---------------------------------*
      *                              * Se altro tipo riga              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione in base alla *
      *                                  * quantita' effettivamente    *
      *                                  * fatturata                   *
      *                                  *-----------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-500.
      *                          *-------------------------------------*
      *                          * Codice iva                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se da eseguire             *
      *                              *---------------------------------*
           if        w-prs-trt-aic        not  = 01
                     go to car-rig-cat-550.
      *                              *---------------------------------*
      *                              * Test se da trattare             *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-550.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del con-   *
      *                              * fronto tra assoggettamento iva  *
      *                              * del documento e codice iva in   *
      *                              * riga                            *
      *                              *---------------------------------*
           move      rf-ocr-cod-iva       to   w-edt-iva-cod          .
      *
           if       (w-tes-ass-iva (1)    =    zero) and
                    (w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7   )
                     go to car-rig-cat-510
           else if   w-tes-ass-iva (1)    >    zero
                     go to car-rig-cat-520
           else      go to car-rig-cat-550.
       car-rig-cat-510.
      *                              *---------------------------------*
      *                              * Se documento soggetto e codice  *
      *                              * iva in riga esente              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Deviazione in funzione del  *
      *                                  * tipo riga                   *
      *                                  *-----------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-512
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-514
           else      go to car-rig-cat-516.
       car-rig-cat-512.
      *                                  *-----------------------------*
      *                                  * Tipo riga : Prodotto di     *
      *                                  * vendita                     *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Lettura archivio [dcp]  *
      *                                      *-------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-cli (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-cli-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                                      *-------------------------*
      *                                      * Bufferizzazione codice  *
      *                                      * iva                     *
      *                                      *-------------------------*
           move      w-let-dcp-pdx-coi    to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-514.
      *                                  *-----------------------------*
      *                                  * Tipo riga : Addebito        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Lettura archivio [zac]  *
      *                                      *-------------------------*
           move      01                   to   w-let-arc-zac-tip      .
           move      w-rig-tip-rig-cac (1)
                                          to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
      *                                      *-------------------------*
      *                                      * Bufferizzazione codice  *
      *                                      * iva                     *
      *                                      *-------------------------*
           move      w-let-arc-zac-civ    to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-516.
      *                              *---------------------------------*
      *                              * Tipo riga : Altra               *
      *                              *---------------------------------*
           move      zero                 to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-520.
      *                              *---------------------------------*
      *                              * Se documento esente e codice    *
      *                              * iva in riga soggetto            *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Forzatura esenzione in co-  *
      *                                  * dice iva in riga            *
      *                                  *-----------------------------*
           move      w-tes-ass-iva (1)    to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-550.
      *                      *-----------------------------------------*
      *                      * Fine trattamento valori contenuti o de- *
      *                      * dotti da buffer righe ordine            *
      *                      *-----------------------------------------*
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Valori contenuti indirettamente in re-  *
      *                      * cord [ocr]                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di estensione                   *
      *                              *---------------------------------*
           if        w-rig-des-ext (1)    =    0
                     go to car-rig-cat-610
           else if   w-rig-des-ext (1)    =    1
                     go to car-rig-cat-620
           else if   w-rig-des-ext (1)    =    2
                     go to car-rig-cat-630.
       car-rig-cat-610.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : nessuna *
      *                              * azione in quanto la descrizione *
      *                              * e' gia' stata bufferizzata      *
      *                              *---------------------------------*
           go to     car-rig-cat-640.
       car-rig-cat-620.
      *                              *---------------------------------*
      *                              * Se estensione nel file [ocx]    *
      *                              *---------------------------------*
           move      w-gen-prt-orc        to   w-let-arc-bix-prt      .
           move      w-brb-num-prg
                    (w-crc-wrk-ctr)       to   w-let-arc-bix-prg      .
           move      11                   to   w-let-arc-bix-trc      .
           perform   let-arc-bix-000      thru let-arc-bix-999        .
           move      w-let-arc-bix-des    to   w-rig-des-rig (1)      .
           go to     car-rig-cat-640.
       car-rig-cat-630.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx]    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-cli (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-cli-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-640.
       car-rig-cat-640.
      *                          *-------------------------------------*
      *                          * Anagrafica titoli esenzione         *
      *                          *-------------------------------------*
           move      w-rig-coi-rig (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-rig-coi-rig-des (1)  .
      *                          *-------------------------------------*
      *                          * Anagrafica piano dei conti          *
      *                          *-------------------------------------*
           move      w-rig-ctv-rig (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-rig-ctv-rig-des (1)  .
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
       car-rig-cat-750.
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
           call      "pgm/fat/prg/obj/pfat3002"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-prg (1)      .
           move      zero                 to   w-rig-cli-pls (1)      .
           move      spaces               to   w-rig-dpc-pls (1)      .
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
           move      zero                 to   w-rig-prz-acc (1)      .
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
           move      spaces               to   w-rig-bcc-tip (1)      .
           move      zero                 to   w-rig-bcc-dat (1)      .
           move      zero                 to   w-rig-bcc-num (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-flg-puq (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento records [oct] e [ocr] per quanto riguarda i *
      *    * dati relativi alla fattura differita durante la fase di   *
      *    * Inserimento di un nuovo record [fir]                      *
      *    *-----------------------------------------------------------*
       wrt-rec-orc-000.
       wrt-rec-orc-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento records [oct] e [ocr] per quanto riguarda i *
      *    * dati relativi alla fattura differita durante la fase di   *
      *    * Modifica di un record [fir]                               *
      *    *-----------------------------------------------------------*
       rew-rec-orc-000.
       rew-rec-orc-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento records [oct] e [ocr] per quanto riguarda i *
      *    * dati relativi alla fattura differita durante la fase di   *
      *    * Cancellazione di un record [fir]                          *
      *    *-----------------------------------------------------------*
       del-rec-orc-000.
       del-rec-orc-999.
           exit.

      *    *===========================================================*
      *    * Routine di decomposizione tipo riga                       *
      *    *                                                           *
      *    * Input  : rf-ocr-tip-rig                                   *
      *    *                                                           *
      *    * Output : w-rig-tip-rig (1) e relativi valori di w-rig con-*
      *    *          nessi                                            *
      *    *-----------------------------------------------------------*
       dec-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo di lavoro ridefinito        *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to    w-dec-tip-rig-str     .
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
      *    * Find su archivio [oct]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-oct-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "porc3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to  fnd-arc-oct-999.
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
           move      "pgm/orc/prg/obj/porc3010"
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
           move      "tmo-orc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-alf                to   w-fnd-arc-oct-tmo      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-dat                to   w-fnd-arc-oct-dat      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-num                to   w-fnd-arc-oct-num      .
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-oct-tmo    to   w-acc-cod-tmo          .
           move      w-fnd-arc-oct-dat    to   w-acc-dat-doc          .
           move      w-fnd-arc-oct-num    to   w-acc-num-doc          .
       fnd-arc-oct-400.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
           move      w-let-arc-zoc-des    to   w-acc-cod-tmo-des      .
      *                  *---------------------------------------------*
      *                  * Se codice errato : uscita con errore        *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : uscita con errore             *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmo        =    spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-ord    to   w-acc-cod-tmo-ord      .
           move      w-let-arc-zoc-sgl    to   w-acc-cod-tmo-sgl      .
           move      w-let-arc-zoc-tvd    to   w-acc-cod-tmo-tvd      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmo        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
       fnd-arc-oct-410.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmo-tvd    =    spaces
                     go to fnd-arc-oct-411
           else if   w-acc-cod-tmo-tvd    =    "S"
                     go to fnd-arc-oct-412
           else if   w-acc-cod-tmo-tvd    =    "D"
                     go to fnd-arc-oct-413
           else if   w-acc-cod-tmo-tvd    =    "X"
                     go to fnd-arc-oct-414.
       fnd-arc-oct-411.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-420.
       fnd-arc-oct-412.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       not  = 1
                     go to fnd-arc-oct-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-420.
       fnd-arc-oct-413.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       =    1
                     go to fnd-arc-oct-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-420.
       fnd-arc-oct-414.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to fnd-arc-oct-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-420.
       fnd-arc-oct-418.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita con errore                         *
      *                  *---------------------------------------------*
           go to     fnd-arc-oct-900.
       fnd-arc-oct-420.
       fnd-arc-oct-440.
       fnd-arc-oct-460.
       fnd-arc-oct-600.
      *              *-------------------------------------------------*
      *              * Completamento visualizzazione campi di accet-   *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *              *-------------------------------------------------*
      *              * Uscita con successo                             *
      *              *-------------------------------------------------*
           go to     fnd-arc-oct-999.
       fnd-arc-oct-900.
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
           move      "#"                  to   w-fnd-arc-oct-sel      .
       fnd-arc-oct-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zoc]                         *
      *    *-----------------------------------------------------------*
       let-arc-zoc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zoc-cod    =    spaces
                     go to let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOC"             to   f-key                  .
           move      w-let-arc-zoc-cod    to   rf-zoc-cod-toc         .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zoc-400.
       let-arc-zoc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zoc-des-toc       to   w-let-arc-zoc-des      .
           move      rf-zoc-org-doc       to   w-let-arc-zoc-ord      .
           move      rf-zoc-sgl-num       to   w-let-arc-zoc-sgl      .
           move      rf-zoc-vld-dpz       to   w-let-arc-zoc-tvd      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zoc-999.
       let-arc-zoc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zoc-flg      .
           move      all   "."            to   w-let-arc-zoc-des      .
           go to     let-arc-zoc-600.
       let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-des      .
       let-arc-zoc-600.
           move      zero                 to   w-let-arc-zoc-ord      .
           move      spaces               to   w-let-arc-zoc-sgl      .
           move      spaces               to   w-let-arc-zoc-tvd      .
       let-arc-zoc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-prt-iva       to   w-let-arc-cli-piv      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-stc      .
           move      rf-cli-cod-iva       to   w-let-arc-cli-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-520.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-520.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-piv      .
           move      zero                 to   w-let-arc-cli-stc      .
           move      zero                 to   w-let-arc-cli-ass      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici a zero o spazi                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cli    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
           move      rf-dcc-cod-vlt       to   w-let-arc-dcc-vlt      .
           move      rf-dcc-cod-lng       to   w-let-arc-dcc-lng      .
           move      rf-dcc-tas-ivc       to   w-let-arc-dcc-tai      .
           move      rf-dcc-fco-blo       to   w-let-arc-dcc-blo      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all   "."            to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-520.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-520.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      spaces               to   w-let-arc-dcc-vlt      .
           move      spaces               to   w-let-arc-dcc-lng      .
           move      zero                 to   w-let-arc-dcc-tai      .
           move      spaces               to   w-let-arc-dcc-blo      .
       let-arc-dcc-999.
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
      *    * Routine di lettura archivio [zvf]                         *
      *    *-----------------------------------------------------------*
       let-arc-zvf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zvf-num    =    zero or
                     w-let-arc-zvf-cod    =    spaces
                     go to let-arc-zvf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-let-arc-zvf-num    to   rf-zvf-num-def         .
           move      w-let-arc-zvf-cod    to   rf-zvf-cod-def         .
           move      "I  "                to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zvf-400.
       let-arc-zvf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zvf-des-stp       to   w-let-arc-zvf-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zvf-999.
       let-arc-zvf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zvf-flg      .
           move      all   "."            to   w-let-arc-zvf-des      .
           go to     let-arc-zvf-999.
       let-arc-zvf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvf-des      .
       let-arc-zvf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zls]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/larczls0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [zcs]                         *
      *    *-----------------------------------------------------------*
       let-arc-zcs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zcs-cod    =    zero
                     go to let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      w-let-arc-zcs-tip    to   rf-zcs-tip-csc         .
           move      w-let-arc-zcs-cod    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcs-400.
       let-arc-zcs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcs-des-csc       to   w-let-arc-zcs-des      .
           move      rf-zcs-per-sco (1)   to   w-let-arc-zcs-per (1)  .
           move      rf-zcs-per-sco (2)   to   w-let-arc-zcs-per (2)  .
           move      rf-zcs-per-sco (3)   to   w-let-arc-zcs-per (3)  .
           move      rf-zcs-per-sco (4)   to   w-let-arc-zcs-per (4)  .
           move      rf-zcs-per-sco (5)   to   w-let-arc-zcs-per (5)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcs-999.
       let-arc-zcs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcs-flg      .
           move      all   "."            to   w-let-arc-zcs-des      .
           go to     let-arc-zcs-520.
       let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-des      .
       let-arc-zcs-520.
           move      zero                 to   w-let-arc-zcs-per (1)  .
           move      zero                 to   w-let-arc-zcs-per (2)  .
           move      zero                 to   w-let-arc-zcs-per (3)  .
           move      zero                 to   w-let-arc-zcs-per (4)  .
           move      zero                 to   w-let-arc-zcs-per (5)  .
       let-arc-zcs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [age]                         *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE"             to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
           move      rf-age-sup-age       to   w-let-arc-age-spa      .
           move      rf-age-cat-pvg       to   w-let-arc-age-cpv      .
           move      rf-age-per-pvg (1)   to   w-let-arc-age-ppv (1)  .
           move      rf-age-per-pvg (2)   to   w-let-arc-age-ppv (2)  .
           move      rf-age-per-pvg (3)   to   w-let-arc-age-ppv (3)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-rag      .
           go to     let-arc-age-600.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-rag      .
       let-arc-age-600.
           move      zero                 to   w-let-arc-age-spa      .
           move      zero                 to   w-let-arc-age-cpv      .
           move      zero                 to   w-let-arc-age-ppv (1)  .
           move      zero                 to   w-let-arc-age-ppv (2)  .
           move      zero                 to   w-let-arc-age-ppv (3)  .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zfp]                         *
      *    *-----------------------------------------------------------*
       let-arc-zfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-zfp-cod    =    zero
                     go to let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      w-let-arc-zfp-cod    to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfp-400.
       let-arc-zfp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfp-des-fop       to   w-let-arc-zfp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfp-999.
       let-arc-zfp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfp-flg      .
           move      all   "."            to   w-let-arc-zfp-des      .
           go to     let-arc-zfp-999.
       let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-des      .
       let-arc-zfp-999.
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
      *    * Routine di lettura archivio [zin]                         *
      *    *-----------------------------------------------------------*
       let-arc-zin-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zin-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zin-cod    =    spaces
                     go to let-arc-zin-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-let-arc-zin-cod    to   rf-zin-cod-spi         .
           move      w-let-arc-zin-tpg    to   rf-zin-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zin-400.
       let-arc-zin-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zin-des-spi       to   w-let-arc-zin-des      .
           move      rf-zin-civ-spi       to   w-let-arc-zin-coi      .
           move      rf-zin-ccp-spi       to   w-let-arc-zin-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zin-999.
       let-arc-zin-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zin-flg      .
           move      all   "."            to   w-let-arc-zin-des      .
           go to     let-arc-zin-520.
       let-arc-zin-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zin-des      .
       let-arc-zin-520.
           move      zero                 to   w-let-arc-zin-coi      .
           move      zero                 to   w-let-arc-zin-ccp      .
       let-arc-zin-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zbo]                         *
      *    *-----------------------------------------------------------*
       let-arc-zbo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbo-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zbo-cod    =    spaces
                     go to let-arc-zbo-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-let-arc-zbo-cod    to   rf-zbo-cod-spb         .
           move      w-let-arc-zbo-tpg    to   rf-zbo-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zbo-400.
       let-arc-zbo-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbo-des-spb       to   w-let-arc-zbo-des      .
           move      rf-zbo-civ-spb       to   w-let-arc-zbo-coi      .
           move      rf-zbo-ccp-spb       to   w-let-arc-zbo-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zbo-999.
       let-arc-zbo-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zbo-flg      .
           move      all   "."            to   w-let-arc-zbo-des      .
           go to     let-arc-zbo-520.
       let-arc-zbo-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbo-des      .
       let-arc-zbo-520.
           move      zero                 to   w-let-arc-zbo-coi      .
           move      zero                 to   w-let-arc-zbo-ccp      .
       let-arc-zbo-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [ocx]                         *
      *    *-----------------------------------------------------------*
       let-arc-bix-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bix-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-bix-prt    =    zero   or
                     w-let-arc-bix-prg    =    zero   or
                     w-let-arc-bix-trc    =    zero
                     go to let-arc-bix-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-bix-prt    to   rf-ocx-num-prt         .
           move      w-let-arc-bix-prg    to   rf-ocx-num-prg         .
           move      w-let-arc-bix-trc    to   rf-ocx-tip-rec         .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-bix-400.
       let-arc-bix-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ocx-des-400       to   w-let-arc-bix-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-bix-999.
       let-arc-bix-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-bix-flg      .
           move      spaces               to   w-let-arc-bix-des      .
           move      all   "."            to   w-let-arc-bix-drg (1)  .
           go to     let-arc-bix-999.
       let-arc-bix-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bix-des      .
       let-arc-bix-999.
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
      *    * Determinazione se documento gia' esistente oppure no      *
      *    *-----------------------------------------------------------*
       det-doc-ges-000.
      *              *-------------------------------------------------*
      *              * Tipo movimento trovato per il caso che la de-   *
      *              * terminazione dia esito pari a 'X' : a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-tmt      .
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-acc-dat-doc        to   rf-oct-dat-doc         .
           move      w-acc-cod-dpz        to   rf-oct-cod-dpz         .
           move      w-acc-num-doc        to   rf-oct-num-doc         .
           move      spaces               to   rf-oct-tmo-orc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a documento non esistente     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-900.
       det-doc-ges-100.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a determinazione finale           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Max su [oct], se non superato : a determinazio- *
      *              * ne finale                                       *
      *              *-------------------------------------------------*
           if        rf-oct-dat-doc       not  = w-acc-dat-doc or
                     rf-oct-cod-dpz       not  = w-acc-cod-dpz or
                     rf-oct-num-doc       not  = w-acc-num-doc
                     go to det-doc-ges-800.
       det-doc-ges-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo movimento letto   *
      *              *-------------------------------------------------*
           if        rf-oct-tmo-orc       =    w-acc-cod-tmo
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
           if        rf-oct-sgl-num       not  = w-acc-cod-tmo-sgl
                     go to det-doc-ges-100.
      *                  *---------------------------------------------*
      *                  * Se invece la sigla numerazione e' la stessa *
      *                  * si memorizza il codice tipo movimento letto *
      *                  * per il caso in cui la determinazione doves- *
      *                  * se dare esito pari a 'X'                    *
      *                  *---------------------------------------------*
           move      rf-oct-tmo-orc       to   w-det-doc-ges-tmt      .
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
      *    * Determinazione valori testata indotti da record [dcc]     *
      *    * principale                                                *
      *    *-----------------------------------------------------------*
       det-vlt-dcc-000.
      *              *-------------------------------------------------*
      *              * Si/No gestione legame valutario ed eventuale    *
      *              * momento di applicazione cambio                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-005.
      *                  *---------------------------------------------*
      *                  * Se No gestione legame valutario : oltre     *
      *                  *---------------------------------------------*
           if        w-prs-ges-lvl        =    00
                     go to det-vlt-dcc-005.
      *                  *---------------------------------------------*
      *                  * Se no rapporti con legame valutario con il  *
      *                  * cliente : oltre                             *
      *                  *---------------------------------------------*
           if        rf-dcc-snx-rlv        not  = "S"
                     go to det-vlt-dcc-005.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore contenuto   *
      *                  * nel record [dcc]                            *
      *                  *---------------------------------------------*
           if        rf-dcc-mom-alv       =    01
                     go to det-vlt-dcc-001
           else if   rf-dcc-mom-alv       =    11
                     go to det-vlt-dcc-002
           else if   rf-dcc-mom-alv       =    21
                     go to det-vlt-dcc-003
           else      go to det-vlt-dcc-001.
       det-vlt-dcc-001.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : Standard           *
      *                  *---------------------------------------------*
           if        w-prs-ges-lvl        =    11
                     move  11             to   w-tes-mac-lvl (1)
           else if   w-prs-ges-lvl        =    21
                     move  21             to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-005.
       det-vlt-dcc-002.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in ordine          *
      *                  *---------------------------------------------*
           move      11                   to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-005.
       det-vlt-dcc-003.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in fattura         *
      *                  *---------------------------------------------*
           move      21                   to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-005.
       det-vlt-dcc-005.
      *              *-------------------------------------------------*
      *              * Tipo esposizione prezzi e sconti in riga docu-  *
      *              * mento                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-010.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore contenuto   *
      *                  * nel record [dcc]                            *
      *                  *---------------------------------------------*
           if        rf-dcc-epz-pes       =    01
                     go to det-vlt-dcc-006
           else if   rf-dcc-epz-pes       =    11
                     go to det-vlt-dcc-007
           else if   rf-dcc-epz-pes       =    21
                     go to det-vlt-dcc-008
           else      go to det-vlt-dcc-006.
       det-vlt-dcc-006.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione : Standard              *
      *                  *---------------------------------------------*
           if        w-prs-epz-pes        =    11
                     move  11             to   w-tes-epz-pes (1)
           else if   w-prs-epz-pes        =    21
                     move  21             to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-007.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione: Prezzo e sconti        *
      *                  *---------------------------------------------*
           move      11                   to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-008.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione: Prezzo netto           *
      *                  *---------------------------------------------*
           move      21                   to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-010.
      *              *-------------------------------------------------*
      *              * Inoltro documenti                               *
      *              *-------------------------------------------------*
           move      rf-dcc-inl-dcm       to   w-tes-inl-dcm (1)      .
       det-vlt-dcc-015.
      *              *-------------------------------------------------*
      *              * Codice e descrizione nostra banca per bonifico  *
      *              * associati al cliente                            *
      *              *-------------------------------------------------*
           move      rf-dcc-nos-ban       to   w-tes-nsb-aac (1)      .
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nsb-aac (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nsb-aac-des (1)  .
       det-vlt-dcc-020.
      *              *-------------------------------------------------*
      *              * Codice e descrizione nostro c/c postale per pa- *
      *              * gamento associati al cliente                    *
      *              *-------------------------------------------------*
           move      rf-dcc-nos-ccp       to   w-tes-ncp-aac (1)      .
           move      03                   to   w-let-arc-cbp-tip      .
           move      w-tes-ncp-aac (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-ncp-aac-des (1)  .
       det-vlt-dcc-030.
      *              *-------------------------------------------------*
      *              * Codice e descrizione ABI del cliente            *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-abi-cli (1)      .
           move      w-tes-abi-cli (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-cli-den (1)  .
       det-vlt-dcc-040.
      *              *-------------------------------------------------*
      *              * Codice e descrizione CAB del cliente            *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cab-cli (1)      .
           move      w-tes-abi-cli (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-cli (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-cli-den (1)  .
       det-vlt-dcc-050.
      *              *-------------------------------------------------*
      *              * C/c appoggio del cliente                        *
      *              *-------------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-cca-cli (1)      .
       det-vlt-dcc-060.
      *              *-------------------------------------------------*
      *              * Codice e descrizione agente associato al clien- *
      *              * te                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Gruppo   *
      *                  * d'Acquisto e codice agente esistente : ri-  *
      *                  * mane quanto e' stato determinato per il     *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"    and
                     w-tes-tip-frn (1)    =    21     and
                     w-tes-age-aac (1)    not  = zero
                     go to det-vlt-dcc-070.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-age       to   w-tes-age-aac (1)      .
       det-vlt-dcc-070.
      *              *-------------------------------------------------*
      *              * Altri valori                                    *
      *              *-------------------------------------------------*
           move      rf-dcc-tip-esm       to   w-tes-tip-esm (1)      .
           move      rf-dcc-ggg-alt       to   w-tes-ggg-alt (1)      .
           move      rf-dcc-mmm-e01       to   w-tes-mmm-e01 (1)      .
           move      rf-dcc-mmm-e02       to   w-tes-mmm-e02 (1)      .
       det-vlt-dcc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione funzionamento spese in fattura             *
      *    *-----------------------------------------------------------*
       det-fun-spe-000.
      *              *-------------------------------------------------*
      *              * Se tipo determinazione per cliente per fattura- *
      *              * zione e tipo fornitura tramite Leasing : rimane *
      *              * quanto e' stato determinato per il Cliente Com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           if        w-det-fun-spe-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-fun-spe-999.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella personalizzazioni *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-fun-spe-ctr      .
       det-fun-spe-100.
           add       1                    to   w-det-fun-spe-ctr      .
           if        w-det-fun-spe-ctr    >    w-prs-spe-fat-nst
                     go to det-fun-spe-999.
      *              *-------------------------------------------------*
      *              * Preparazione indice per tabella                 *
      *              *-------------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-det-fun-spe-ctr)   to   w-det-fun-spe-inx      .
      *              *-------------------------------------------------*
      *              * Si/No addebito                                  *
      *              *-------------------------------------------------*
           if        rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    zero or
                     rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    01
                     move  0              to   w-pie-spe-snx
                                              (1, w-det-fun-spe-inx)
           else      move  1              to   w-pie-spe-snx
                                              (1, w-det-fun-spe-inx)  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              * derivato dalle personalizzazioni                *
      *              *-------------------------------------------------*
           if        w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    01
                     go to det-fun-spe-110
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    02
                     go to det-fun-spe-120
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    03
                     go to det-fun-spe-130
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    04
                     go to det-fun-spe-140
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    05
                     go to det-fun-spe-150.
       det-fun-spe-110.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : % fissa                    *
      *              *-------------------------------------------------*
           move      1                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-per
                    (w-det-fun-spe-ctr)   to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-120.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : % variabile                *
      *              *-------------------------------------------------*
           move      2                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      rf-dcc-per-spe
                    (w-det-fun-spe-inx)   to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-130.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : importo fisso              *
      *              *-------------------------------------------------*
           move      3                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-imp
                    (w-det-fun-spe-ctr)   to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-140.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : importo variabile          *
      *              *-------------------------------------------------*
           move      4                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      rf-dcc-imp-spe
                    (w-det-fun-spe-inx)   to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-150.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : secondo [dcc]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo funziona-   *
      *                  * mento espresso in [dcc]                     *
      *                  *---------------------------------------------*
           if        rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    03
                     go to det-fun-spe-151
           else if   rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    04
                     go to det-fun-spe-152.
       det-fun-spe-151.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento espresso in [dcc] : a %  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Come per tipo funzionamento espresso in *
      *                      * personalizzazioni : % variabile         *
      *                      *-----------------------------------------*
           go to     det-fun-spe-120.
       det-fun-spe-152.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento espresso in [dcc] : ad   *
      *                  * importo                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Come per tipo funzionamento espresso in *
      *                      * personalizzazioni : importo variabile   *
      *                      *-----------------------------------------*
           go to     det-fun-spe-140.
       det-fun-spe-200.
      *              *-------------------------------------------------*
      *              * Codice iva della spesa                          *
      *              *-------------------------------------------------*
           move      w-prs-spe-fat-civ
                    (w-det-fun-spe-ctr)   to   w-pie-spe-civ
                                              (1, w-det-fun-spe-inx)  .
      *              *-------------------------------------------------*
      *              * Contropartita della spesa                       *
      *              *-------------------------------------------------*
           move      w-prs-spe-fat-ccp
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ccp
                                              (1, w-det-fun-spe-inx)  .
       det-fun-spe-300.
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella personalizzazioni  *
      *              *-------------------------------------------------*
           go to     det-fun-spe-100.
       det-fun-spe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori testata indotti da record [dcc]     *
      *    * relativo alla dipendenza del cliente                      *
      *    *-----------------------------------------------------------*
       det-vlt-dcd-000.
      *              *-------------------------------------------------*
      *              * Se dipendenza a spaces : uscita                 *
      *              *-------------------------------------------------*
           if        w-det-vlt-dcd-dpz    =    spaces
                     go to det-vlt-dcd-999.
      *              *-------------------------------------------------*
      *              * Codice e descrizione ABI dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-abi-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-dpz-den (1)  .
      *              *-------------------------------------------------*
      *              * Codice e descrizione CAB dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cab-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-dpz (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-dpz-den (1)  .
      *              *-------------------------------------------------*
      *              * C/c appoggio della dipendenza                   *
      *              *-------------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-cca-dpz (1)      .
      *              *-------------------------------------------------*
      *              * Codice e descrizione agente associato alla      *
      *              * dipendenza                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Gruppo   *
      *                  * d'Acquisto e codice agente esistente : ri-  *
      *                  * mane quanto e' stato determinato per il     *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcd-tdt    =    "F"    and
                     w-tes-tip-frn (1)    =    21     and
                     w-tes-age-aad (1)    not  = zero
                     go to det-vlt-dcd-999.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-age       to   w-tes-age-aad (1)      .
       det-vlt-dcd-999.
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
                     move  w-tes-cod-cli (1)
                                          to   d-pvg-age-cod-cli
           else      move  w-tes-cli-plf (1)
                                          to   d-pvg-age-cod-cli      .
           move      w-tes-cpv-aac (1)    to   d-pvg-age-cpv-aac      .
           move      w-tes-ppv-aac (1, 1) to   d-pvg-age-ppv-aac (1)  .
           move      w-tes-ppv-aac (1, 2) to   d-pvg-age-ppv-aac (2)  .
           move      w-tes-ppv-aac (1, 3) to   d-pvg-age-ppv-aac (3)  .
           move      w-tes-sgl-vpf (1)    to   d-pvg-age-sgl-vpp      .
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
      *                      * valuta per fatturazione                 *
      *                      *-----------------------------------------*
           move      w-rig-prz-lrs (1)    to   d-pvg-age-prz-lrs      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-tes-sgl-vpf (1)
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
      *                          * valuta per fatturazione             *
      *                          *-------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-lrs      .
       det-ppv-rig-220.
      *                      *-----------------------------------------*
      *                      * Prezzo netto standard espresso nella    *
      *                      * valuta per fatturazione                 *
      *                      *-----------------------------------------*
           move      w-rig-prz-nts (1)    to   d-pvg-age-prz-nts      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-tes-sgl-vpf (1)
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
      *                          * valuta per fatturazione             *
      *                          *-------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-nts      .
       det-ppv-rig-240.
      *                      *-----------------------------------------*
      *                      * Prezzo netto effettivo, gia' espresso   *
      *                      * nella valuta per fatturazione           *
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
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

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
      *    * Subroutines per l'accettazione tipo movimento ordini cl.  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acs"                   .

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

