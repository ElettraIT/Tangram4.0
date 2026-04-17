       Identification Division.
       Program-Id.                                 porf300e           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orf                 *
      *                                Settore:    mov                 *
      *                                   Fase:    orf300e             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Duplicazione righe da altro ordine          *
      *                                                                *
      *                    Richiamata in orf300 da funzione 'PF3'      *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-dup-rdo-tip-ope = "OP"                       *
      *                 l-dup-rdo-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-dup-rdo-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-dup-rdo-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-dup-rdo-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "RO" - Accettazione dati identificativi ordine                 *
      *                                                                *
      *        Input  : l-dup-rdo-tip-ope = "RO"                       *
      *                 l-dup-rdo-tmo-orf = codice tipo movimento or-  *
      *                                     dine da proporre come de-  *
      *                                     fault                      *
      *                                                                *
      *        Output : l-dup-rdo-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "SC" - Saldaconto per selezione righe ordine                   *
      *                                                                *
      *        Input  : l-dup-rdo-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-dup-rdo-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe ordine selezionate in catena          *
      *                                                                *
      *        Input  : l-dup-rdo-tip-ope = "CC"                       *
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
      *    * Record files area 'orf'                                   *
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
      *    * Record files area 'dcf'                                   *
      *    *-----------------------------------------------------------*
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

      *    *===========================================================*
      *    * Work-area generica per il programma                       *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-gen-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine in corso di trattamento      *
      *        *-------------------------------------------------------*
           05  w-gen-prt-orf              pic  9(11)                  .

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
      *        * Tipo movimento ordini                                 *
      *        *-------------------------------------------------------*
           05  w-acc-tmo-orf              pic  x(05)                  .
           05  w-acc-tmo-orf-des          pic  x(30)                  .
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

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-val-acc.
               10  filler   occurs 200    pic  x(01)                  .
           05  w-sav-qta-dup              pic s9(06)v9(03)            .
           05  w-sav-snx-aoc              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ricalcolo da effettuare               *
      *        *-------------------------------------------------------*
           05  w-exp-tip-rcl.
               10  w-exp-tip-rcl-num      pic  9(02)       value 2    .
               10  w-exp-tip-rcl-lun      pic  9(02)       value 15   .
               10  w-exp-tip-rcl-tbl.
                   15  filler             pic  x(15) value
                            "[ ] Prezzi     "                         .
                   15  filler             pic  x(15) value
                            "[ ] Sconti     "                         .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe ordine da duplicare   *
      *    *-----------------------------------------------------------*
       01  w-bro.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-bro-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-bro-max-ele              pic  9(05) value 500        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-bro-tbl.
               10  w-bro-sng-ele occurs 999.
                   15  w-bro-num-prg      pic  9(05)       comp-3     .
                   15  w-bro-tip-rig      pic  x(01)                  .
                   15  w-bro-des-rig      pic  x(40)                  .
                   15  w-bro-dec-qta      pic  9(01)                  .
                   15  w-bro-qta-orf      pic s9(10)v9(03) comp-3     .
                   15  w-bro-qta-dup      pic s9(10)v9(03) comp-3     .
                   15  w-bro-snx-aoc      pic  x(01)                  .

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
      *    * Work-area per routine buf-rig-orf-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-orf.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orf-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine                              *
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
      *    * Work-area per routine car-rig-cat-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-car-rig-cat.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-car-rig-cat-ctr          pic  9(05)                  .

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
               10  w-let-arc-yof-dtr      pic  x(05)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
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
               10  w-let-arc-aaq-pfz      pic  9(07)                  .
               10  w-let-arc-aaq-apv      pic  9(07)                  .
               10  w-let-arc-aaq-uda      pic  9(07)                  .
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
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-cod      pic  9(07)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .

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
      *            * - S : Si, documento del tipo movimento cercato    *
      *            *       e con data e numero cercato esistente in    *
      *            *       archivio                                    *
      *            * - N : No, documento del tipo movimento cercato    *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio                                 *
      *            * - X : No, documento del tipo movimento cercato    *
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
      *            *---------------------------------------------------*
      *            * Protocollo movimento trovato                      *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-prt      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det flag di estensione alla descrizione      *
      *        *-------------------------------------------------------*
           05  w-det-des-ext.
               10  w-det-des-ext-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det prezzo di acquisto                       *
      *        *-------------------------------------------------------*
           05  w-det-prz-acq.
               10  w-det-prz-qta-pre      pic s9(06)v9(03)            .
               10  w-det-prz-qta-att      pic s9(06)v9(03)            .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per determinazione prezzo netto                 *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dimpacq0.dtl"                   .

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
      *    * Link-area per accettazione tipo movimento ordini          *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/acdeyof0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione prezzi e sconti  *
      *    * di acquisto                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dprzacq0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per funzione di duplicazione righe  *
      *    * da altro ordine                                           *
      *    *-----------------------------------------------------------*
       01  l-dup-rdo.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-dup-rdo-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-dup-rdo-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-dup-rdo-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento ordine              *
      *        *-------------------------------------------------------*
           05  l-dup-rdo-tmo-orf          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento ordine                            *
      *        *-------------------------------------------------------*
           05  l-dup-rdo-flg-orf          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area libera per espansioni future                     *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(99)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie porf3000       *
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
           copy      "pgm/orf/prg/cpy/porf3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-dup-rdo
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
           move      spaces               to   l-dup-rdo-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi ordine     *
      *                  *---------------------------------------------*
           if        l-dup-rdo-tip-ope    =    "RO"
                     perform acc-dti-orf-000
                                          thru acc-dti-orf-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per selezione righe ordine       *
      *                  *---------------------------------------------*
           else if   l-dup-rdo-tip-ope    =    "SC"
                     perform sdc-sel-orf-000
                                          thru sdc-sel-orf-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe ordine in catena          *
      *                  *---------------------------------------------*
           else if   l-dup-rdo-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-dup-rdo-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-dup-rdo-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-dup-rdo-tip-ope    =    "C?"
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
           move      l-dup-rdo-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per gli *
      *              * ordini                                          *
      *              *-------------------------------------------------*
           perform   cod-des-yof-opn-000  thru cod-des-yof-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione importo in riga   *
      *              *-------------------------------------------------*
           perform   det-imp-acq-opn-000  thru det-imp-acq-opn-999    .
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
      *              * Close modulo accettazione tipo movimento per    *
      *              * ordini                                          *
      *              *-------------------------------------------------*
           perform   cod-des-yof-cls-000  thru cod-des-yof-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
       exe-fun-cls-200.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione importo in riga  *
      *              *-------------------------------------------------*
           perform   det-imp-acq-cls-000  thru det-imp-acq-cls-999    .
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
                     move  spaces         to   l-dup-rdo-exi-sts
           else      move  "#"            to   l-dup-rdo-exi-sts      .
       tst-cnc-mod-999.
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
           move      34                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "DUPLICAZIONE RIGHE DA ALTRO ORDINE"
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
      *                  * Tipo di ricalcolo                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Ricalcolo:"         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo di ri-  *
      *              * calcolo                                         *
      *              *-------------------------------------------------*
           move      "X"                  to   w-acc-tip-rcl-prz      .
           move      "X"                  to   w-acc-tip-rcl-sco      .
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo movi-   *
      *              * mento ordine                                    *
      *              *-------------------------------------------------*
           move      l-dup-rdo-tmo-orf    to   w-acc-tmo-orf          .
           if        w-acc-tmo-orf        =    spaces
                     go to acc-dti-orf-100.
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
      *                  * Tipo movimento per ordini fornitori         *
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
      *                  * Tipo di ricalcolo                           *
      *                  *---------------------------------------------*
           perform   acc-tip-rcl-000      thru acc-tip-rcl-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orf-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orf-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orf-300.
       acc-dti-orf-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori di iden-  *
      *              * tificazione della ordine                        *
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
       acc-dti-orf-440.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo               *
      *              *-------------------------------------------------*
           move      w-det-doc-ges-prt    to   w-acc-num-prt          .
       acc-dti-orf-500.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe ordine da trattare        *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-buf-rig-orf-prt      .
           perform   buf-rig-orf-000      thru buf-rig-orf-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-buf-rig-orf-flg    =    spaces
                     go to acc-dti-orf-620.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Nessuna riga ordine da duplicare !                
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orf-100.
       acc-dti-orf-620.
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo ordine in corso di     *
      *              * trattamento                                     *
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
           move      "#"                  to   l-dup-rdo-exi-sts      .
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
       acc-tmo-orf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orf        to   l-dup-rdo-tmo-orf      .
       acc-tmo-orf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tmo-orf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : tipo movimento per ordini         *
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
      *                  * Visualizzazione valori                      *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
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
      *                  * Visualizzazione valori                      *
      *                  *---------------------------------------------*
           perform   vis-tmo-orf-000      thru vis-tmo-orf-999        .
           perform   vis-tmo-orf-des-000  thru vis-tmo-orf-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
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
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
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
      *    * Accettazione campo : Tipo ricalcolo da effettuare         *
      *    *-----------------------------------------------------------*
       acc-tip-rcl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-rcl-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-tip-rcl-lun    to   v-car                  .
           move      w-exp-tip-rcl-num    to   v-ldt                  .
           move      "X"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      w-exp-tip-rcl-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-tip-rcl        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-tip-rcl-999.
       acc-tip-rcl-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-tip-rcl          .
       acc-tip-rcl-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-rcl-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-rcl-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tip-rcl-999.
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
      *              * Tipo movimento per orflettazione                *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-tmo-orf-des      .
           move      spaces               to   w-acc-tmo-orf-sgl      .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-num-doc          .
      *              *-------------------------------------------------*
      *              * Tipo ricalcolo da effettuare                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-tip-rcl          .
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
      *              * Normalizzazione comodi di visualizzazione       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-edt-qta-inc-ope      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *              *-------------------------------------------------*
      *              * Azzeramento numero righe nel buffer             *
      *              *-------------------------------------------------*
           move      zero                 to   w-bro-num-ele          .
       buf-rig-orf-100.
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
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orf-500.
       buf-rig-orf-200.
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
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orf-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-ofr-num-prt       not  = w-buf-rig-orf-prt
                     go to buf-rig-orf-500.
       buf-rig-orf-250.
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
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orf-flg      .
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
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-buf-rig-orf-wtp    to   w-bro-tip-rig
                                              (w-bro-num-ele)         .
       buf-rig-orf-300.
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uguale per tutti i tipi riga            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura preliminare record [ofx]    *
      *                          *-------------------------------------*
           move      rf-ofr-num-prt       to   w-let-arc-ofx-prt      .
           move      rf-ofr-num-prg       to   w-let-arc-ofx-prg      .
           move      11                   to   w-let-arc-ofx-trc      .
           perform   let-arc-ofx-000      thru let-arc-ofx-999        .
      *
           if        w-let-arc-ofx-flg    not  = spaces
                     go to buf-rig-orf-320.
      *
           move      w-let-arc-ofx-des    to   w-buf-rig-orf-wde      .
      *
           go to     buf-rig-orf-350.
       buf-rig-orf-320.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del flag di  *
      *                          * estensione alla descrizione         *
      *                          *-------------------------------------*
           if        rf-ofr-des-ext       =    0
                     go to buf-rig-orf-322
           else if   rf-ofr-des-ext       =    1
                     go to buf-rig-orf-324
           else if   rf-ofr-des-ext       =    2
                     go to buf-rig-orf-326
           else if   rf-ofr-des-ext       =    3
                     go to buf-rig-orf-328.
       buf-rig-orf-322.
      *                          *-------------------------------------*
      *                          * Se nessuna estensione : bufferizza- *
      *                          * zione descrizione contenuta record  *
      *                          * [ofr]                               *
      *                          *-------------------------------------*
           move      rf-ofr-des-rig       to   w-buf-rig-orf-wde      .
           go to     buf-rig-orf-350.
       buf-rig-orf-324.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [ofx]        *
      *                          *-------------------------------------*
           go to     buf-rig-orf-350.
       buf-rig-orf-326.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [pdx]        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [dcp]          *
      *                              *---------------------------------*
           move      rf-ofr-num-mag       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-orf-wde      .
           go to     buf-rig-orf-350.
       buf-rig-orf-328.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx] ma *
      *                              * con tipo record 13 o 33 o 43    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [aaf]      *
      *                                  *-----------------------------*
           move      w-rig-tip-mag (1)    to   w-let-arc-aaf-tpm      .
           move      w-rig-num-mag (1)    to   w-let-arc-aaf-cdm      .
           move      w-tes-cod-arc (1)    to   w-let-arc-aaf-fnt      .
           move      w-rig-fda-pif (1)    to   w-let-arc-aaf-fda      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           move      w-let-arc-aaf-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
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
       buf-rig-orf-356.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           move      rf-ofr-dec-qta       to   w-bro-dec-qta
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' in ordine                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si/no trasformazione unita' di misura   *
      *                      *-----------------------------------------*
           if        rf-ofr-snx-tum       =    "P"
                     move  rf-ofr-qta-ord to   w-bro-qta-orf
                                              (w-bro-num-ele)
           else if   rf-ofr-snx-tum       =    "S"
                     move  rf-ofr-qta-fda to   w-bro-qta-orf
                                              (w-bro-num-ele)
           else      move  rf-ofr-qta-fda to   w-bro-qta-orf
                                              (w-bro-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento : zero   *
      *                      *-----------------------------------------*
           if        w-buf-rig-orf-wtp    =    "C" or
                     w-buf-rig-orf-wtp    =    "A"
                     move  zero           to   w-bro-qta-orf
                                              (w-bro-num-ele)         .
       buf-rig-orf-360.
      *                  *---------------------------------------------*
      *                  * Quantita' da duplicare pari a quantita' in  *
      *                  * ordine                                      *
      *                  *---------------------------------------------*
           move      w-bro-qta-orf
                    (w-bro-num-ele)       to   w-bro-qta-dup
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      "S"                  to   w-bro-snx-aoc
                                              (w-bro-num-ele)         .
       buf-rig-orf-400.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [ofr]       *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-200.
       buf-rig-orf-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orf-999.
       buf-rig-orf-999.
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
                     w-buf-rig-orf-wtp    =    "S" or
                     w-buf-rig-orf-wtp    =    "M"  ) and
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
      *    * Saldaconto per selezione righe ordine da duplicare        *
      *    *-----------------------------------------------------------*
       sdc-sel-orf-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione di saldaconto                 *
      *              *-------------------------------------------------*
           perform   acc-fun-sdc-000      thru acc-fun-sdc-999        .
           if        w-sdc-flg-exi        not  = spaces
                     move  "#"            to   l-dup-rdo-exi-sts      .
       sdc-sel-orf-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per selezione righe ordine                     *
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
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Quantita' da duplicare                      *
      *                  *---------------------------------------------*
           perform   acc-qta-dup-000      thru acc-qta-dup-999        .
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
           move      "      |               Descrizione              |  
      -              "Quantita' |  Duplicare |      "
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
           move      "      |----------------------------------------|--
      -              "----------|------------|      "
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
           move      "      |       Descrizione            Codice    |  
      -              "Quantita' |  Duplicare |      "
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
           move      "      |------------------------- --------------|--
      -              "----------|------------|      "
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
           move      "      |     Descrizione            Codice      |  
      -              "Quantita' |  Duplicare |      "
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
           move      "      |--------------------- ------------------|--
      -              "----------|------------|      "
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
           move      "      |    Codice            Descrizione       |  
      -              "Quantita' |  Duplicare |      "
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
           move      "      |-------------- -------------------------|--
      -              "----------|------------|      "
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
           move      "      |      Codice            Descrizione     |  
      -              "Quantita' |  Duplicare |      "
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
           move      "      |------------------ ---------------------|--
      -              "----------|------------|      "
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
           move      "      |       Descrizione            Codice    |  
      -              "Quantita' |  Duplicare |      "
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
           move      "      |------------------------- --------------|--
      -              "----------|------------|      "
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
           move      "      |     Descrizione            Codice      |  
      -              "Quantita' |  Duplicare |      "
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
           move      "      |--------------------- ------------------|--
      -              "----------|------------|      "
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
           move      "      |                                        |  
      -              "          |            |      "
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
           move      "      |                                        |  
      -              "          |            |      "
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
           move      08                   to   v-pos                  .
           move      w-bro-des-rig
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' in ordine                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita' da incolonnare    *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-bro-qta-orf
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
           move      49                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-bro-tip-rig
                    (w-sdc-wrk-rig)       not  = "C" and
                     w-bro-tip-rig
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
           move      67                   to   v-pos                  .
           if        w-bro-snx-aoc
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
      *                          * Quantita' da duplicare              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-bro-dec-qta
                    (w-sdc-wrk-rig)       >    2
                     move  2              to   v-dec
           else      move  w-bro-dec-qta
                          (w-sdc-wrk-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      62                   to   v-pos                  .
           move      w-bro-qta-dup
                    (w-sdc-wrk-rig)       to   v-num                  .
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
      *    * Accettazione campo saldaconto : Quantita' da duplicare    *
      *    *-----------------------------------------------------------*
       acc-qta-dup-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-bro-tip-rig
                    (w-sdc-ctr-rig)       =    "A" or
                     w-bro-tip-rig
                    (w-sdc-ctr-rig)       =    "C"
                     go to acc-qta-dup-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-bro-qta-dup
                    (w-sdc-ctr-rig)       to   w-sav-qta-dup          .
       acc-qta-dup-100.
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
           move      62                   to   v-pos                  .
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
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-bro-qta-dup
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-dup-999.
       acc-qta-dup-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-bro-qta-dup
                                              (w-sdc-ctr-rig)         .
       acc-qta-dup-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo compatibilita' fra segno quantita'*
      *                  * da duplicare e segno quantita' in ordine    *
      *                  *---------------------------------------------*
           if        w-bro-qta-orf
                    (w-sdc-ctr-rig)       >    zero and
                     w-bro-qta-dup
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-dup-100
           else if   w-bro-qta-orf
                    (w-sdc-ctr-rig)       <    zero and
                     w-bro-qta-dup
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-dup-100.
       acc-qta-dup-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-dup-600.
           if        w-bro-qta-dup
                    (w-sdc-ctr-rig)       not  = w-sav-qta-dup
                     move  w-sav-qta-dup  to   w-bro-qta-dup
                                              (w-sdc-ctr-rig)
                     go to acc-qta-dup-100.
       acc-qta-dup-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Slct                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-qta-dup-620.
      *                      *-----------------------------------------*
      *                      * Se valore gia' presente                 *
      *                      *-----------------------------------------*
           if        w-bro-qta-dup
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-dup-610.
      *                          *-------------------------------------*
      *                          * Quantita' da duplicare              *
      *                          *-------------------------------------*
           move      zero                 to   w-bro-qta-dup
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' da duplicare              *
      *                          *-------------------------------------*
           move      w-bro-qta-orf
                    (w-sdc-ctr-rig)       to   w-bro-qta-dup
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-620.
      *                  *---------------------------------------------*
      *                  * Se Return o altri tasti funzione            *
      *                  *---------------------------------------------*
       acc-qta-dup-700.
      *                  *---------------------------------------------*
      *                  * Trattamento altri valori riga               *
      *                  *---------------------------------------------*
           go to     acc-qta-dup-760.
       acc-qta-dup-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' da duplicare                  *
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
           move      62                   to   v-pos                  .
           move      w-bro-qta-dup
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-qta-dup-800.
       acc-qta-dup-999.
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
           if        w-bro-tip-rig
                    (w-sdc-ctr-rig)       not  = "A" and
                     w-bro-tip-rig
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
           move      67                   to   v-pos                  .
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
           move      67                   to   v-pos                  .
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
           move      zero                 to   w-car-rig-cat-ctr      .
       car-rig-cat-100.
           add       1                    to   w-car-rig-cat-ctr      .
           if        w-car-rig-cat-ctr    >    w-bro-num-ele
                     go to car-rig-cat-900.
      *                  *---------------------------------------------*
      *                  * Selezione su righe ordine nel buffer        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-bro-tip-rig
                    (w-car-rig-cat-ctr)   =    "A" or
                     w-bro-tip-rig
                    (w-car-rig-cat-ctr)   =    "C"
                     go to car-rig-cat-110
           else      go to car-rig-cat-120.
       car-rig-cat-110.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           if        w-bro-snx-aoc
                    (w-car-rig-cat-ctr)   not  = "S"
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
           if        w-bro-qta-dup
                    (w-car-rig-cat-ctr)   =    zero
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
                    (w-car-rig-cat-ctr)   to   rf-ofr-num-prg         .
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
      *                      *-----------------------------------------*
      *                      * Numero progressivo riga                 *
      *                      *-----------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
       car-rig-cat-310.
      *                      *-----------------------------------------*
      *                      * Dati relativi ai blocchi : non gestiti  *
      *                      *-----------------------------------------*
       car-rig-cat-320.
      *                      *-----------------------------------------*
      *                      * Tipo riga                               *
      *                      *-----------------------------------------*
           move      rf-ofr-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
       car-rig-cat-330.
      *                      *-----------------------------------------*
      *                      * Tipo magazzino                          *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     move  01             to   w-rig-tip-mag (1)
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     move  03             to   w-rig-tip-mag (1)
           else if   w-rig-tip-rig-tpr (1)
                                          =    "V"
                     move  04             to   w-rig-tip-mag (1)
           else      move  zero           to   w-rig-tip-mag (1)      .
       car-rig-cat-340.
      *                      *-----------------------------------------*
      *                      * Codice numerico magazzino               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da trattare                 *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-350.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      rf-ofr-num-mag       to   w-rig-num-mag (1)      .
       car-rig-cat-350.
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico magazzino           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da trattare                 *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-360.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      rf-ofr-alf-mag       to   w-rig-alf-mag (1)      .
       car-rig-cat-360.
      *                      *-----------------------------------------*
      *                      * Sigla variante : non gestita            *
      *                      *-----------------------------------------*
       car-rig-cat-363.
      *                      *-----------------------------------------*
      *                      * Formato di acquisto                     *
      *                      *-----------------------------------------*
           move      rf-ofr-fda-pif       to   w-rig-fda-pif (1)      .
       car-rig-cat-365.
      *                      *-----------------------------------------*
      *                      * Codice prodotto secondo il fornitore    *
      *                      *                                         *
      *                      * - Se tipo riga 'P':                     *
      *                      *   - Se tipo e codice archivio del docu- *
      *                      *     mento di origine sono gli stessi di *
      *                      *     quelli di testata : si duplica il   *
      *                      *     codice prodotto secondo fornitore   *
      *                      *     contenuto nel documento di origine  *
      *                      *   - Se tipo e codice archivio del docu- *
      *                      *     mento di origine sono diversi da    *
      *                      *     quelli di testata : si ricalcola il *
      *                      *     codice prodotto secondo fornitore   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da trattare                 *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "M" or
                     w-rig-tip-rig-tfu (1)
                                          not  = spaces
                     go to car-rig-cat-370.
      *                          *-------------------------------------*
      *                          * Confronto tipo e codice archivio    *
      *                          * del documento originale e quelli di *
      *                          * testata                             *
      *                          *-------------------------------------*
           if        rf-ofr-tip-arc       =    w-tes-tip-arc (1) and
                     rf-ofr-cod-arc       =    w-tes-cod-arc (1)
                     go to car-rig-cat-366
           else      go to car-rig-cat-367.
       car-rig-cat-366.
      *                          *-------------------------------------*
      *                          * Se uguali a quelli del documento o- *
      *                          * riginale                            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione codice prodotto *
      *                              * secondo fornitore da documento  *
      *                              * originale                       *
      *                              *---------------------------------*
           move      rf-ofr-cop-sfn       to   w-rig-cop-sfn (1)      .
           go to     car-rig-cat-370.
       car-rig-cat-367.
      *                          *-------------------------------------*
      *                          * Se diversi da quelli del documento  *
      *                          * originale                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [aaf]          *
      *                              *---------------------------------*
           move      w-rig-tip-mag (1)    to   w-let-arc-aaf-tpm      .
           move      w-rig-num-mag (1)    to   w-let-arc-aaf-cdm      .
           move      w-tes-cod-arc (1)    to   w-let-arc-aaf-fnt      .
           move      w-rig-fda-pif (1)    to   w-let-arc-aaf-fda      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione codice prodotto *
      *                              * secondo il fornitore            *
      *                              *---------------------------------*
           move      w-let-arc-aaf-csf    to   w-rig-cop-sfn (1)      .
           go to     car-rig-cat-370.
       car-rig-cat-370.
      *                      *-----------------------------------------*
      *                      * Descrizione prodotto originale          *
      *                      *                                         *
      *                      * - Se tipo riga 'A', 'C' o 'PN':         *
      *                      *   si duplica la descrizione contenuta   *
      *                      *   nel documento di origine              *
      *                      *                                         *
      *                      * - Se tipo riga 'P':                     *
      *                      *   - Se tipo e codice archivio e codice  *
      *                      *     lingua del documento di origine so- *
      *                      *     no gli stessi di quelli di testa-   *
      *                      *     ta : si duplica la descrizione con- *
      *                      *     tenuta nel documento di origine     *
      *                      *   - Se tipo e codice archivio e codice  *
      *                      *     lingua del documento di origine so- *
      *                      *     no diversi da quelli di testata :   *
      *                      *     si ricalcola la descrizione del     *
      *                      *     prodotto                            *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A"     or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"     or
                    (w-rig-tip-rig-tpr (1)
                                          =    "P" and
                     w-rig-tip-rig-tfu (1)
                                          =    "N"   )
                     go to car-rig-cat-371
           else if   w-rig-tip-rig-tpr (1)
                                          =    "P" and
                     w-rig-tip-rig-tfu (1)
                                          =    " "
                     go to car-rig-cat-376
           else      go to car-rig-cat-371.
       car-rig-cat-371.
      *                          *-------------------------------------*
      *                          * Se tipo riga 'A', 'C' o 'PN'        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura preliminare file [ofx]  *
      *                              *---------------------------------*
           move      rf-ofr-num-prt       to   w-let-arc-ofx-prt      .
           move      rf-ofr-num-prg       to   w-let-arc-ofx-prg      .
           move      11                   to   w-let-arc-ofx-trc      .
           perform   let-arc-ofx-000      thru let-arc-ofx-999        .
      *
           if        w-let-arc-ofx-flg    not  = spaces
                     go to car-rig-cat-372.
      *
           move      w-let-arc-ofx-des    to   w-rig-des-por (1)      .
      *
           go to     car-rig-cat-380.
       car-rig-cat-372.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del flag *
      *                              * di estensione alla descrizione  *
      *                              *---------------------------------*
           if        rf-ofr-des-ext       =    0
                     go to car-rig-cat-373
           else if   rf-ofr-des-ext       =    1
                     go to car-rig-cat-374
           else if   rf-ofr-des-ext       =    2
                     go to car-rig-cat-375.
       car-rig-cat-373.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : buffe-  *
      *                              * rizzazione descrizione contenu- *
      *                              * ta record [ofr]                 *
      *                              *---------------------------------*
           move      rf-ofr-des-rig       to   w-rig-des-por (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-374.
      *                              *---------------------------------*
      *                              * Se estensione nel file [ofx]    *
      *                              *---------------------------------*
           go to     car-rig-cat-380.
       car-rig-cat-375.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx]    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-mag (1)    to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-376.
      *                          *-------------------------------------*
      *                          * Se tipo riga 'P'                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su tipo e codice archivio  *
      *                              * e su codice lingua              *
      *                              *---------------------------------*
           if        rf-ofr-tip-arc       =    w-tes-tip-arc (1) and
                     rf-ofr-cod-arc       =    w-tes-cod-arc (1) and
                     rf-ofr-cod-lng       =    w-tes-cod-lng (1)
                     go to car-rig-cat-371.
      *                              *---------------------------------*
      *                              * Determinazione descrizione as-  *
      *                              * sociata al prodotto             *
      *                              *---------------------------------*
           move      w-rig-num-mag (1)    to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-380.
      *                      *-----------------------------------------*
      *                      * Descrizione riga                        *
      *                      *-----------------------------------------*
           move      w-rig-des-por (1)    to   w-rig-des-rig (1)      .
       car-rig-cat-385.
      *                      *-----------------------------------------*
      *                      * Descrizione ad uso interno e lotto di   *
      *                      * vendita                                 *
      *                      *-----------------------------------------*
       car-rig-cat-390.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica archivio relativo al *
      *                      * tipo riga                               *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-392
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-394
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-398
           else      go to car-rig-cat-400.
       car-rig-cat-392.
      *                      *-----------------------------------------*
      *                      * Tipo riga : 'P'                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      w-rig-num-mag (1)    to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           go to     car-rig-cat-400.
       car-rig-cat-394.
      *                      *-----------------------------------------*
      *                      * Tipo riga : 'M'                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dpm]              *
      *                          *-------------------------------------*
           move      w-rig-num-mag (1)    to   w-let-arc-dpm-cod      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           move      w-let-arc-dpm-des    to   w-rig-des-por (1)      .
           go to     car-rig-cat-400.
       car-rig-cat-398.
      *                      *-----------------------------------------*
      *                      * Tipo riga : 'A' o 'C'                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [zac]              *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A"
                     move  01             to   w-let-arc-zac-tip
           else      move  02             to   w-let-arc-zac-tip      .
           move      w-rig-tip-rig-cac (1)
                                          to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
           go to     car-rig-cat-400.
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
           else      go to car-rig-cat-410.
       car-rig-cat-402.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-tpr    to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-410.
      *                      *-----------------------------------------*
      *                      * Codice iva                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P" or
                                          =    "M" or
                                          =    "V"
                     go to car-rig-cat-412
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-414
           else      go to car-rig-cat-420.
       car-rig-cat-412.
      *                          *-------------------------------------*
      *                          * Tipo riga : Prodotto di vendita     *
      *                          *             Materia Prima           *
      *                          *             Materiale vario         *
      *                          *-------------------------------------*
           if        w-tes-ass-iva (1)    not  = zero
                     move  w-tes-ass-iva (1)
                                          to   w-rig-civ-rig (1)
           else      move  w-let-dcp-pdx-coi
                                          to   w-rig-civ-rig (1)      .
           go to     car-rig-cat-420.
       car-rig-cat-414.
      *                          *-------------------------------------*
      *                          * Tipo riga : Addebito                *
      *                          *-------------------------------------*
           move      w-let-arc-zac-civ    to   w-edt-iva-cod          .
      *
           if        w-tes-ass-iva (1)    not  = zero and
                     w-edt-iva-cod-003    =    zero
                     move  w-tes-ass-iva (1)
                                          to   w-rig-civ-rig (1)
           else      move  w-let-arc-zac-civ
                                          to   w-rig-civ-rig (1)      .
           go to     car-rig-cat-420.
       car-rig-cat-420.
      *                      *-----------------------------------------*
      *                      * Contropartita                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P" or
                                          =    "M" or
                                          =    "V"
                     go to car-rig-cat-422
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-424
           else      go to car-rig-cat-430.
       car-rig-cat-422.
      *                          *-------------------------------------*
      *                          * Tipo riga : Prodotto di vendita     *
      *                          *             Materia Prima           *
      *                          *             Materiale vario         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se indicata in testata ordine   *
      *                              *---------------------------------*
           if        w-tes-ctp-acq (1)    not  = zero
                     move  w-tes-ctp-acq (1)
                                          to   w-rig-ctp-rig (1)
                     go to car-rig-cat-430.
      *                              *---------------------------------*
      *                              * Se da leggere in [aaq]          *
      *                              *---------------------------------*
           move      w-rig-tip-mag (1)    to   w-let-arc-aaq-tpm      .
           move      w-rig-num-mag (1)    to   w-let-arc-aaq-cdm      .
           perform   let-arc-aaq-000      thru let-arc-aaq-999        .
           move      w-let-arc-aaq-ctp    to   w-rig-ctp-rig (1)      .
      *
           go to     car-rig-cat-430.
       car-rig-cat-424.
      *                          *-------------------------------------*
      *                          * Tipo riga : Addebito                *
      *                          *-------------------------------------*
           move      w-let-arc-zac-ccp    to   w-rig-ctp-rig (1)      .
           go to     car-rig-cat-430.
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
           else      go to car-rig-cat-440.
       car-rig-cat-432.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-umi    to   w-rig-umi-acq (1)      .
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
       car-rig-cat-450.
      *                      *-----------------------------------------*
      *                      * Quantita' ordinata                      *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     move  zero           to   w-rig-qta-ord (1)
                     go to car-rig-cat-460.
      *                          *-------------------------------------*
      *                          * Trasformazione unita' di misura     *
      *                          *-------------------------------------*
           move      rf-ofr-snx-tum       to   w-rig-snx-tum (1)      .
           move      rf-ofr-umf-tum       to   w-rig-umf-tum (1)      .
           move      rf-ofr-nde-tum       to   w-rig-nde-tum (1)      .
           move      rf-ofr-cmo-tum       to   w-rig-cmo-tum (1)      .
           move      rf-ofr-cdi-tum       to   w-rig-cdi-tum (1)      .
      *                          *-------------------------------------*
      *                          * Quantita' ordinata in base alla     *
      *                          * trasformazione unita' di misura     *
      *                          *-------------------------------------*
           if        w-rig-snx-tum (1)    =    "P"
                     move  w-bro-qta-dup
                          (w-car-rig-cat-ctr)
                                          to   w-rig-qta-ord (1)
           else if   w-rig-snx-tum (1)    =    "S"
                     move  w-bro-qta-dup
                          (w-car-rig-cat-ctr)
                                          to   w-rig-qta-fda (1)
           else      move  w-bro-qta-dup
                          (w-car-rig-cat-ctr)
                                          to   w-rig-qta-fda (1)      .
      *                          *-------------------------------------*
      *                          * Trattamento quantita' secondaria    *
      *                          *-------------------------------------*
           move      w-bro-qta-dup
                    (w-car-rig-cat-ctr)   to   w-det-qta-sec-qim      .
           move      w-rig-dec-qta (1)    to   w-det-qta-sec-dgm      .
           move      w-rig-snx-tum (1)    to   w-det-qta-sec-snu      .
           move      w-rig-umf-tum (1)    to   w-det-qta-sec-umf      .
           move      w-rig-nde-tum (1)    to   w-det-qta-sec-ndu      .
           move      w-rig-cmo-tum (1)    to   w-det-qta-sec-cmu      .
           move      w-rig-cdi-tum (1)    to   w-det-qta-sec-cdu      .
           perform   det-qta-sec-000      thru det-qta-sec-999        .
           if        w-rig-snx-tum (1)    =    "P"
                     move  w-det-qta-sec-qts
                                          to   w-rig-qta-fda (1)
           else if   w-rig-snx-tum (1)    =    "S"
                     move  w-det-qta-sec-qts
                                          to   w-rig-qta-ord (1)
           else      move  w-det-qta-sec-qts
                                          to   w-rig-qta-ord (1)      .
       car-rig-cat-460.
      *                      *-----------------------------------------*
      *                      * Segnale di riga considerata comunque    *
      *                      * saldata                                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-rig-sdr-ccs (1)      .
       car-rig-cat-470.
      *                      *-----------------------------------------*
      *                      * Seconda quantita' : non gestita         *
      *                      *-----------------------------------------*
       car-rig-cat-480.
      *                      *-----------------------------------------*
      *                      * Terza quantita' : non gestita           *
      *                      *-----------------------------------------*
       car-rig-cat-490.
      *                      *-----------------------------------------*
      *                      * Decimali prezzo                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P" or
                     w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-492
           else      go to car-rig-cat-500.
       car-rig-cat-492.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          * o Materia Prima                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * ricalcolo sul prezzo richiesto  *
      *                              *---------------------------------*
           if        w-acc-tip-rcl-prz    =    spaces
                     go to car-rig-cat-493
           else if   w-acc-tip-rcl-prz    =    "X"
                     go to car-rig-cat-494.
       car-rig-cat-493.
      *                              *---------------------------------*
      *                              * Se ricalcolo sul prezzo : No    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Bufferizzazione valore con- *
      *                                  * tenuto nel record [bir]     *
      *                                  *-----------------------------*
           move      rf-ofr-dec-prz       to   w-rig-dec-prz (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-494.
      *                              *---------------------------------*
      *                              * Se ricalcolo sul prezzo : Si    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Bufferizzazione valore con- *
      *                                  * tenuto nel record [dcp]     *
      *                                  *-----------------------------*
           move      w-let-dcp-pdx-dep    to   w-rig-dec-prz (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-500.
      *                      *-----------------------------------------*
      *                      * Valuta per prezzi standard              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo riga 'P' o 'M' o 'S'   *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "M" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "S"
                     go to car-rig-cat-510.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ri-   *
      *                          * calcolo sul prezzo richiesto        *
      *                          *-------------------------------------*
           if        w-acc-tip-rcl-prz    =    spaces
                     go to car-rig-cat-502
           else if   w-acc-tip-rcl-prz    =    "X"
                     go to car-rig-cat-504.
       car-rig-cat-502.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-510.
       car-rig-cat-504.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-510.
       car-rig-cat-510.
      *                      *-----------------------------------------*
      *                      * Prezzo lordo standard                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo riga 'P' o 'M' o 'S'   *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "M" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "S"
                     go to car-rig-cat-520.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ri-   *
      *                          * calcolo sul prezzo richiesto        *
      *                          *-------------------------------------*
           if        w-acc-tip-rcl-prz    =    spaces
                     go to car-rig-cat-512
           else if   w-acc-tip-rcl-prz    =    "X"
                     go to car-rig-cat-514.
       car-rig-cat-512.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-520.
       car-rig-cat-514.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : Si        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Normalizzazioni                 *
      *                              *---------------------------------*
           move      zero                 to   w-det-prz-qta-pre      .
           move      zero                 to   w-det-prz-qta-att      .
      *                              *---------------------------------*
      *                              * Preparazione link-area          *
      *                              *---------------------------------*
           move      "PA"                 to   d-prz-acq-tip-ope      .
           move      w-tes-cod-arc (1)    to   d-prz-acq-cod-fnt      .
           move      w-rig-tip-mag (1)    to   d-prz-acq-tip-mag      .
           move      w-rig-num-mag (1)    to   d-prz-acq-num-mag      .
           move      w-rig-fda-pif (1)    to   d-prz-acq-fda-pif      .
           move      w-let-dcp-pdx-plb    to   d-prz-acq-prz-lsb      .
           move      w-let-dcp-pdx-dlb    to   d-prz-acq-dec-vlb      .
           move      w-rig-dec-prz (1)    to   d-prz-acq-dec-prz      .
      *                                  *-----------------------------*
      *                                  * Preparazione quantita'      *
      *                                  *-----------------------------*
           if        w-rig-snx-tum (1)    =    "P"
                     move  w-rig-qta-ord (1)
                                          to   d-prz-acq-qta-rif
           else if   w-rig-snx-tum (1)    =    "S"
                     move  w-rig-qta-fda (1)
                                          to   d-prz-acq-qta-rif
           else      move  w-rig-qta-ord (1)
                                          to   d-prz-acq-qta-rif      .
      *                                  *-----------------------------*
      *                                  * Data di riferimento         *
      *                                  *-----------------------------*
           if        w-tes-cof-dat (1)    =    zero
                     move  w-tes-dat-doc  to   d-prz-acq-dat-rfp
           else      move  w-tes-cof-dat (1)
                                          to   d-prz-acq-dat-rfp      .
      *                              *---------------------------------*
      *                              * Richiamo del sottoprogramma     *
      *                              *---------------------------------*
           perform   det-prz-acq-cll-000  thru det-prz-acq-cll-999    .
      *                              *---------------------------------*
      *                              * Memorizzazione valore determi-  *
      *                              * nato                            *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Prezzo di acquisto          *
      *                                  *-----------------------------*
           move      d-prz-acq-prz-acq    to   w-rig-prz-acq (1)      .
      *                                  *-----------------------------*
      *                                  * Percentuali di sconto       *
      *                                  *-----------------------------*
           move      d-prz-acq-psr-det (1)
                                          to   w-rig-per-scr (1, 1)   .
           move      d-prz-acq-psr-det (2)
                                          to   w-rig-per-scr (1, 2)   .
           move      d-prz-acq-psr-det (3)
                                          to   w-rig-per-scr (1, 3)   .
           move      d-prz-acq-psr-det (4)
                                          to   w-rig-per-scr (1, 4)   .
           move      d-prz-acq-psr-det (5)
                                          to   w-rig-per-scr (1, 5)   .
      *                                  *-----------------------------*
      *                                  * Segnale di percentuali      *
      *                                  * sconto determinate          *
      *                                  *-----------------------------*
           move      d-prz-acq-psr-snx    to   w-rig-per-scr-ffp (1)  .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-520.
       car-rig-cat-520.
      *                      *-----------------------------------------*
      *                      * Valuta per il prezzo                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo riga 'P' o 'M' o 'S'   *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "M" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "S"
                     go to car-rig-cat-540.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ri-   *
      *                          * calcolo sul prezzo richiesto        *
      *                          *-------------------------------------*
           if        w-acc-tip-rcl-prz    =    spaces
                     go to car-rig-cat-522
           else if   w-acc-tip-rcl-prz    =    "X"
                     go to car-rig-cat-524.
       car-rig-cat-522.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-540.
       car-rig-cat-524.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : Si        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-540.
       car-rig-cat-540.
      *                      *-----------------------------------------*
      *                      * Prezzo di vendita                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ri-   *
      *                          * calcolo sul prezzo richiesto        *
      *                          *-------------------------------------*
           if        w-acc-tip-rcl-prz    =    spaces
                     go to car-rig-cat-542
           else if   w-acc-tip-rcl-prz    =    "X"
                     go to car-rig-cat-544.
       car-rig-cat-542.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione di vendita da   *
      *                              * record [ofr]                    *
      *                              *---------------------------------*
           move      rf-ofr-prz-acq       to   w-rig-prz-acq (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-550.
       car-rig-cat-544.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : Si        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-550.
       car-rig-cat-550.
      *                      *-----------------------------------------*
      *                      * Secondo prezzo : non gestito            *
      *                      *-----------------------------------------*
       car-rig-cat-560.
      *                      *-----------------------------------------*
      *                      * Legame valutario                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se gestione legame valutario non e- *
      *                          * sistente per la riga : oltre        *
      *                          *-------------------------------------*
           if        rf-ofr-sgl-vpl       =    spaces
                     go to car-rig-cat-570.
      *                          *-------------------------------------*
      *                          * Solo se tipo riga 'P'               *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-570.
      *                          *-------------------------------------*
      *                          * Bufferizzazione dati legame valuta- *
      *                          * rio da record [ofr], escluso coef-  *
      *                          * ficiente di cambio effettivo        *
      *                          *-------------------------------------*
           move      rf-ofr-sgl-vpl       to   w-rig-sgl-vpl (1)      .
           move      rf-ofr-dec-vpl       to   w-rig-dec-vpl (1)      .
           move      rf-ofr-tdc-vpl       to   w-rig-tdc-vpl (1)      .
           move      rf-ofr-prz-vpl       to   w-rig-prz-vpl (1)      .
           move      rf-ofr-ccr-vpl       to   w-rig-ccr-vpl (1)      .
           move      rf-ofr-plm-vpl       to   w-rig-plm-vpl (1)      .
           move      rf-ofr-tlm-vpl       to   w-rig-tlm-vpl (1)      .
           move      rf-ofr-map-vpl       to   w-rig-map-vpl (1)      .
      *                          *-------------------------------------*
      *                          * Determinazione coefficiente di cam- *
      *                          * bio effettivo per legame valutario  *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vpl (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-dec-vpl (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                          *-------------------------------------*
      *                          * Se cambio indeterminato : forzatura *
      *                          * di quello contenuto nel record      *
      *                          * [ofr]                               *
      *                          *-------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ofr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vpl (1)      .
       car-rig-cat-570.
      *                      *-----------------------------------------*
      *                      * Tipo esposizione riga in fattura        *
      *                      *-----------------------------------------*
       car-rig-cat-580.
      *                      *-----------------------------------------*
      *                      * Categoria e percentuali di sconto in    *
      *                      * riga associate al prodotto              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'P' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-590.
      *                          *-------------------------------------*
      *                          * Se prodotto similare : oltre        *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-590.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ri-   *
      *                          * calcolo sul prezzo richiesto        *
      *                          *-------------------------------------*
           if        w-acc-tip-rcl-prz    =    spaces
                     go to car-rig-cat-582
           else if   w-acc-tip-rcl-prz    =    "X"
                     go to car-rig-cat-584.
       car-rig-cat-582.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione valori contenu- *
      *                              * ti nel record [ofr]             *
      *                              *---------------------------------*
           move      rf-ofr-csr-aap       to   w-rig-csr-aap (1)      .
           move      rf-ofr-psr-aap (1)   to   w-rig-psr-aap (1, 1)   .
           move      rf-ofr-psr-aap (2)   to   w-rig-psr-aap (1, 2)   .
           move      rf-ofr-psr-aap (3)   to   w-rig-psr-aap (1, 3)   .
           move      rf-ofr-psr-aap (4)   to   w-rig-psr-aap (1, 4)   .
           move      rf-ofr-psr-aap (5)   to   w-rig-psr-aap (1, 5)   .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-590.
       car-rig-cat-584.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione valori contenu- *
      *                              * ti in record [dcp]              *
      *                              *---------------------------------*
           move      rf-dcp-cat-scr       to   w-rig-csr-aap (1)      .
           move      rf-dcp-per-scr (1)   to   w-rig-psr-aap (1, 1)   .
           move      rf-dcp-per-scr (2)   to   w-rig-psr-aap (1, 2)   .
           move      rf-dcp-per-scr (3)   to   w-rig-psr-aap (1, 3)   .
           move      rf-dcp-per-scr (4)   to   w-rig-psr-aap (1, 4)   .
           move      rf-dcp-per-scr (5)   to   w-rig-psr-aap (1, 5)   .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-590.
       car-rig-cat-590.
      *                      *-----------------------------------------*
      *                      * Sconti in riga                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ri-   *
      *                          * calcolo sugli sconti richiesto      *
      *                          *-------------------------------------*
           if        w-acc-tip-rcl-sco    =    spaces
                     go to car-rig-cat-592
           else if   w-acc-tip-rcl-sco    =    "X"
                     go to car-rig-cat-594.
       car-rig-cat-592.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sugli sconti : No      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione percentuali di  *
      *                              * sconto da record [ofr]          *
      *                              *---------------------------------*
           move      rf-ofr-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-ofr-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-ofr-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-ofr-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-ofr-per-scr (5)   to   w-rig-per-scr (1, 5)   .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-600.
       car-rig-cat-594.
      *                          *-------------------------------------*
      *                          * Se ricalcolo sugli sconti : Si      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo riga diverso da 'P' :   *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-600.
      *                              *---------------------------------*
      *                              * Se prodotto similare : oltre    *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-600.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           perform   det-per-scr-000      thru det-per-scr-999        .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-600.
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
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'A' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "A"
                     go to car-rig-cat-635.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      rf-ofr-imp-rig       to   w-rig-imp-rig (1)      .
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
      *                      * Data consegna richiesta                 *
      *                      *                                         *
      *                      * Da testata nuovo ordine                 *
      *                      *-----------------------------------------*
           move      w-tes-dat-cns (1)    to   w-rig-dcn-ric (1)      .
       car-rig-cat-700.
      *                      *-----------------------------------------*
      *                      * Data consegna prevista                  *
      *                      *                                         *
      *                      * Da testata nuovo ordine                 *
      *                      *-----------------------------------------*
           move      w-tes-dat-cns (1)    to   w-rig-dcn-prv (1)      .
      *                      *-----------------------------------------*
      *                      * Altri valori rimangono normalizzati     *
      *                      *-----------------------------------------*
       car-rig-cat-800.
      *                      *-----------------------------------------*
      *                      * Valori indotti                          *
      *                      *-----------------------------------------*
       car-rig-cat-805.
      *                          *-------------------------------------*
      *                          * Flag di estensione alla descrizione *
      *                          *-------------------------------------*
           perform   det-des-ext-000      thru det-des-ext-999        .
       car-rig-cat-810.
      *                          *-------------------------------------*
      *                          * Anagrafica titoli esenzione         *
      *                          *-------------------------------------*
           move      w-rig-civ-rig (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-rig-civ-rig-des (1)  .
       car-rig-cat-820.
      *                          *-------------------------------------*
      *                          * Anagrafica piano dei conti          *
      *                          *-------------------------------------*
           move      w-rig-ctp-rig (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-rig-ctp-rig-des (1)  .
       car-rig-cat-825.
      *                          *-------------------------------------*
      *                          * Prezzo netto standard               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo riga diverso da 'P' :   *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-830.
      *                              *---------------------------------*
      *                              * Se prodotto similare : oltre    *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-830.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ri-   *
      *                          * calcolo prezzo richiesto            *
      *                          *-------------------------------------*
           if        w-acc-tip-rcl-prz    =    spaces
                     go to car-rig-cat-826
           else if   w-acc-tip-rcl-prz    =    "X"
                     go to car-rig-cat-827.
       car-rig-cat-826.
      *                          *-------------------------------------*
      *                          * Se ricalcolo prezzo : No            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione prezzo netto da *
      *                              * record [ofr]                    *
      *                              *---------------------------------*
           move      rf-ofr-prz-net       to   w-rig-prz-net (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-830.
       car-rig-cat-827.
      *                          *-------------------------------------*
      *                          * Se ricalcolo prezzo : Si            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-rig-prz-acq (1)    to   w-cal-prz-net-prz      .
           move      w-rig-per-scr (1, 1) to   w-cal-prz-net-psc (1)  .
           move      w-rig-per-scr (1, 2) to   w-cal-prz-net-psc (2)  .
           move      w-rig-per-scr (1, 3) to   w-cal-prz-net-psc (3)  .
           move      w-rig-per-scr (1, 4) to   w-cal-prz-net-psc (4)  .
           move      w-rig-per-scr (1, 5) to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   w-rig-prz-net (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-830.
       car-rig-cat-830.
      *                          *-------------------------------------*
      *                          * Prezzo netto effettivo              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo riga 'A' o 'C' : oltre  *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-835.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-rig-prz-acq (1)    to   w-cal-prz-net-prz      .
           move      w-rig-per-scr (1, 1) to   w-cal-prz-net-psc (1)  .
           move      w-rig-per-scr (1, 2) to   w-cal-prz-net-psc (2)  .
           move      w-rig-per-scr (1, 3) to   w-cal-prz-net-psc (3)  .
           move      w-rig-per-scr (1, 4) to   w-cal-prz-net-psc (4)  .
           move      w-rig-per-scr (1, 5) to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   w-rig-prz-net (1)      .
       car-rig-cat-835.
      *                          *-------------------------------------*
      *                          * Importo in riga                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo riga 'A' o 'C' : oltre  *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-850.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
       car-rig-cat-850.
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
           call      "pgm/orf/prg/obj/porf3002"
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
           move      zero                 to   w-rig-qta-fda (1)      .
           move      zero                 to   w-rig-qta-ord (1)      .
           move      spaces               to   w-rig-sdr-ccs (1)      .
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
           move      spaces               to   w-rig-sgl-vpl (1)      .
           move      zero                 to   w-rig-dec-vpl (1)      .
           move      spaces               to   w-rig-tdc-vpl (1)      .
           move      zero                 to   w-rig-prz-vpl (1)      .
           move      zero                 to   w-rig-cdc-vpl (1)      .
           move      zero                 to   w-rig-ccr-vpl (1)      .
           move      zero                 to   w-rig-plm-vpl (1)      .
           move      spaces               to   w-rig-tlm-vpl (1)      .
           move      spaces               to   w-rig-map-vpl (1)      .
           move      zero                 to   w-rig-epz-rgo (1)      .
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
           move      zero                 to   w-rig-dcn-ric (1)      .
           move      spaces               to   w-rig-fds-dcr (1)      .
           move      zero                 to   w-rig-dcn-prv (1)      .
           move      spaces               to   w-rig-flg-cnf (1)      .
           move      spaces               to   w-rig-oda-tip (1)      .
           move      zero                 to   w-rig-oda-dat (1)      .
           move      zero                 to   w-rig-oda-num (1)      .
           move      spaces               to   w-rig-flg-rch (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
           move      zero                 to   w-rig-qta-ric (1)      .
           move      spaces               to   w-rig-tdo-urc (1)      .
           move      zero                 to   w-rig-drg-urc (1)      .
           move      zero                 to   w-rig-prt-urc (1)      .
       nor-nok-rig-999.
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
           move      w-tes-cod-dpz        to   s-num                  .
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
           move      w-tes-val-key        to   w-sav-val-acc          .
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
           move      w-let-arc-yof-sgl    to   w-acc-tmo-orf-sgl      .
       fnd-arc-oft-600.
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
           go to     fnd-arc-oft-999.
       fnd-arc-oft-900.
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
           move      w-tes-val-key        to   w-sav-val-acc          .
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
      *                  * movimento ordini fornitori                  *
      *                  *---------------------------------------------*
           move      w-let-arc-yof-des    to   w-acc-tmo-orf-des      .
           move      w-let-arc-yof-sgl    to   w-acc-tmo-orf-sgl      .
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
           move      w-sav-val-acc        to   w-tes-val-key          .
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
           move      rf-oft-num-doc (6:6) to   v-num                  .
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
           move      rf-yof-def-tpr       to   w-let-arc-yof-dtr      .
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
           move      spaces               to   w-let-arc-yof-dtr      .
       let-arc-yof-999.
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
           if        rf-aaf-xdp-sfn       =    0
                     move  rf-aaf-dep-sfn to   w-let-arc-aaf-des
                     go to let-arc-aaf-300.
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
           move      "CODLNG"             to   f-key                  .
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
           move      rf-aaq-dcf-pfz       to   w-let-arc-aaq-pfz      .
           move      rf-aaq-tmp-apv       to   w-let-arc-aaq-apv      .
           move      rf-aaq-uda-par       to   w-let-arc-aaq-uda      .
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
           move      zero                 to   w-let-arc-aaq-pfz      .
           move      zero                 to   w-let-arc-aaq-apv      .
           move      zero                 to   w-let-arc-aaq-uda      .
           move      spaces               to   w-let-arc-aaq-cdp      .
       let-arc-aaq-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

      *    *===========================================================*
      *    * Determinazione se documento gia' esistente oppure no      *
      *    *-----------------------------------------------------------*
       det-doc-ges-000.
      *              *-------------------------------------------------*
      *              * Protocollo movimento trovato                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-doc-ges-prt      .
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
      *              * Bufferizzazione valore determinato              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di documento esistente e trovato    *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-doc-ges-snx      .
      *                  *---------------------------------------------*
      *                  * Tipo documento esistente e trovato          *
      *                  *---------------------------------------------*
           move      rf-oft-tmo-orf       to   w-det-doc-ges-tmt      .
      *                  *---------------------------------------------*
      *                  * Protocollo individuato                      *
      *                  *---------------------------------------------*
           move      rf-oft-num-prt       to   w-det-doc-ges-prt      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-doc-ges-999.
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
      *    * Determinazione quantita' secondaria per l'acquisto        *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/wdetqts0.wks"                   .

      *    *===========================================================*
      *    * Editing di una quantita' da incolonnare                   *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Determinazione prezzo netto                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

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
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-aav = Ammontare da convertire, espres- *
      *    *                          so nell'altra valuta             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-avb = Ammontare convertito, espresso   *
      *    *                          nella valuta base                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta base ad altra valuta     *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-avb = Ammontare da convertire, espres- *
      *    *                          so nella valuta base             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-aav = Ammontare convertito, espresso   *
      *    *                          nell'altra valuta                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta a valuta                 *
      *    *                                                           *
      *    * Input  : Valuta da trasformare                            *
      *    *                                                           *
      *    *            - w-cvs-vdt-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vdt-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vdt-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vdt-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    *            - w-cvs-vdt-amm = Ammontare da convertire,     *
      *    *                              espresso nella valuta da     *
      *    *                              trasformare                  *
      *    *                                                           *
      *    *          Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vnu-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vnu-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vnu-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    * Output : Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-amm = Ammontare convertito,        *
      *    *                              espresso nella valuta        *
      *    *                              in cui trasformare           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

      *    *===========================================================*
      *    * Determinazione percentuali di sconto in riga              *
      *    *-----------------------------------------------------------*
       det-per-scr-000.
      *              *-------------------------------------------------*
      *              * Solo se riga di tipo 'new'                      *
      *              *-------------------------------------------------*
           if        w-cat-rig-new        =    spaces
                     go to det-per-scr-999.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento sconti in riga 001           *
      *              *-------------------------------------------------*
           perform   det-psr-001-000      thru det-psr-001-999        .
           go to     det-per-scr-999.
       det-per-scr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di sconto in riga tipo 001     *
      *    *                                                           *
      *    * N.B.: La determinazione degli sconti in riga segue la se- *
      *    *       guente scaletta di prevalenza:                      *
      *    *       - Quelli diretti dall'incrocio Fornitore/Prodotto   *
      *    *         contenuti nel record [aaq]                        *
      *    *       - Quelli contenuti direttamente in [dcf]            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-psr-001-000.
      *              *-------------------------------------------------*
      *              * Se gli sconti sono associati alla combinazione  *
      *              * fornitore/prodotto, questi sono gia' stati memo-*
      *              * rizzati e quindi si esce                        *
      *              *-------------------------------------------------*
           if        w-rig-per-scr-ffp (1)
                                          not  = spaces
                     go to det-psr-001-999.
       det-psr-001-200.
      *              *-------------------------------------------------*
      *              * Test se sconti diretti da record [dcf]          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se percentuali di sconto in riga associate  *
      *                  * al fornitore sono tutte a zero : oltre      *
      *                  *---------------------------------------------*
           if        w-tes-psr-aaf (1, 1) =    zero and
                     w-tes-psr-aaf (1, 2) =    zero and
                     w-tes-psr-aaf (1, 3) =    zero and
                     w-tes-psr-aaf (1, 4) =    zero and
                     w-tes-psr-aaf (1, 5) =    zero
                     go to det-psr-001-700.
      *                  *---------------------------------------------*
      *                  * Altrimenti si memorizzano                   *
      *                  *---------------------------------------------*
           move      w-tes-psr-aaf (1, 1) to   w-rig-per-scr (1, 1)   .
           move      w-tes-psr-aaf (1, 2) to   w-rig-per-scr (1, 2)   .
           move      w-tes-psr-aaf (1, 3) to   w-rig-per-scr (1, 3)   .
           move      w-tes-psr-aaf (1, 4) to   w-rig-per-scr (1, 4)   .
           move      w-tes-psr-aaf (1, 5) to   w-rig-per-scr (1, 5)   .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-psr-001-999.
       det-psr-001-700.
      *              *-------------------------------------------------*
      *              * A questo punto nessuno dei possibili tipi di de-*
      *              * terminazione si ' verificato, per cui non resta *
      *              * che normalizzare le percentuali di sconto in ri-*
      *              * ga ed uscire                                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-per-scr (1, 1)   .
           move      zero                 to   w-rig-per-scr (1, 2)   .
           move      zero                 to   w-rig-per-scr (1, 3)   .
           move      zero                 to   w-rig-per-scr (1, 4)   .
           move      zero                 to   w-rig-per-scr (1, 5)   .
           go to     det-psr-001-999.
       det-psr-001-999.
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
      *    * Subroutines per determinazione importo in riga acquisti   *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dimpacq0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione prezzo di acquisto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/dprzacq0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

