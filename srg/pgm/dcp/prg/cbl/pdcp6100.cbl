       Identification Division.
       Program-Id.                                 pdcp6100           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:    cdv                 *
      *                                   Fase:    dcp610              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 05/01/94    *
      *                       Ultima revisione:    NdK del 14/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione manuale prezzi per campagne promo- *
      *                    zionali                                     *
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
                     "dcp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "cdv"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcp610"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcp6100"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " GESTIONE CAMPAGNE PROMOZIONALI PRODOTTI"       .

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
      *            * Per routine pos-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pos-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-let-reg-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-let-reg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-ins-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-ins      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-mod-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-mod      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-acc-vis-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-acc-vis      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-snx-del-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-snx-del      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pos-snx-del-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pos-snx-del      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-tus.
      *            *---------------------------------------------------*
      *            * Da accettazione campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione campi non chiave                  *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-nok      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Da accettazione testata                           *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-tes      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-key-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per controllo se chiave vuota                     *
      *            *---------------------------------------------------*
               10  w-cnt-key-vuo-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi non chiave                  *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-nok-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Tipo impostazione                                 *
      *            * - K : Impostazione campi chiave                   *
      *            * - T : Impostazione campi testata                  *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-imp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo funzione                                     *
      *            * - I : Inserimento                                 *
      *            * - M : Modifica                                    *
      *            * - V : Visualizzazione                             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-tip-fun      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di controllo su status visualizzazione titolo    *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis-tit          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione chiave                               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina testata in corso di trattamento     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-npt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio per il campo precedente               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-svp      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Si/No pagina testata in corso di trattamento      *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-snp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero pagine componenti la testata               *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-mpt      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Impostazione testata                              *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-tes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Impostazione pagine di testata                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-pte.
                   15  w-cnt-sts-imp-ptx  occurs 9
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ingresso in pagine di testata                     *
      *            *---------------------------------------------------*
               10  w-cnt-sts-ing-pte.
                   15  w-cnt-sts-ing-ptx  occurs 9
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts chiave                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts testata                   *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-tes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts pagine di testata         *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-pte.
                   15  w-cnt-sts-pmt-ptx  occurs 9
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati chiave                       *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-key      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione dati testata                      *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-tes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Visualizzazione dati pagine di testata            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-pte.
                   15  w-cnt-sts-vis-ptx  occurs 9
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per accettazioni singoli campi      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
               10  w-cnt-acc-flg-aum      pic  x(01)                  .
               10  w-cnt-acc-sav-tip      pic  x(01)                  .
               10  w-cnt-acc-sav-car      pic  9(02)                  .
               10  w-cnt-acc-sav-ldt      pic  9(02)                  .
               10  w-cnt-acc-sav-dec      pic  9(01)                  .
               10  w-cnt-acc-sav-sgn      pic  x(01)                  .
               10  w-cnt-acc-sav-edm      pic  x(10)                  .
               10  w-cnt-acc-sav-msk      pic  x(24)                  .
               10  w-cnt-acc-sav-lin      pic  9(02)                  .
               10  w-cnt-acc-sav-pos      pic  9(02)                  .
               10  w-cnt-acc-sav-alf      pic  x(80)                  .
               10  w-cnt-acc-sav-txt.
                   15  filler occurs 400  pic  x(01)                  .
               10  w-cnt-acc-sav-num      pic s9(13)v9(05)  trailing
                                                            separate
                                                            character .
               10  w-cnt-acc-sav-dat      pic  9(07)                  .
               10  w-cnt-acc-sav-ufk      pic  x(80)                  .
               10  w-cnt-acc-sav-mod      pic  x(01)                  .
               10  w-cnt-acc-sav-l23      pic  x(80)                  .
               10  w-cnt-acc-sav-l24      pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Work area                                             *
      *        *-------------------------------------------------------*
           05  w-cnt-wrk.
               10  w-cnt-wrk-ctr-001      pic  9(05)                  .
               10  w-cnt-wrk-ctr-002      pic  9(05)                  .
               10  w-cnt-wrk-ctr-008      pic  9(05)                  .
               10  w-cnt-wrk-ctr-009      pic  9(05)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [zcs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzcs"                          .
      *        *-------------------------------------------------------*
      *        * [zpv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpv"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-tip-rec          pic  9(02)                  .
               10  w-tes-num-pro          pic  9(07)                  .
               10  w-tes-alf-pro          pic  x(14)                  .
               10  w-tes-num-pro-des      pic  x(40)                  .
               10  w-tes-num-pro-prz      pic  9(09)                  .
               10  w-tes-num-pro-dec      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-tip-fnz          pic  9(02)                  .
               10  w-tes-bas-lst          pic  9(09)                  .
               10  w-tes-prz-lst          pic  9(09)                  .
               10  w-tes-cat-pvg          pic  9(05)                  .
               10  w-tes-cat-pvg-des      pic  x(20)                  .
               10  w-tes-per-pvg occurs 03
                                          pic  9(02)v9(01)            .
               10  w-tes-cat-sco          pic  9(05)                  .
               10  w-tes-cat-sco-des      pic  x(20)                  .
               10  w-tes-per-sco occurs 05
                                          pic  9(02)v9(01)            .
               10  w-tes-snx-prz          pic  x(01)                  .
               10  w-tes-snx-sco          pic  x(01)                  .
               10  w-tes-snx-pvg          pic  x(01)                  .
               10  w-tes-dva-ini          pic  9(07)                  .
               10  w-tes-dva-fin          pic  9(07)                  .
               10  w-tes-snx-sto          pic  x(01)                  .
               10  w-tes-alx-exp.
                   15  filler occurs 06   pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per lettura personalizzazioni                   *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazione relativa al tipo funzionamento      *
      *        * prezzi di vendita per campagne promozionali           *
      *        *                                                       *
      *        * - 00 : Nessuna preferenza, sempre utilizzo di prezzo  *
      *        *        e sconti                                       *
      *        * - 01 : Utilizzo di prezzo, sconti e provvigioni       *
      *        * - 02 : Utilizzo solo di prezzo                        *
      *        * - 03 : Utilizzo solo di sconti                        *
      *        * - 04 : Utilizzo solo di provvigioni                   *
      *        * - 05 : Utilizzo di prezzo e sconti                    *
      *        * - 06 : Utilizzo di prezzo e provvigioni               *
      *        * - 07 : Utilizzo di sconti e provvigioni               *
      *        *-------------------------------------------------------*
           05  w-prs-tfu-pcp              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-cod      pic  9(07)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-prz      pic  9(09)                  .
               10  w-let-arc-dcp-dec      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcs.
               10  w-let-arc-zcs-flg      pic  x(01)                  .
               10  w-let-arc-zcs-tip      pic  9(02)                  .
               10  w-let-arc-zcs-cod      pic  9(05)                  .
               10  w-let-arc-zcs-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zpv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zpv.
               10  w-let-arc-zpv-flg      pic  x(01)                  .
               10  w-let-arc-zpv-tip      pic  9(02)                  .
               10  w-let-arc-zpv-cod      pic  9(05)                  .
               10  w-let-arc-zpv-des      pic  x(20)                  .

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
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio campo 'Si/No storicizzazione valori pre-  *
      *        * cedenti pre determinazione della sua significativita' *
      *        *-------------------------------------------------------*
           05  w-sav-det-snx-sto          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore per percentuali                             *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing                           *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Area editata per 5 percentuali di sconto              *
      *        *-------------------------------------------------------*
           05  w-edt-per-sco.
               10  w-edt-cst-psr occurs 5.
                   15  w-edt-psr-ele      pic  x(04)                  .
                   15  filler             pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Area editata per 3 percentuali di provvigioni         *
      *        *-------------------------------------------------------*
           05  w-edt-per-pvg.
               10  w-edt-cst-ppv occurs 3.
                   15  w-edt-ppv-ele      pic  x(04)                  .
                   15  filler             pic  x(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo funzionamento                         *
      *        *-------------------------------------------------------*
           05  w-exp-tip-fun.
               10  w-exp-tip-fun-num      pic  9(02)       value 7    .
               10  w-exp-tip-fun-lun      pic  9(02)       value 40   .
               10  w-exp-tip-fun-tbl.
                   15  filler             pic  x(40) value
                            "Utilizzo di prezzo, sconti e provvigioni".
                   15  filler             pic  x(40) value
                            "Utilizzo solo di prezzo                 ".
                   15  filler             pic  x(40) value
                            "Utilizzo solo di sconti                 ".
                   15  filler             pic  x(40) value
                            "Utilizzo solo di provvigioni            ".
                   15  filler             pic  x(40) value
                            "Utilizzo di prezzo e sconti             ".
                   15  filler             pic  x(40) value
                            "Utilizzo di prezzo e provvigioni        ".
                   15  filler             pic  x(40) value
                            "Utilizzo di sconti e provvigioni        ".
      *        *-------------------------------------------------------*
      *        * Work per : Si/No storicizzazione valori precedenti    *
      *        *-------------------------------------------------------*
           05  w-exp-snx-sto.
               10  w-exp-snx-sto-num      pic  9(02)       value 2    .
               10  w-exp-snx-sto-lun      pic  9(02)       value 02   .
               10  w-exp-snx-sto-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione categoria sconto in riga pro-  *
      *    * dotto                                                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzcs3.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione categoria provvigione legata   *
      *    * al prodotto                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnzpv2.acl"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di visualizzazione titolo  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-tit      .
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
       main-050.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
       main-100.
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo uscita "E" : fine programma         *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Lettura registrazione pre-esistente             *
      *              *-------------------------------------------------*
           perform   rou-let-reg-000      thru rou-let-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se esito negativo : riciclo su chiave       *
      *                  *---------------------------------------------*
           if        w-cnt-rou-let-reg    not  = spaces
                     go to main-100.
      *              *-------------------------------------------------*
      *              * Routine pre-accettazione campi non chiave       *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     go to main-130
           else if   w-cnt-mfu-tip-fun    =    "M"
                     go to main-140
           else if   w-cnt-mfu-tip-fun    =    "V"
                     go to main-150
           else      go to main-100.
       main-130.
           move      spaces               to   w-cnt-pre-acc-ins      .
           perform   pre-acc-ins-000      thru pre-acc-ins-999        .
           if        w-cnt-pre-acc-ins    =    spaces
                     go to main-180
           else      go to main-100.
       main-140.
           move      spaces               to   w-cnt-pre-acc-mod      .
           perform   pre-acc-mod-000      thru pre-acc-mod-999        .
           if        w-cnt-pre-acc-mod    =    spaces
                     go to main-180
           else      go to main-100.
       main-150.
           move      spaces               to   w-cnt-pre-acc-vis      .
           perform   pre-acc-vis-000      thru pre-acc-vis-999        .
           if        w-cnt-pre-acc-vis    =    spaces
                     go to main-180
           else      go to main-100.
       main-180.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di modifica di almeno un   *
      *              * campo non chiave                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Accettazione campi non chiave                   *
      *              *-------------------------------------------------*
           perform   acc-nok-reg-000      thru acc-nok-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "E"
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Routine post-exit su campi non chiave   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Inserimento                      *
      *                          *-------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-exi-ins-000
                                          thru pos-exi-ins-999
      *                          *-------------------------------------*
      *                          * Se Modifica                         *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-exi-mod-000
                                          thru pos-exi-mod-999
      *                          *-------------------------------------*
      *                          * Se Visualizzazione                  *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "V"
                     perform   pos-exi-vis-000
                                          thru pos-exi-vis-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-200.
      *                  *---------------------------------------------*
      *                  * Se uscita per annullamento                  *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-nok    not  = "X"
                     go to main-300.
      *                      *-----------------------------------------*
      *                      * Routine post-conferma di annullamento   *
      *                      *-----------------------------------------*
           perform   pos-cnf-ann-000      thru pos-cnf-ann-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-300.
      *                  *---------------------------------------------*
      *                  * Se uscita per conferma                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Routine post-conferma                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se post-conferma di Inserimento     *
      *                          *-------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "I"
                     perform   pos-cnf-ins-000
                                          thru pos-cnf-ins-999
      *                          *-------------------------------------*
      *                          * Se post-conferma di Modifica        *
      *                          *-------------------------------------*
           else if   w-cnt-mfu-tip-fun    =    "M"
                     perform   pos-cnf-mod-000
                                          thru pos-cnf-mod-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     main-100.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pos-exe-pgm      .
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
           if        w-cnt-pos-exe-pgm    not  = spaces
                     go to main-050.
       main-900.
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
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Prsc :                        *
      *              * - se in tipo impostazione testata e su pagina   *
      *              *   maggiore di 1 : abilitato                     *
      *              * - altrimenti    : invariato                     *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "T" and
                     w-cnt-sts-imp-npt    >    1
                     move  "PRSC"         to   v-pfk (07)             .
      *              *-------------------------------------------------*
      *              * Tasto di funzione Nxsc :                        *
      *              * - se in tipo impostazione testata e se pagina   *
      *              *   gia' completamente impostata o testata gia'   *
      *              *   completamente impostata : abilitato           *
      *              *  - altrimenti             : invariato           *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    not  = "T"
                     go to exe-acc-cmp-050.
           if        w-cnt-sts-imp-tes    not  = spaces or
                     w-cnt-sts-imp-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces
                     move  "NXSC"         to   v-pfk (08)             .
       exe-acc-cmp-050.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Delt :                        *
      *              *  - se Impostazione chiave    : non abilitato    *
      *              *  - se Inserimento            : non abilitato    *
      *              *  - se Visualizzazione        : non abilitato    *
      *              *  - se Almeno una modifica    : non abilitato    *
      *              *  - altrimenti                : abilitato        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"   or
                     w-cnt-mfu-tip-fun    =    "I"   or
                     w-cnt-mfu-tip-fun    =    "V"   or
                     w-cnt-acc-flg-aum    not  = spaces
                     go to exe-acc-cmp-100.
           move      "DELT"               to   v-pfk (19)             .
       exe-acc-cmp-100.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Do :                          *
      *              *  - se Visualizzazione     : disabilitati        *
      *              *  - altrimenti             : inalterati          *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     move  spaces         to   v-pfk (05)             .
      *              *-------------------------------------------------*
      *              * Salvataggio parametri di accettazione           *
      *              *-------------------------------------------------*
           move      v-tip                to   w-cnt-acc-sav-tip      .
           move      v-car                to   w-cnt-acc-sav-car      .
           move      v-lin                to   w-cnt-acc-sav-lin      .
           move      v-pos                to   w-cnt-acc-sav-pos      .
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" or
                     v-tip                =    "E"
                     move  v-alf          to   w-cnt-acc-sav-alf      .
           if        v-tip                =    "N" or
                     v-tip                =    "V" or
                     v-tip                =    "P" or
                     v-tip                =    "E"
                     move  v-dec          to   w-cnt-acc-sav-dec
                     move  v-sgn          to   w-cnt-acc-sav-sgn
                     move  v-edm          to   w-cnt-acc-sav-edm
                     move  v-msk          to   w-cnt-acc-sav-msk
                     move  v-num          to   w-cnt-acc-sav-num      .
           if        v-tip                =    "D"
                     move  v-dat          to   w-cnt-acc-sav-dat      .
           if        v-tip                =    "T" or
                     v-tip                =    "E"
                     move  v-ldt          to   w-cnt-acc-sav-ldt
                     move  v-txt          to   w-cnt-acc-sav-txt      .
           move      v-ufk                to   w-cnt-acc-sav-ufk      .
       exe-acc-cmp-200.
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione segnale di campo modificato      *
      *              *-------------------------------------------------*
           move      v-mod                to   w-cnt-acc-sav-mod      .
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "DELT"
                     go to exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Se il campo impostato ha modificato il valore   *
      *              * precedente : ripetizione impostazione           *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     go to exe-acc-cmp-400.
      *              *-------------------------------------------------*
      *              * Routine pre-richiesta di ratifica tasto Delete  *
      *              *-------------------------------------------------*
           perform   pre-snx-del-000      thru pre-snx-del-999        .
      *              *-------------------------------------------------*
      *              * Se errore : a ripristino                        *
      *              *-------------------------------------------------*
           if        w-cnt-pre-snx-del    not  = spaces
                     go to exe-acc-cmp-400.
      *              *-------------------------------------------------*
      *              * Salvataggio linee 23 e 24 di note operative     *
      *              *-------------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      23                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l23      .
           move      "GL"                 to   v-ope                  .
           move      24                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-cnt-acc-sav-l24      .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Richiesta conferma annullamento                 *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#DEL"               to   v-not                  .
           move      "N"                  to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "DELT"               to   v-pfk (19)             .
           move      "UP  "               to   v-pfk (02)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    spaces
                     if      v-alf        =    "S"
                             move  "DELT" to   v-key
                     else    move  "UP  " to   v-key                  .
      *              *-------------------------------------------------*
      *              * Se non function key DELT si ripristina          *
      *              *-------------------------------------------------*
           if        v-key                not  = "DELT"
                     go to exe-acc-cmp-300.
      *              *-------------------------------------------------*
      *              * Routine post-richiesta di ratifica tasto Delete *
      *              *-------------------------------------------------*
           perform   pos-snx-del-000      thru pos-snx-del-999        .
      *              *-------------------------------------------------*
      *              * Se errore : a ripristino                        *
      *              *-------------------------------------------------*
           if        w-cnt-pos-snx-del    not  = spaces
                     go to exe-acc-cmp-300.
      *              *-------------------------------------------------*
      *              * Altrimenti : uscita                             *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-999.
       exe-acc-cmp-300.
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 di note operative      *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      w-cnt-acc-sav-l23    to   v-nt1                  .
           move      w-cnt-acc-sav-l24    to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-400.
      *              *-------------------------------------------------*
      *              * Ripristino parametri di impostazione            *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      w-cnt-acc-sav-tip    to   v-tip                  .
           move      w-cnt-acc-sav-car    to   v-car                  .
           move      w-cnt-acc-sav-lin    to   v-lin                  .
           move      w-cnt-acc-sav-pos    to   v-pos                  .
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-alf
                                          to   v-alf                  .
           if        v-tip                =    "N" or
                     v-tip                =    "V" or
                     v-tip                =    "P" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-dec
                                          to   v-dec
                     move  w-cnt-acc-sav-sgn
                                          to   v-sgn
                     move  w-cnt-acc-sav-edm
                                          to   v-edm
                     move  w-cnt-acc-sav-msk
                                          to   v-msk
                     move  w-cnt-acc-sav-num
                                          to   v-num                  .
           if        v-tip                =    "D"
                     move  w-cnt-acc-sav-dat
                                          to   v-dat                  .
           if        v-tip                =    "T" or
                     v-tip                =    "E"
                     move  w-cnt-acc-sav-ldt
                                          to   v-ldt
                     move  w-cnt-acc-sav-txt
                                          to   v-txt                  .
           move      w-cnt-acc-sav-ufk    to   v-ufk                  .
      *              *-------------------------------------------------*
      *              * Ritorno alla impostazione                       *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-200.
       exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Flag globale di avvenuta almeno una modifica    *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     move  "#"            to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
       exe-acc-cmp-999.
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
      *              * Test su flag di visualizzazione titolo          *
      *              *-------------------------------------------------*
           if        w-cnt-sts-vis-tit    not  = spaces
                     move  spaces         to   w-cnt-sts-vis-tit
                     go to vis-tit-pgm-999.
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
      *    * Visualizzazione tipo funzionamento                        *
      *    *-----------------------------------------------------------*
       vis-tip-fun-000.
           move      "TF"                 to   v-ope                  .
           move      w-cnt-mfu-tip-fun    to   v-tfu                  .
           move      w-cnt-acc-flg-aum    to   v-tfm                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
           move      "#"                  to   w-cnt-sts-vis-tit      .
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
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice categoria di    *
      *              * sconto prodotto                                 *
      *              *-------------------------------------------------*
           perform   cmn-zcs-003-opn-000  thru cmn-zcs-003-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione categoria provvigioni  *
      *              *-------------------------------------------------*
           perform   cmn-zpv-002-opn-000  thru cmn-zpv-002-opn-999    .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Personalizzazione per tipo funzionamento    *
      *                  * prezzi per campagne promozionali            *
      *                  *---------------------------------------------*
           perform   prs-tfu-pcp-000      thru prs-tfu-pcp-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione per il tipo funzionamento *
      *    * dei prezzi per campagne promozionali                      *
      *    *-----------------------------------------------------------*
       prs-tfu-pcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-tfu-pcp          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp/cdv[tfu-pcp]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     go to prs-tfu-pcp-999.
      *              *-------------------------------------------------*
      *              * Valore personalizzazione in work-area           *
      *              *-------------------------------------------------*
           move      s-num                to   w-prs-tfu-pcp          .
      *              *-------------------------------------------------*
      *              * Controllo personalizzazione                     *
      *              *-------------------------------------------------*
           if        w-prs-tfu-pcp        <    01 or
                     w-prs-tfu-pcp        >    07
                     move  00             to   w-prs-tfu-pcp          .
       prs-tfu-pcp-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice categoria di   *
      *              * sconto prodotto                                 *
      *              *-------------------------------------------------*
           perform   cmn-zcs-003-cls-000  thru cmn-zcs-003-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione categoria provvigioni *
      *              *-------------------------------------------------*
           perform   cmn-zpv-002-cls-000  thru cmn-zpv-002-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [lst]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *              *-------------------------------------------------*
      *              * [lsd]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [zcs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *              *-------------------------------------------------*
      *              * [zpv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [lst]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *              *-------------------------------------------------*
      *              * [lsd]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [zcs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
      *              *-------------------------------------------------*
      *              * [zpv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave della registrazione             *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-key      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-mfu-tip-fun      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo impostazione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-mfu-tip-imp      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-npt      .
           move      spaces               to   w-cnt-sts-imp-key
                                               w-cnt-sts-imp-tes
                                               w-cnt-sts-imp-pte
                                               w-cnt-sts-ing-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key
                                               w-cnt-sts-pmt-tes
                                               w-cnt-sts-pmt-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-key
                                               w-cnt-sts-vis-tes
                                               w-cnt-sts-vis-pte      .
      *              *-------------------------------------------------*
      *              * Normalizzazione registrazione                   *
      *              *-------------------------------------------------*
           perform   nor-key-nok-000      thru nor-key-nok-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Prompts per campi chiave                    *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-key      .
      *                  *---------------------------------------------*
      *                  * Prompts per prima pagina testata            *
      *                  *---------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
           move      "#"                  to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo impostazione : chiave                      *
      *              *-------------------------------------------------*
           move      "K"                  to   w-cnt-mfu-tip-imp      .
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           perform   acc-cod-pro-000      thru acc-cod-pro-999        .
       acc-key-reg-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni chiave    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-key      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visual. dati chiave    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-key      .
      *              *-------------------------------------------------*
      *              * Test se chiave vuota                            *
      *              *-------------------------------------------------*
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-key-reg-999.
      *              *-------------------------------------------------*
      *              * Controllo globale su tasto Do su chiave         *
      *              *-------------------------------------------------*
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-key-reg-100.
           move      "S"                  to   w-cnt-tus-acc-key      .
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campi chiave della registrazione          *
      *    *-----------------------------------------------------------*
       vis-key-reg-000.
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-pro-000      thru vis-cod-pro-999        .
           perform   vis-cod-pro-des-000  thru vis-cod-pro-des-999    .
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      05                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-pro-000      thru pmt-cod-pro-999        .
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
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice prodotto                                  *
      *    *-----------------------------------------------------------*
       pmt-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice prodotto  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice prodotto                            *
      *    *-----------------------------------------------------------*
       acc-cod-pro-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-pro-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      w-tes-num-pro        to   w-cod-cod-dcp-num      .
           move      w-tes-alf-pro        to   w-cod-cod-dcp-alf      .
           move      04                   to   w-cod-cod-dcp-lin      .
           move      20                   to   w-cod-cod-dcp-pos      .
           move      04                   to   w-cod-cod-dcp-dln      .
           move      37                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-cod-pro-110.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-cod-pro-115.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-cod-pro-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-pro-115.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-cod-pro-110.
       acc-cod-pro-120.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           move      w-cod-cod-dcp-num    to   v-num                  .
       acc-cod-pro-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pro-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-alf-pro          .
           move      v-num                to   w-tes-num-pro          .
       acc-cod-pro-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-tes-num-pro        to   w-let-arc-dcp-cod      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione prezzo listino   *
      *                  *---------------------------------------------*
           move      w-let-arc-dcp-des    to   w-tes-num-pro-des      .
           move      w-let-arc-dcp-prz    to   w-tes-num-pro-prz      .
           move      w-let-arc-dcp-dec    to   w-tes-num-pro-dec      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione prodotto        *
      *                  *---------------------------------------------*
           perform   vis-cod-pro-des-000  thru vis-cod-pro-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-cod-pro-100.
      *                  *---------------------------------------------*
      *                  * Se codice a zero : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-tes-num-pro        =    zero
                     go to acc-cod-pro-100.
       acc-cod-pro-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore listino base            *
      *                  *---------------------------------------------*
           move      w-tes-num-pro-prz    to   w-tes-bas-lst (1)      .
       acc-cod-pro-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-pro-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-pro-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-pro-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-pro-999.
       acc-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice prodotto                         *
      *    *-----------------------------------------------------------*
       vis-cod-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-tes-alf-pro        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice prodotto, descrizione            *
      *    *-----------------------------------------------------------*
       vis-cod-pro-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-num-pro-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi non chiave della registrazione         *
      *    *-----------------------------------------------------------*
       acc-nok-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-nok      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Assestamento status di impostazione se in modi- *
      *              * fica o visualizzazione, per testata             *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M" or
                     w-cnt-mfu-tip-fun    =    "V"
                     move  "#"            to   w-cnt-sts-imp-tes
                     move  all "#"        to   w-cnt-sts-imp-pte
                     move  all "#"        to   w-cnt-sts-ing-pte      .
       acc-nok-reg-200.
      *              *-------------------------------------------------*
      *              * Trattamento testata                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione del numero di pagine che     *
      *                  * compongono la testata                       *
      *                  *---------------------------------------------*
           perform   dmp-tes-reg-000      thru dmp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompts per campi chiave                    *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-key    =    spaces
                     perform pmt-key-reg-000
                                          thru pmt-key-reg-999
                     move    "#"          to   w-cnt-sts-pmt-key      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati campi chiave           *
      *                  *---------------------------------------------*
           if        w-cnt-sts-vis-key    =    spaces
                     perform vis-key-reg-000
                                          thru vis-key-reg-999
                     move    "#"          to   w-cnt-sts-vis-key      .
      *                  *---------------------------------------------*
      *                  * Prompts per pagina di testata               *
      *                  *---------------------------------------------*
           if        w-cnt-sts-pmt-ptx
                    (w-cnt-sts-imp-npt)   =    spaces
                     perform pmt-tes-reg-000
                                          thru pmt-tes-reg-999
                     move    "#"          to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Visualizzazione dati pagina testata         *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "M"      or
                     w-cnt-mfu-tip-fun    =    "V"      or
                     w-cnt-sts-imp-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces or
                     w-cnt-sts-ing-ptx
                    (w-cnt-sts-imp-npt)   not  = spaces
                     if    w-cnt-sts-vis-ptx
                          (w-cnt-sts-imp-npt)
                                          =    spaces
                           perform vis-tes-reg-000
                                          thru vis-tes-reg-999
                           move    "#"    to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Flag di ingresso in pagina di testata       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-ing-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione dati testata                   *
      *                  *---------------------------------------------*
           perform   acc-tes-reg-000      thru acc-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo uscita definitivo                   *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-tes    =    "S" or
                     w-cnt-tus-acc-tes    =    "X" or
                     w-cnt-tus-acc-tes    =    "E"
                     move  w-cnt-tus-acc-tes
                                          to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999.
      *                  *---------------------------------------------*
      *                  * Se spostamento di pagina                    *
      *                  *---------------------------------------------*
           if        w-cnt-tus-acc-tes    =    "-"
                     go to acc-nok-reg-300
           else      go to acc-nok-reg-400.
       acc-nok-reg-300.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina precedente      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' 1 si ignora *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  > 1
                     go to acc-nok-reg-200.
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della chiave a : no            *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      .
           move      spaces               to   w-cnt-sts-vis-key      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della pagina attuale a : no    *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-npt)     .
           move      spaces               to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-npt)     .
       acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attuale    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : decremento ulteriore        *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces and
                     w-cnt-sts-imp-npt    >    1
                     go to acc-nok-reg-325.
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-400.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio numero pagina attuale   *
      *                          *-------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-nok-reg-425.
      *                          *-------------------------------------*
      *                          * Status di impostazione dati della   *
      *                          * della pagina attuale a : si'        *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ptx
                                              (w-cnt-sts-imp-npt)     .
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' la massima  *
      *                          * si pone lo status di impostazione   *
      *                          * dati generale testata a si'         *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  "#"            to   w-cnt-sts-imp-tes      .
      *                          *-------------------------------------*
      *                          * Se la pagina attuale e' la massima  *
      *                          * si va' a conferma impostazioni      *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-nok-reg-800.
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attuale    *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Test se pagina da trattare          *
      *                          *-------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                          *-------------------------------------*
      *                          * Se no : a re-incremento             *
      *                          *-------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-nok-reg-425.
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della chiave a : no            *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-key      .
           move      spaces               to   w-cnt-sts-vis-key      .
      *                          *-------------------------------------*
      *                          * Status di visualizzazione prompts e *
      *                          * dati della pagina impostata a : no  *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ptx
                                              (w-cnt-sts-imp-svp)     .
           move      spaces               to   w-cnt-sts-vis-ptx
                                              (w-cnt-sts-imp-svp)     .
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-800.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-nok-reg-810.
      *                  *---------------------------------------------*
      *                  * Test se sola visualizzazione                *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     go to acc-nok-reg-820
           else      go to acc-nok-reg-830.
       acc-nok-reg-820.
      *                      *-----------------------------------------*
      *                      * Accettazione se sola visualizzazione    *
      *                      *-----------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#VIS"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "E"                  to   v-msk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-nok-reg-840.
           if        v-alf                =    "E"
                     move  "EXIT"         to   v-key                  .
           go to     acc-nok-reg-840.
       acc-nok-reg-830.
      *                      *-----------------------------------------*
      *                      * Accettazione se inserimento o modifica  *
      *                      *-----------------------------------------*
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
                     go to acc-nok-reg-840.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-nok-reg-840.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-nok-reg-850
           else if   v-key                =    "EXIT"
                     go to acc-nok-reg-860
           else if   v-key                =    "UP  "
                     go to acc-nok-reg-870
           else      go to acc-nok-reg-810.
       acc-nok-reg-850.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-nok
                     go to acc-nok-reg-999
           else      move  spaces         to   w-cnt-tdo-nok-flg
                     go to acc-nok-reg-800.
       acc-nok-reg-860.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-tus-acc-nok      .
           go to     acc-nok-reg-999.
       acc-nok-reg-870.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazione testata                 *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-nok-reg-200.
       acc-nok-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero pagine che compongono la testata    *
      *    *-----------------------------------------------------------*
       dmp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * La testata e' composta di nr. 1 pagine          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione si/no pagina w-cnt-sts-imp-npt da trattare *
      *    *-----------------------------------------------------------*
       snp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Flag di uscita a Si                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-snp      .
       snp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione testata registrazione                        *
      *    *-----------------------------------------------------------*
       acc-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo uscita                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-tes      .
      *              *-------------------------------------------------*
      *              * Normalizzazione function-key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Tipo impostazione : testata                     *
      *              *-------------------------------------------------*
           move      "T"                  to   w-cnt-mfu-tip-imp      .
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero pagina    *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Data inizio campagna promozionale           *
      *                  *---------------------------------------------*
           perform   acc-dva-ini-000      thru acc-dva-ini-999        .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Data fine campagna promozionale             *
      *                  *---------------------------------------------*
           perform   acc-dva-fin-000      thru acc-dva-fin-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
           perform   acc-tip-fnz-000      thru acc-tip-fnz-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Prezzo di listino                           *
      *                  *---------------------------------------------*
           perform   acc-prz-lst-000      thru acc-prz-lst-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Categoria di sconto in riga                 *
      *                  *---------------------------------------------*
           perform   acc-cat-sco-000      thru acc-cat-sco-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * % sconto                                    *
      *                  *---------------------------------------------*
           perform   acc-per-sco-000      thru acc-per-sco-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Categoria provvigioni                       *
      *                  *---------------------------------------------*
           perform   acc-cat-pvg-000      thru acc-cat-pvg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-170.
      *                  *---------------------------------------------*
      *                  * % provvigioni                               *
      *                  *---------------------------------------------*
           perform   acc-per-pvg-000      thru acc-per-pvg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
       acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Si/No storicizzazione valori precedenti     *
      *                  *---------------------------------------------*
           perform   acc-snx-sto-000      thru acc-snx-sto-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-170.
       acc-tes-reg-190.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione testata registrazione                     *
      *    *-----------------------------------------------------------*
       vis-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Data inizio campagna promozionale               *
      *              *-------------------------------------------------*
           perform   vis-dva-ini-000      thru vis-dva-ini-999        .
      *              *-------------------------------------------------*
      *              * Data fine campagna promozionale                 *
      *              *-------------------------------------------------*
           perform   vis-dva-fin-000      thru vis-dva-fin-999        .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento                              *
      *              *-------------------------------------------------*
           perform   vis-tip-fnz-000      thru vis-tip-fnz-999        .
      *              *-------------------------------------------------*
      *              * Prezzo di listino                               *
      *              *-------------------------------------------------*
           perform   vis-prz-lst-000      thru vis-prz-lst-999        .
      *              *-------------------------------------------------*
      *              * Categoria di sconto in riga                     *
      *              *-------------------------------------------------*
           perform   vis-cat-sco-000      thru vis-cat-sco-999        .
           perform   vis-des-sco-000      thru vis-des-sco-999        .
      *              *-------------------------------------------------*
      *              * % sconto                                        *
      *              *-------------------------------------------------*
           perform   vis-per-sco-000      thru vis-per-sco-999        .
      *              *-------------------------------------------------*
      *              * Categoria provvigioni                           *
      *              *-------------------------------------------------*
           perform   vis-cat-pvg-000      thru vis-cat-pvg-999        .
           perform   vis-des-cpv-000      thru vis-des-cpv-999        .
      *              *-------------------------------------------------*
      *              * % provvigioni                                   *
      *              *-------------------------------------------------*
           perform   vis-per-pvg-000      thru vis-per-pvg-999        .
      *              *-------------------------------------------------*
      *              * Si/No storicizzazione valori precedenti         *
      *              *-------------------------------------------------*
           perform   vis-snx-sto-000      thru vis-snx-sto-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts testata                           *
      *    *-----------------------------------------------------------*
       pmt-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-100
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Data inizio campagna promozionale               *
      *              *-------------------------------------------------*
           perform   pmt-dva-ini-000      thru pmt-dva-ini-999        .
      *              *-------------------------------------------------*
      *              * Data fine campagna promozionale                 *
      *              *-------------------------------------------------*
           perform   pmt-dva-fin-000      thru pmt-dva-fin-999        .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento                              *
      *              *-------------------------------------------------*
           perform   pmt-tip-fnz-000      thru pmt-tip-fnz-999        .
      *              *-------------------------------------------------*
      *              * Prezzo di listino                               *
      *              *-------------------------------------------------*
           perform   pmt-prz-lst-000      thru pmt-prz-lst-999        .
      *              *-------------------------------------------------*
      *              * Categoria di sconto in riga                     *
      *              *-------------------------------------------------*
           perform   pmt-cat-sco-000      thru pmt-cat-sco-999        .
      *              *-------------------------------------------------*
      *              * % sconto                                        *
      *              *-------------------------------------------------*
           perform   pmt-per-sco-000      thru pmt-per-sco-999        .
      *              *-------------------------------------------------*
      *              * Categoria provvigioni                           *
      *              *-------------------------------------------------*
           perform   pmt-cat-pvg-000      thru pmt-cat-pvg-999        .
      *              *-------------------------------------------------*
      *              * % provvigioni                                   *
      *              *-------------------------------------------------*
           perform   pmt-per-pvg-000      thru pmt-per-pvg-999        .
      *              *-------------------------------------------------*
      *              * Si/No storicizzazione valori precedenti         *
      *              *-------------------------------------------------*
           perform   pmt-snx-sto-000      thru pmt-snx-sto-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data inizio campagna promozionale                *
      *    *-----------------------------------------------------------*
       pmt-dva-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Validita' campagna     dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dva-ini-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data fine campagna promozionale                  *
      *    *-----------------------------------------------------------*
       pmt-dva-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "al"                 to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dva-fin-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo funzionamento                               *
      *    *-----------------------------------------------------------*
       pmt-tip-fnz-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Nessuna Preferenza per il tipo funziona- *
      *                  * mento prezzi per campagna promozionale :    *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to pmt-tip-fnz-999.
       pmt-tip-fnz-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo funzionamento         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-fnz-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Prezzo di vendita per campagna promozionale      *
      *    *-----------------------------------------------------------*
       pmt-prz-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Prezzo di vendita          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-prz-lst-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Prezzo su listino base                           *
      *    *-----------------------------------------------------------*
       pmt-bas-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      52                   to   v-pos                  .
           move      "Listino Base :"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-bas-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Categoria sconto in riga fattura *
      *    *-----------------------------------------------------------*
       pmt-cat-sco-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Categoria   di sconto      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cat-sco-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Percentuali di sconto                            *
      *    *-----------------------------------------------------------*
       pmt-per-sco-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Percentuali di sconto      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-per-sco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Categoria provvigionale          *
      *    *-----------------------------------------------------------*
       pmt-cat-pvg-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Nessuna Preferenza per il tipo funziona- *
      *                  * mento prezzi per campagna promozionale :    *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to pmt-cat-pvg-999.
       pmt-cat-pvg-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Categoria   provvigioni    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cat-pvg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : % di provvigioni                                 *
      *    *-----------------------------------------------------------*
       pmt-per-pvg-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Nessuna Preferenza per il tipo funziona- *
      *                  * mento prezzi per campagna promozionale :    *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to pmt-per-pvg-999.
       pmt-per-pvg-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompt                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Percentuali provvigioni    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-per-pvg-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Si/No storicizzazione valori precedenti          *
      *    *-----------------------------------------------------------*
       pmt-snx-sto-000.
      *              *-------------------------------------------------*
      *              * Linea 1                                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Storicizzazione dei valori :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea 2                                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                precedenti  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-sto-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Presa visione per pagina                   *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
      *              *-------------------------------------------------*
      *              * Se non esiste alcuna pagina attiva successiva   *
      *              * alla pagina attuale : uscita                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio numero pagina attuale           *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-npt    to   w-cnt-sts-imp-svp      .
       acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se all'ultima pagina : ripristino pagina    *
      *                  * salvata ed uscita                           *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-npt    not  < w-cnt-sts-imp-mpt
                     move  w-cnt-sts-imp-svp
                                          to   w-cnt-sts-imp-npt
                     go to acc-pre-vpg-999.
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina                    *
      *                  *---------------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
      *                  *---------------------------------------------*
      *                  * Test se pagina da trattare                  *
      *                  *---------------------------------------------*
           perform   snp-tes-reg-000      thru snp-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Se no : a re-incremento                     *
      *                  *---------------------------------------------*
           if        w-cnt-sts-imp-snp    not  = spaces
                     go to acc-pre-vpg-100.
      *                  *---------------------------------------------*
      *                  * Se si : ripristino pagina salvata e accet-  *
      *                  * tazione presa visione                       *
      *                  *---------------------------------------------*
           move      w-cnt-sts-imp-svp    to   w-cnt-sts-imp-npt      .
       acc-pre-vpg-200.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-alf                  .
           move      spaces               to   v-not                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-tes    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pre-vpg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pre-vpg-999.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pre-vpg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pre-vpg-000.
       acc-pre-vpg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data inizio campagna di vendita            *
      *    *-----------------------------------------------------------*
       acc-dva-ini-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dva-ini-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dva-ini (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dva-ini-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dva-ini-999.
       acc-dva-ini-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dva-ini (1)      .
       acc-dva-ini-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-dva-ini (1)    =    zero
                     go to acc-dva-ini-100.
       acc-dva-ini-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dva-ini-625.
      *                  *---------------------------------------------*
      *                  * Determinazione significativita' del campo   *
      *                  * 'Si/No storicizzazione valori precedenti' e *
      *                  * sua eventuale rivisualizzazione             *
      *                  *---------------------------------------------*
           perform   det-snx-sto-000      thru det-snx-sto-999        .
       acc-dva-ini-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dva-ini-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dva-ini-100.
       acc-dva-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data inizio campagna di vendita         *
      *    *-----------------------------------------------------------*
       vis-dva-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-dva-ini (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dva-ini-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data fine campagna di vendita              *
      *    *-----------------------------------------------------------*
       acc-dva-fin-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dva-fin-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ">"                  to   v-edm                  .
           move      07                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-dva-fin (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-dva-fin-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-dva-fin-999.
       acc-dva-fin-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-dva-fin (1)      .
       acc-dva-fin-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *                                   
      *                  *---------------------------------------------*
           if        w-tes-dva-fin (1)    not  = zero
                     go to acc-dva-fin-600.
           if        v-key                =    "UP  "
                     go to acc-dva-fin-600
           else      go to acc-dva-fin-100.
       acc-dva-fin-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dva-fin-625.
      *                  *---------------------------------------------*
      *                  * Determinazione significativita' del campo   *
      *                  * 'Si/No storicizzazione valori precedenti' e *
      *                  * sua eventuale rivisualizzazione             *
      *                  *---------------------------------------------*
           perform   det-snx-sto-000      thru det-snx-sto-999        .
       acc-dva-fin-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-dva-fin-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-dva-fin-100.
       acc-dva-fin-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data fine campagna di vendita           *
      *    *-----------------------------------------------------------*
       vis-dva-fin-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      w-tes-dva-fin (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dva-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo funzionamento                         *
      *    *-----------------------------------------------------------*
       acc-tip-fnz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-fnz-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Nessuna Preferenza per il tipo fun-  *
      *                      * zionamento prezzi per campagne di ven-  *
      *                      * dita : uscita                           *
      *                      *-----------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to acc-tip-fnz-999.
       acc-tip-fnz-050.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        w-tes-tip-fnz (1)    not  = zero
                     go to acc-tip-fnz-075.
           move      w-prs-tfu-pcp        to   w-tes-tip-fnz (1)      .
       acc-tip-fnz-075.
      *                  *---------------------------------------------*
      *                  * Fine pre-accettazione                       *
      *                  *---------------------------------------------*
           go to     acc-tip-fnz-100.
       acc-tip-fnz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-fun-lun    to   v-car                  .
           move      w-exp-tip-fun-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-fun-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-tip-fnz (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-fnz-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-fnz-999.
       acc-tip-fnz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-fnz (1)      .
       acc-tip-fnz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-tip-fnz (1)    not  = zero
                     go to acc-tip-fnz-500.
           if        v-key                =    "UP  "
                     go to acc-tip-fnz-600
           else      go to acc-tip-fnz-100.
       acc-tip-fnz-500.
      *                  *---------------------------------------------*
      *                  * Controllo se valore non a zero              *
      *                  *---------------------------------------------*
           if        w-tes-tip-fnz (1)    >    w-exp-tip-fun-num
                     go to acc-tip-fnz-100.
       acc-tip-fnz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-fnz-625.
      *                  *---------------------------------------------*
      *                  * Determinazione significativita' del campo   *
      *                  * 'Si/No storicizzazione valori precedenti' e *
      *                  * sua eventuale rivisualizzazione             *
      *                  *---------------------------------------------*
           perform   det-snx-sto-000      thru det-snx-sto-999        .
       acc-tip-fnz-650.
      *                  *---------------------------------------------*
      *                  * Preparazione flags di si/no utilizzo prezzo *
      *                  * sconti e provvigioni                        *
      *                  *---------------------------------------------*
           if        w-tes-tip-fnz (1)    =    01
                     move  "S"            to   w-tes-snx-prz (1)
                     move  "S"            to   w-tes-snx-sco (1)
                     move  "S"            to   w-tes-snx-pvg (1)
           else if   w-tes-tip-fnz (1)    =    02
                     move  "S"            to   w-tes-snx-prz (1)
                     move  "N"            to   w-tes-snx-sco (1)
                     move  "N"            to   w-tes-snx-pvg (1)
           else if   w-tes-tip-fnz (1)    =    03
                     move  "N"            to   w-tes-snx-prz (1)
                     move  "S"            to   w-tes-snx-sco (1)
                     move  "N"            to   w-tes-snx-pvg (1)
           else if   w-tes-tip-fnz (1)    =    04
                     move  "N"            to   w-tes-snx-prz (1)
                     move  "N"            to   w-tes-snx-sco (1)
                     move  "S"            to   w-tes-snx-pvg (1)
           else if   w-tes-tip-fnz (1)    =    05
                     move  "S"            to   w-tes-snx-prz (1)
                     move  "S"            to   w-tes-snx-sco (1)
                     move  "N"            to   w-tes-snx-pvg (1)
           else if   w-tes-tip-fnz (1)    =    06
                     move  "S"            to   w-tes-snx-prz (1)
                     move  "N"            to   w-tes-snx-sco (1)
                     move  "S"            to   w-tes-snx-pvg (1)
           else if   w-tes-tip-fnz (1)    =    07
                     move  "N"            to   w-tes-snx-prz (1)
                     move  "S"            to   w-tes-snx-sco (1)
                     move  "S"            to   w-tes-snx-pvg (1)
           else      move  "S"            to   w-tes-snx-prz (1)
                     move  "S"            to   w-tes-snx-sco (1)
                     move  "S"            to   w-tes-snx-pvg (1)      .
       acc-tip-fnz-700.
      *                  *---------------------------------------------*
      *                  * Normalizzazioni campi in conseguenza ai     *
      *                  * flags di si/no                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se prezzo a no                          *
      *                      *-----------------------------------------*
           if        w-tes-snx-prz (1)    not  = "N"
                     go to acc-tip-fnz-720.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      * prezzi                                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-prz-lst (1)      .
           move      zero                 to   w-tes-bas-lst (1)      .
           perform   vis-prz-lst-000      thru vis-prz-lst-999        .
           perform   vis-bas-lst-000      thru vis-bas-lst-999        .
       acc-tip-fnz-720.
      *                      *-----------------------------------------*
      *                      * Se sconti a no                          *
      *                      *-----------------------------------------*
           if        w-tes-snx-sco (1)    not  = "N"
                     go to acc-tip-fnz-740.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      * % di sconto                             *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-per-sco (1, 1)   .
           move      zero                 to   w-tes-per-sco (1, 2)   .
           move      zero                 to   w-tes-per-sco (1, 3)   .
           move      zero                 to   w-tes-per-sco (1, 4)   .
           move      zero                 to   w-tes-per-sco (1, 5)   .
           perform   vis-per-sco-000      thru vis-per-sco-999        .
       acc-tip-fnz-740.
      *                      *-----------------------------------------*
      *                      * Se provvigioni a no                     *
      *                      *-----------------------------------------*
           if        w-tes-snx-pvg (1)    not  = "N"
                     go to acc-tip-fnz-800.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione       *
      *                      * % di provvigioni                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-per-pvg (1, 1)   .
           move      zero                 to   w-tes-per-pvg (1, 2)   .
           move      zero                 to   w-tes-per-pvg (1, 3)   .
           perform   vis-per-pvg-000      thru vis-per-pvg-999        .
       acc-tip-fnz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-fnz-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-fnz-100.
       acc-tip-fnz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo funzionamento                      *
      *    *-----------------------------------------------------------*
       vis-tip-fnz-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Nessuna Preferenza per il tipo funziona- *
      *                  * mento prezzi per campagne di vendita : u-   *
      *                  * scita                                       *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to vis-tip-fnz-999.
       vis-tip-fnz-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione campo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-fun-lun    to   v-car                  .
           move      w-exp-tip-fun-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-fun-tbl    to   v-txt                  .
           move      w-tes-tip-fnz (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-fnz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Prezzo di vendita per campagna di vendita  *
      *    *-----------------------------------------------------------*
       acc-prz-lst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-prz-lst-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Nessuna Preferenza per il tipo fun-  *
      *                      * zionamento prezzi per campagna di ven-  *
      *                      * dita : ad accettazione                  *
      *                      *-----------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to acc-prz-lst-100.
      *                      *-----------------------------------------*
      *                      * Se Tipo funzionamento che non prevede   *
      *                      * il prezzo : uscita                      *
      *                      *-----------------------------------------*
           if        w-tes-snx-prz (1)    not  = "S"
                     go to acc-prz-lst-999.
       acc-prz-lst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           add       w-tes-num-pro-dec    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-prz-lst (1)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-prz-lst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-prz-lst-999.
       acc-prz-lst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-prz-lst (1)      .
       acc-prz-lst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-tes-prz-lst (1)    not  = zero
                     go to acc-prz-lst-600.
           if        v-key                =    "UP  "
                     go to acc-prz-lst-600
           else      go to acc-prz-lst-100.
       acc-prz-lst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-prz-lst-625.
      *                  *---------------------------------------------*
      *                  * Determinazione significativita' del campo   *
      *                  * 'Si/No storicizzazione valori precedenti' e *
      *                  * sua eventuale rivisualizzazione             *
      *                  *---------------------------------------------*
           perform   det-snx-sto-000      thru det-snx-sto-999        .
       acc-prz-lst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-prz-lst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-prz-lst-100.
       acc-prz-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Prezzo di vendita per campagna di ven-  *
      *    * dita                                                      *
      *    *-----------------------------------------------------------*
       vis-prz-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           add       w-tes-num-pro-dec    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-prz-lst (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-prz-lst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Prezzo di listino base                  *
      *    *-----------------------------------------------------------*
       vis-bas-lst-000.
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           add       w-tes-num-pro-dec    to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      w-tes-bas-lst (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-bas-lst-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Categoria sconto in riga     *
      *    *-----------------------------------------------------------*
       acc-cat-sco-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cat-sco-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zcs-003-ope      .
           move      w-tes-cat-sco (1)    to   w-cmn-zcs-003-cod      .
           move      13                   to   w-cmn-zcs-003-lin      .
           move      30                   to   w-cmn-zcs-003-pos      .
           move      13                   to   w-cmn-zcs-003-dln      .
           move      37                   to   w-cmn-zcs-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cmn-zcs-003-cll-000  thru cmn-zcs-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zcs-003-foi-000  thru cmn-zcs-003-foi-999    .
       acc-cat-sco-110.
           perform   cmn-zcs-003-cll-000  thru cmn-zcs-003-cll-999    .
           if        w-cmn-zcs-003-ope    =    "F+"
                     go to acc-cat-sco-115.
           if        w-cmn-zcs-003-ope    =    "AC"
                     go to acc-cat-sco-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cat-sco-115.
           perform   cmn-zcs-003-foi-000  thru cmn-zcs-003-foi-999    .
           go to     acc-cat-sco-110.
       acc-cat-sco-120.
           move      w-cmn-zcs-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cat-sco-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cat-sco-999.
       acc-cat-sco-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cat-sco (1)      .
       acc-cat-sco-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zcs-tip      .
           move      w-tes-cat-sco (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zcs-des    to   w-tes-cat-sco-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-sco-000      thru vis-des-sco-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zcs-flg    not  = spaces
                     go to acc-cat-sco-100.
       acc-cat-sco-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-sco-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cat-sco-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cat-sco-100.
       acc-cat-sco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Categoria sconto in riga  *
      *    *-----------------------------------------------------------*
       vis-cat-sco-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cat-sco (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-sco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione categoria     *
      *    *-----------------------------------------------------------*
       vis-des-sco-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cat-sco-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-sco-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Percentuali di sconto                      *
      *    *-----------------------------------------------------------*
       acc-per-sco-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-per-sco-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Nessuna Preferenza per il tipo fun-  *
      *                      * zionamento prezzi per campagne di ven-  *
      *                      * dita : ad accettazione                  *
      *                      *-----------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to acc-per-sco-050.
      *                      *-----------------------------------------*
      *                      * Se Tipo funzionamento che non prevede   *
      *                      * percentuali di sconto : uscita          *
      *                      *-----------------------------------------*
           if        w-tes-snx-sco (1)    not  = "S"
                     go to acc-per-sco-999.
       acc-per-sco-050.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..05           *
      *                  *---------------------------------------------*
           move      01                   to   w-cix-ctr-001          .
       acc-per-sco-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      14                   to   v-lin                  .
           move      w-cix-ctr-001        to   v-pos                  .
           if        w-cix-ctr-001        >    5
                     subtract  5          from v-pos                  .
           multiply  06                   by   v-pos                  .
           add       24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "TAB "               to   v-pfk (10)             .
           move      w-tes-per-sco
                    (1, w-cix-ctr-001)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-per-sco-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-per-sco-999.
       acc-per-sco-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-per-sco
                                              (1, w-cix-ctr-001)      .
       acc-per-sco-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-per-sco-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-per-sco-625.
      *                  *---------------------------------------------*
      *                  * Determinazione significativita' del campo   *
      *                  * 'Si/No storicizzazione valori precedenti' e *
      *                  * sua eventuale rivisualizzazione             *
      *                  *---------------------------------------------*
           perform   det-snx-sto-000      thru det-snx-sto-999        .
       acc-per-sco-650.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo la function key imposta- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-per-sco-660
           else if   v-key                =    "DOWN"
                     go to acc-per-sco-670
           else if   v-key                =    "TAB "
                     go to acc-per-sco-700
           else if   v-key                =    "DO  "
                     go to acc-per-sco-800
           else      go to acc-per-sco-670.
       acc-per-sco-660.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        w-cix-ctr-001        =    1
                     go to acc-per-sco-999.
           subtract  1                    from w-cix-ctr-001          .
           go to     acc-per-sco-100.
       acc-per-sco-670.
      *                  *---------------------------------------------*
      *                  * Se Down o Return                            *
      *                  *---------------------------------------------*
           go to     acc-per-sco-999.
       acc-per-sco-700.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        w-cix-ctr-001        =    05
                     go to acc-per-sco-999.
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-cix-ctr-001          .
           go to     acc-per-sco-100.
       acc-per-sco-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-per-sco-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-per-sco-100.
       acc-per-sco-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Percentuali di sconto                   *
      *    *-----------------------------------------------------------*
       vis-per-sco-000.
           move      spaces               to   w-edt-per-sco          .
           move      zero                 to   w-cix-ctr-001          .
       vis-per-sco-200.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    5
                     go to vis-per-sco-500.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-tes-per-sco 
                    (1, w-cix-ctr-001)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-psr-ele
                                              (w-cix-ctr-001)         .
           go to     vis-per-sco-200.
       vis-per-sco-500.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-per-sco        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-per-sco-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Categoria provvigioni        *
      *    *-----------------------------------------------------------*
       acc-cat-pvg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Nessuna Preferenza per il tipo fun-  *
      *                      * zionamento prezzi per campagne di ven-  *
      *                      * dita : uscita                           *
      *                      *-----------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to acc-cat-pvg-999.
      *                      *-----------------------------------------*
      *                      * Se Tipo funzionamento che non prevede   *
      *                      * percentuali di provvigione : uscita     *
      *                      *-----------------------------------------*
           if        w-tes-snx-pvg (1)    not  = "S"
                     go to acc-cat-pvg-999.
       acc-cat-pvg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zpv-002-ope      .
           move      w-tes-cat-pvg (1)    to   w-cmn-zpv-002-cod      .
           move      16                   to   w-cmn-zpv-002-lin      .
           move      30                   to   w-cmn-zpv-002-pos      .
           move      16                   to   w-cmn-zpv-002-dln      .
           move      37                   to   w-cmn-zpv-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cmn-zpv-002-cll-000  thru cmn-zpv-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zpv-002-foi-000  thru cmn-zpv-002-foi-999    .
       acc-cat-pvg-110.
           perform   cmn-zpv-002-cll-000  thru cmn-zpv-002-cll-999    .
           if        w-cmn-zpv-002-ope    =    "F+"
                     go to acc-cat-pvg-115.
           if        w-cmn-zpv-002-ope    =    "AC"
                     go to acc-cat-pvg-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cat-pvg-115.
           perform   cmn-zpv-002-foi-000  thru cmn-zpv-002-foi-999    .
           go to     acc-cat-pvg-110.
       acc-cat-pvg-120.
           move      w-cmn-zpv-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cat-pvg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cat-pvg-999.
       acc-cat-pvg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cat-pvg (1)      .
       acc-cat-pvg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zpv-tip      .
           move      w-tes-cat-pvg (1)    to   w-let-arc-zpv-cod      .
           perform   let-arc-zpv-000      thru let-arc-zpv-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zpv-des    to   w-tes-cat-pvg-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cpv-000      thru vis-des-cpv-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zpv-flg    not  = spaces
                     go to acc-cat-pvg-100.
       acc-cat-pvg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cat-pvg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cat-pvg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cat-pvg-100.
       acc-cat-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Categoria provvigioni     *
      *    *-----------------------------------------------------------*
       vis-cat-pvg-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Nessuna Preferenza per il tipo funziona- *
      *                  * mento prezzi per campagna promozionale :    *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to vis-cat-pvg-999.
       vis-cat-pvg-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione campo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cat-pvg (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cat-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione categoria     *
      *    * provvigioni                                               *
      *    *-----------------------------------------------------------*
       vis-des-cpv-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Nessuna Preferenza per il tipo funziona- *
      *                  * mento prezzi per campagna promozionale :    *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to vis-des-cpv-999.
       vis-des-cpv-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione campo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cat-pvg-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cpv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Percentuali di provvigione                 *
      *    *-----------------------------------------------------------*
       acc-per-pvg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Nessuna Preferenza per il tipo fun-  *
      *                      * zionamento prezzi per campagne di ven-  *
      *                      * dita : uscita                           *
      *                      *-----------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to acc-per-pvg-999.
      *                      *-----------------------------------------*
      *                      * Se Tipo funzionamento che non prevede   *
      *                      * percentuali di provvigione : uscita     *
      *                      *-----------------------------------------*
           if        w-tes-snx-pvg (1)    not  = "S"
                     go to acc-per-pvg-999.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01.03            *
      *                  *---------------------------------------------*
           move      01                   to   w-cix-ctr-001          .
       acc-per-pvg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      17                   to   v-lin                  .
           move      w-cix-ctr-001        to   v-pos                  .
           if        w-cix-ctr-001        >    3
                     subtract  3          from v-pos                  .
           multiply  06                   by   v-pos                  .
           add       24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "TAB "               to   v-pfk (10)             .
           move      w-tes-per-pvg
                    (1, w-cix-ctr-001)    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-per-pvg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-per-pvg-999.
       acc-per-pvg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-per-pvg
                                              (1, w-cix-ctr-001)      .
       acc-per-pvg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-per-pvg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-per-sco-625.
      *                  *---------------------------------------------*
      *                  * Determinazione significativita' del campo   *
      *                  * 'Si/No storicizzazione valori precedenti' e *
      *                  * sua eventuale rivisualizzazione             *
      *                  *---------------------------------------------*
           perform   det-snx-sto-000      thru det-snx-sto-999        .
       acc-per-pvg-650.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo la function key imposta- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-per-pvg-660
           else if   v-key                =    "DOWN"
                     go to acc-per-pvg-670
           else if   v-key                =    "TAB "
                     go to acc-per-pvg-700
           else if   v-key                =    "DO  "
                     go to acc-per-pvg-800
           else      go to acc-per-pvg-670.
       acc-per-pvg-660.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        w-cix-ctr-001        =    1
                     go to acc-per-pvg-999.
           subtract  1                    from w-cix-ctr-001          .
           go to     acc-per-pvg-100.
       acc-per-pvg-670.
      *                  *---------------------------------------------*
      *                  * Se Down o Return                            *
      *                  *---------------------------------------------*
           go to     acc-per-pvg-999.
       acc-per-pvg-700.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        w-cix-ctr-001        =    03
                     go to acc-per-pvg-999.
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-cix-ctr-001          .
           go to     acc-per-pvg-100.
       acc-per-pvg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-per-pvg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-per-pvg-100.
       acc-per-pvg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Percentuali di provvigione              *
      *    *-----------------------------------------------------------*
       vis-per-pvg-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Nessuna Preferenza per il tipo funziona- *
      *                  * mento prezzi per campagna promozionale :    *
      *                  * uscita                                      *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to vis-per-pvg-999.
       vis-per-pvg-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione campo                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-edt-per-pvg          .
           move      zero                 to   w-cix-ctr-001          .
       vis-per-pvg-200.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    03
                     go to vis-per-pvg-500.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      w-tes-per-pvg 
                    (1, w-cix-ctr-001)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-ppv-ele
                                              (w-cix-ctr-001)         .
           go to     vis-per-pvg-200.
       vis-per-pvg-500.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-per-pvg        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-per-pvg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione significativita' del campo 'Si/No stori-   *
      *    * cizzazione valori precedenti', e sua visualizzazione se   *
      *    * variato                                                   *
      *    *-----------------------------------------------------------*
       det-snx-sto-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valore attuale del campo            *
      *              *-------------------------------------------------*
           move      w-tes-snx-sto (1)    to   w-sav-det-snx-sto      .
       det-snx-sto-100.
      *              *-------------------------------------------------*
      *              * Se non si e' in modifica : ad uscita senza al-  *
      *              * cuna azione                                     *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "M"
                     go to det-snx-sto-900.
       det-snx-sto-200.
      *              *-------------------------------------------------*
      *              * Test se il campo puo' essere significativo, e   *
      *              * deviazione secondo l'esito                      *
      *              *-------------------------------------------------*
       det-snx-sto-210.
      *                  *---------------------------------------------*
      *                  * Test su data di validita' iniziale          *
      *                  *---------------------------------------------*
           if        w-tes-dva-ini (1)    not  = w-tes-dva-ini (2)
                     go to det-snx-sto-600.
       det-snx-sto-500.
      *              *-------------------------------------------------*
      *              * Se il valore non puo' essere significativo      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura del valore a spaces               *
      *                  *---------------------------------------------*
           move      spaces               to   w-tes-snx-sto (1)      .
      *                  *---------------------------------------------*
      *                  * Ad eventuale ri-visualizzazione             *
      *                  *---------------------------------------------*
           go to     det-snx-sto-700.
       det-snx-sto-600.
      *              *-------------------------------------------------*
      *              * Se il valore puo' essere significativo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' indefinito si forza *
      *                  * il valore a 'No'                            *
      *                  *---------------------------------------------*
           if        w-tes-snx-sto (1)    not  = "S" and
                     w-tes-snx-sto (1)    not  = "N"
                     move  "N"            to   w-tes-snx-sto (1)      .
      *                  *---------------------------------------------*
      *                  * Ad eventuale ri-visualizzazione             *
      *                  *---------------------------------------------*
           go to     det-snx-sto-700.
       det-snx-sto-700.
      *              *-------------------------------------------------*
      *              * Se il valore e' inalterato rispetto al valore   *
      *              * salvato all'inizio : ad  uscita                 *
      *              *-------------------------------------------------*
           if        w-tes-snx-sto (1)    =    w-sav-det-snx-sto
                     go to det-snx-sto-900.
       det-snx-sto-800.
      *              *-------------------------------------------------*
      *              * Ri-visualizzazione campo                        *
      *              *-------------------------------------------------*
           perform   vis-snx-sto-000      thru vis-snx-sto-999        .
       det-snx-sto-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-snx-sto-999.
       det-snx-sto-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Si/No storicizzazione valori precedenti    *
      *    *-----------------------------------------------------------*
       acc-snx-sto-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-sto-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il valore attuale del campo e' inde- *
      *                      * terminato : uscita                      *
      *                      * dita : uscita                           *
      *                      *-----------------------------------------*
           if        w-tes-snx-sto (1)    not  = "S" and
                     w-tes-snx-sto (1)    not  = "N"
                     go to acc-snx-sto-999.
       acc-snx-sto-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-sto-lun    to   v-car                  .
           move      w-exp-snx-sto-num    to   v-ldt                  .
           move      "X"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-sto-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-tes-snx-sto (1)     =    "N"
                     move  01             to   v-num
           else if   w-tes-snx-sto (1)    =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-sto-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-sto-999.
       acc-snx-sto-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-tes-snx-sto (1)
           else if   v-num                =    02
                     move  "S"            to   w-tes-snx-sto (1)
           else      move  spaces         to   w-tes-snx-sto (1)      .
       acc-snx-sto-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-tes-snx-sto (1)    not  = "S" and
                     w-tes-snx-sto (1)    not  = "N"
                     go to acc-snx-sto-100.
       acc-snx-sto-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-sto-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-sto-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-sto-100.
       acc-snx-sto-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Si/No storicizzazione valori precedenti *
      *    *-----------------------------------------------------------*
       vis-snx-sto-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-sto-lun    to   v-car                  .
           move      w-exp-snx-sto-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-snx-sto-tbl    to   v-txt                  .
           if        w-tes-snx-sto (1)    =    "N"
                     move  01             to   v-num
           else if   w-tes-snx-sto (1)    =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-sto-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi chiave           *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-999.
           exit.
           
      *    *===========================================================*
      *    * Controllo se chiave vuota                                 *
      *    *-----------------------------------------------------------*
       cnt-key-vuo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-key-vuo-flg      .
      *              *-------------------------------------------------*
      *              * Test                                            *
      *              *-------------------------------------------------*
           if        w-tes-num-pro        =    zero
                     move  "#"            to   w-cnt-key-vuo-flg      .
       cnt-key-vuo-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do campi non chiave       *
      *    *-----------------------------------------------------------*
       cnt-tdo-nok-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-nok-flg      .
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controlli formali                               *
      *              *-------------------------------------------------*
       cnt-tdo-nok-110.
      *                  *---------------------------------------------*
      *                  * Data inizio campagna                        *
      *                  *---------------------------------------------*
           if        w-tes-dva-ini (1)    not  = zero
                     go to cnt-tdo-nok-120.
           move      "Manca la data di inizio campagna promozionale     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-120.
      *                  *---------------------------------------------*
      *                  * Data fine campagna                          *
      *                  *---------------------------------------------*
           if        w-tes-dva-fin (1)    not  = zero
                     go to cnt-tdo-nok-130.
           move      "Manca la data di fine campagna promozionale       
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-130.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     go to cnt-tdo-nok-140.
           if        w-tes-tip-fnz (1)    not  = zero
                     go to cnt-tdo-nok-140.
           move      "Manca la definizione del tipo funzionamento       
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-140.
      *                  *---------------------------------------------*
      *                  * Prezzo di vendita per campagna promozionale *
      *                  *---------------------------------------------*
           if        w-tes-snx-prz (1)    not  = "S"
                     go to cnt-tdo-nok-150.
           if        w-tes-prz-lst (1)    not  = zero
                     go to cnt-tdo-nok-150.
           move      "Manca il prezzo di vendita per campagna promoziona
      -              "le             "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *                  *---------------------------------------------*
      *                  * Sconti per campagna promozionale            *
      *                  *---------------------------------------------*
       cnt-tdo-nok-160.
      *                  *---------------------------------------------*
      *                  * Provvigioni per campagna promozionale       *
      *                  *---------------------------------------------*
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo di congruenza tra data di inizio e    *
      *              * data di fine campagna                           *
      *              *-------------------------------------------------*
           if        w-tes-dva-ini (1)    not  > w-tes-dva-fin (1)
                     go to cnt-tdo-nok-300.
           move      "La data di inizio campagna di vendita non puo' ess
      -              "ere superiore a"    to   w-err-box-err-msg      .
           move      "quella di fine campagna                           
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo di congruenza tra la campagna attuale *
      *              * e l'ultima campagna storicizzata                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [lsd]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODFR "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      w-tes-num-pro        to   rf-lsd-num-pro         .
           move      zero                 to   rf-lsd-dvf-rev         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : nessun controllo          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-400.
      *                  *---------------------------------------------*
      *                  * Lettura primo record [lsd]                  *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * Se At End : nessun controllo                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-400.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-lsd-tip-rec       not  = 03            or
                     rf-lsd-cod-lst       not  = spaces        or
                     rf-lsd-cod-cli       not  = zero          or
                     rf-lsd-sgl-vlt       not  = c-sgl         or
                     rf-lsd-num-pro       not  = w-tes-num-pro
                     go to cnt-tdo-nok-400.
      *                  *---------------------------------------------*
      *                  * Confronto fra data inizio campagna attuale  *
      *                  * con data di fine campagna ultima storiciz-  *
      *                  * zat                                         *
      *                  *---------------------------------------------*
           if        rf-lsd-dva-fin       <    w-tes-dva-ini (1)
                     go to cnt-tdo-nok-400.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "La data di inizio campagna di vendita deve essere 
      -              "superiore a    "    to   w-err-box-err-msg      .
           move      "quella di fine campagna dell'ultima campagna stori
      -              "cizzata !      "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * Ad errore                                   *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Controllo di congruenza tra la campagna attuale *
      *              * e la campagna da storicizzare                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non richiesta la storicizzazione : nes-  *
      *                  * sun controllo                               *
      *                  *---------------------------------------------*
           if        w-tes-snx-sto (1)    not  = "S"
                     go to cnt-tdo-nok-500.
      *                  *---------------------------------------------*
      *                  * Confronto fra data inizio campagna, valore  *
      *                  * attuale, con data di fine campagna, valore  *
      *                  * precedente                                  *
      *                  *---------------------------------------------*
           if        w-tes-dva-ini (2)    <    w-tes-dva-ini (1)
                     go to cnt-tdo-nok-500.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "La data di inizio campagna di vendita attuale deve
      -              " essere        "    to   w-err-box-err-msg      .
           move      "superiore alla data di fine campagna precedente !
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * Ad errore                                   *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Controllo di congruenza tra la campagna prece-  *
      *              * dente da storicizzare e l'ultima campagna sto-  *
      *              * ricizzata                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non richiesta la storicizzazione : nes-  *
      *                  * sun controllo                               *
      *                  *---------------------------------------------*
           if        w-tes-snx-sto (1)    not  = "S"
                     go to cnt-tdo-nok-600.
      *                  *---------------------------------------------*
      *                  * Start su file [lsd]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODFR "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      w-tes-num-pro        to   rf-lsd-num-pro         .
           move      zero                 to   rf-lsd-dvf-rev         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : nessun controllo          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-600.
      *                  *---------------------------------------------*
      *                  * Lettura primo record [lsd]                  *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                  *---------------------------------------------*
      *                  * Se At End : nessun controllo                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cnt-tdo-nok-600.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-lsd-tip-rec       not  = 03            or
                     rf-lsd-cod-lst       not  = spaces        or
                     rf-lsd-cod-cli       not  = zero          or
                     rf-lsd-sgl-vlt       not  = c-sgl         or
                     rf-lsd-num-pro       not  = w-tes-num-pro
                     go to cnt-tdo-nok-600.
      *                  *---------------------------------------------*
      *                  * Confronto fra data inizio campagna prece-   *
      *                  * dente da storicizzare con data di fine cam- *
      *                  * pagna ultima storicizzata                   *
      *                  *---------------------------------------------*
           if        rf-lsd-dva-fin       <    w-tes-dva-ini (2)
                     go to cnt-tdo-nok-600.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "La data di inizio della campagna da storicizzare d
      -              "eve essere     "    to   w-err-box-err-msg      .
           move      "superirore a quella di fine campagna ultima storic
      -              "izzata !       "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * Ad errore                                   *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Fine controlli : uscita                         *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-nok-flg      .
       cnt-tdo-nok-999.
           exit.
           
      *    *===========================================================*
      *    * Normalizzazione dati chiave e non chiave                  *
      *    *-----------------------------------------------------------*
       nor-key-nok-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati chiave                     *
      *              *-------------------------------------------------*
           perform   nor-key-reg-000      thru nor-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione dati non chiave testata         *
      *              *-------------------------------------------------*
           perform   nor-nok-tes-000      thru nor-nok-tes-999        .
       nor-key-nok-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati chiave                               *
      *    *-----------------------------------------------------------*
       nor-key-reg-000.
           move      zero                 to   w-tes-tip-rec          .
           move      zero                 to   w-tes-num-pro          .
           move      spaces               to   w-tes-alf-pro          .
           move      spaces               to   w-tes-num-pro-des      .
           move      zero                 to   w-tes-num-pro-prz      .
           move      zero                 to   w-tes-num-pro-dec      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      zero                 to   w-tes-tip-fnz (1)      .
           move      zero                 to   w-tes-bas-lst (1)      .
           move      zero                 to   w-tes-prz-lst (1)      .
           move      zero                 to   w-tes-cat-pvg (1)      .
           move      spaces               to   w-tes-cat-pvg-des (1)  .
           move      zero                 to   w-tes-per-pvg (1, 1)   .
           move      zero                 to   w-tes-per-pvg (1, 2)   .
           move      zero                 to   w-tes-per-pvg (1, 3)   .
           move      zero                 to   w-tes-cat-sco (1)      .
           move      spaces               to   w-tes-cat-sco-des (1)  .
           move      zero                 to   w-tes-per-sco (1, 1)   .
           move      zero                 to   w-tes-per-sco (1, 2)   .
           move      zero                 to   w-tes-per-sco (1, 3)   .
           move      zero                 to   w-tes-per-sco (1, 4)   .
           move      zero                 to   w-tes-per-sco (1, 5)   .
           move      spaces               to   w-tes-snx-prz (1)      .
           move      spaces               to   w-tes-snx-sco (1)      .
           move      spaces               to   w-tes-snx-pvg (1)      .
           move      zero                 to   w-tes-dva-ini (1)      .
           move      zero                 to   w-tes-dva-fin (1)      .
           move      spaces               to   w-tes-snx-sto (1)      .
           move      spaces               to   w-tes-alx-exp (1)      .
       nor-nok-tes-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *-----------------------------------------------------------*
       rou-let-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-let-reg      .
       rou-let-reg-025.
      *              *-------------------------------------------------*
      *              * Lettura record [lst]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      w-tes-num-pro        to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *              *-------------------------------------------------*
      *              * Se anagrafica non trovata                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se inserimento consentito              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-060.
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                      *-----------------------------------------*
      *                      * Messaggio                               *
      *                      *-----------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                      *-----------------------------------------*
      *                      * Box di errore                           *
      *                      *-----------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-060.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Inserimento            *
      *                  *---------------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-100.
      *              *-------------------------------------------------*
      *              * Se anagrafica trovata                           *
      *              *-------------------------------------------------*
       rou-let-reg-125.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Modifica               *
      *                  *---------------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
       rou-let-reg-150.
      *                  *---------------------------------------------*
      *                  * Determinazione valori attuali               *
      *                  *---------------------------------------------*
       rou-let-reg-175.
      *                      *-----------------------------------------*
      *                      * Valori contenuti direttamente in record *
      *                      * [lst]                                   *
      *                      *-----------------------------------------*
           move      rf-lst-prz-lst       to   w-tes-prz-lst (1)      .
           move      rf-lst-cat-pvg       to   w-tes-cat-pvg (1)      .
           move      rf-lst-per-pvg (1)   to   w-tes-per-pvg (1, 1)   .
           move      rf-lst-per-pvg (2)   to   w-tes-per-pvg (1, 2)   .
           move      rf-lst-per-pvg (3)   to   w-tes-per-pvg (1, 3)   .
           move      rf-lst-cat-sco       to   w-tes-cat-sco (1)      .
           move      rf-lst-per-sco (1)   to   w-tes-per-sco (1, 1)   .
           move      rf-lst-per-sco (2)   to   w-tes-per-sco (1, 2)   .
           move      rf-lst-per-sco (3)   to   w-tes-per-sco (1, 3)   .
           move      rf-lst-per-sco (4)   to   w-tes-per-sco (1, 4)   .
           move      rf-lst-per-sco (5)   to   w-tes-per-sco (1, 5)   .
           move      rf-lst-snx-prz       to   w-tes-snx-prz (1)      .
           move      rf-lst-snx-pvg       to   w-tes-snx-pvg (1)      .
           move      rf-lst-snx-sco       to   w-tes-snx-sco (1)      .
           move      rf-lst-dva-ini       to   w-tes-dva-ini (1)      .
           move      rf-lst-dva-fin       to   w-tes-dva-fin (1)      .
           move      rf-lst-alx-exp       to   w-tes-alx-exp (1)      .
      *                      *-----------------------------------------*
      *                      * Valori contenuti indirettamente in      *
      *                      * record [lst]                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [zcs]              *
      *                          *-------------------------------------*
           move      03                   to   w-let-arc-zcs-tip      .
           move      w-tes-cat-sco (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
           move      w-let-arc-zcs-des    to   w-tes-cat-sco-des (1)  .
      *                          *-------------------------------------*
      *                          * Lettura archivio [zpv]              *
      *                          *-------------------------------------*
           move      02                   to   w-let-arc-zpv-tip      .
           move      w-tes-cat-pvg (1)    to   w-let-arc-zpv-cod      .
           perform   let-arc-zpv-000      thru let-arc-zpv-999        .
           move      w-let-arc-zpv-des    to   w-tes-cat-pvg-des (1)  .
       rou-let-reg-200.
      *                      *-----------------------------------------*
      *                      * Preparazione tipo funzionamento         *
      *                      *-----------------------------------------*
           if        w-tes-snx-prz (1)    =    "S"  and
                     w-tes-snx-sco (1)    =    "S"  and
                     w-tes-snx-pvg (1)    =    "S"
                     move  01             to   w-tes-tip-fnz (1)
           else if   w-tes-snx-prz (1)    =    "S"  and
                     w-tes-snx-sco (1)    =    "N"  and
                     w-tes-snx-pvg (1)    =    "N"
                     move  02             to   w-tes-tip-fnz (1)
           else if   w-tes-snx-prz (1)    =    "N"  and
                     w-tes-snx-sco (1)    =    "S"  and
                     w-tes-snx-pvg (1)    =    "N"
                     move  03             to   w-tes-tip-fnz (1)
           else if   w-tes-snx-prz (1)    =    "N"  and
                     w-tes-snx-sco (1)    =    "N"  and
                     w-tes-snx-pvg (1)    =    "S"
                     move  04             to   w-tes-tip-fnz (1)
           else if   w-tes-snx-prz (1)    =    "S"  and
                     w-tes-snx-sco (1)    =    "S"  and
                     w-tes-snx-pvg (1)    =    "N"
                     move  05             to   w-tes-tip-fnz (1)
           else if   w-tes-snx-prz (1)    =    "S"  and
                     w-tes-snx-sco (1)    =    "N"  and
                     w-tes-snx-pvg (1)    =    "S"
                     move  06             to   w-tes-tip-fnz (1)
           else if   w-tes-snx-prz (1)    =    "N"  and
                     w-tes-snx-sco (1)    =    "S"  and
                     w-tes-snx-pvg (1)    =    "S"
                     move  07             to   w-tes-tip-fnz (1)
           else      move  zero           to   w-tes-tip-fnz (1)      .
       rou-let-reg-225.
      *                  *---------------------------------------------*
      *                  * Valori precedenti anagrafica                *
      *                  *---------------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
       rou-let-reg-300.
      *                  *---------------------------------------------*
      *                  * Test se solo Visualizzazione                *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
       rou-let-reg-400.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per inserimento                  *
      *    *-----------------------------------------------------------*
       pre-acc-ins-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-ins      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt e valore Listino Base    *
      *              *-------------------------------------------------*
           perform   pmt-bas-lst-000      thru pmt-bas-lst-999        .
           perform   vis-bas-lst-000      thru vis-bas-lst-999        .
       pre-acc-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per modifica                     *
      *    *-----------------------------------------------------------*
       pre-acc-mod-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-mod      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt e valore Listino Base    *
      *              *-------------------------------------------------*
           perform   pmt-bas-lst-000      thru pmt-bas-lst-999        .
           perform   vis-bas-lst-000      thru vis-bas-lst-999        .
       pre-acc-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-accettazioni per visualizzazione              *
      *    *-----------------------------------------------------------*
       pre-acc-vis-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-acc-vis      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt e valore Listino Base    *
      *              *-------------------------------------------------*
           perform   pmt-bas-lst-000      thru pmt-bas-lst-999        .
           perform   vis-bas-lst-000      thru vis-bas-lst-999        .
       pre-acc-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-richiesta di ratifica tasto Delete            *
      *    *-----------------------------------------------------------*
       pre-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-snx-del      .
       pre-snx-del-999.
           exit.

      *    *===========================================================*
      *    * Routine post-richiesta di ratifica tasto Delete           *
      *    *-----------------------------------------------------------*
       pos-snx-del-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pos-snx-del      .
       pos-snx-del-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su inserimento                          *
      *    *-----------------------------------------------------------*
       pos-exi-ins-000.
       pos-exi-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su modifica                             *
      *    *-----------------------------------------------------------*
       pos-exi-mod-000.
       pos-exi-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su visualizzazione                      *
      *    *-----------------------------------------------------------*
       pos-exi-vis-000.
       pos-exi-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di inserimento                      *
      *    *-----------------------------------------------------------*
       pos-cnf-ins-000.
      *              *-------------------------------------------------*
      *              * Scrittura movimento su files                    *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
      *              *-------------------------------------------------*
      *              * Scrittura movimento su files                    *
      *              *-------------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
      *              *-------------------------------------------------*
      *              * Delete movimento da files                       *
      *              *-------------------------------------------------*
           perform   del-mov-fil-000      thru del-mov-fil-999        .
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [lst]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [lst]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-lst-000      thru wrt-rec-lst-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [lst]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-lst-000      thru rew-rec-lst-999        .
      *                      *-----------------------------------------*
      *                      * Write record [lsd], solo se richiesta   *
      *                      * la storicizzazione                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-lsd-000      thru wrt-rec-lsd-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [lst]                             *
      *              *-------------------------------------------------*
           perform   del-rec-lst-000      thru del-rec-lst-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [lst]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-lst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       cmp-rec-lst-200.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      c-dec                to   rf-lst-dec-vlt         .
           move      w-tes-num-pro        to   rf-lst-num-pro         .
           move      w-tes-prz-lst (1)    to   rf-lst-prz-lst         .
           move      w-tes-cat-pvg (1)    to   rf-lst-cat-pvg         .
           move      w-tes-per-pvg (1, 1) to   rf-lst-per-pvg (1)     .
           move      w-tes-per-pvg (1, 2) to   rf-lst-per-pvg (2)     .
           move      w-tes-per-pvg (1, 3) to   rf-lst-per-pvg (3)     .
           move      w-tes-cat-sco (1)    to   rf-lst-cat-sco         .
           move      w-tes-per-sco (1, 1) to   rf-lst-per-sco (1)     .
           move      w-tes-per-sco (1, 2) to   rf-lst-per-sco (2)     .
           move      w-tes-per-sco (1, 3) to   rf-lst-per-sco (3)     .
           move      w-tes-per-sco (1, 4) to   rf-lst-per-sco (4)     .
           move      w-tes-per-sco (1, 5) to   rf-lst-per-sco (5)     .
           move      w-tes-snx-prz (1)    to   rf-lst-snx-prz         .
           move      w-tes-snx-pvg (1)    to   rf-lst-snx-pvg         .
           move      w-tes-snx-sco (1)    to   rf-lst-snx-sco         .
           move      w-tes-dva-ini (1)    to   rf-lst-dva-ini         .
           move      w-tes-dva-fin (1)    to   rf-lst-dva-fin         .
           move      w-tes-alx-exp (1)    to   rf-lst-alx-exp         .
       cmp-rec-lst-350.
      *                  *---------------------------------------------*
      *                  * Normalizzazione in caso di Nessuna Prefe-   *
      *                  * renza per il tipo funzionamento prezzi per  *
      *                  * campagna di vendita                         *
      *                  *---------------------------------------------*
           if        w-prs-tfu-pcp        =    zero
                     move  "S"            to   rf-lst-snx-prz
                     move  "N"            to   rf-lst-snx-pvg
                     move  "S"            to   rf-lst-snx-sco
                     move  zero           to   rf-lst-per-pvg (1)
                     move  zero           to   rf-lst-per-pvg (2)
                     move  zero           to   rf-lst-per-pvg (3)     .
       cmp-rec-lst-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [lst]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-lst-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-lst-000      thru cmp-rec-lst-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       wrt-rec-lst-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [lst]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-lst-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-lst-000      thru cmp-rec-lst-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       rew-rec-lst-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [lst]                                *
      *    *-----------------------------------------------------------*
       del-rec-lst-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-lst-000      thru cmp-rec-lst-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       del-rec-lst-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [lsd], solo se richiesta la storicizza-  *
      *    * zione                                                     *
      *    *-----------------------------------------------------------*
       wrt-rec-lsd-000.
      *              *-------------------------------------------------*
      *              * Se non e' stata richiesta la storicizzazione :  *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           if        w-tes-snx-sto (1)    not  = "S"
                     go to wrt-rec-lsd-999.
       wrt-rec-lsd-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       wrt-rec-lsd-200.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      03                   to   rf-lsd-tip-rec         .
      *                  *---------------------------------------------*
      *                  * Codice listino alternativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   rf-lsd-cod-lst         .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      zero                 to   rf-lsd-cod-cli         .
      *                  *---------------------------------------------*
      *                  * Sigla della valuta                          *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-lsd-sgl-vlt         .
      *                  *---------------------------------------------*
      *                  * Numero decimali della valuta                *
      *                  *---------------------------------------------*
           move      c-dec                to   rf-lsd-dec-vlt         .
      *                  *---------------------------------------------*
      *                  * Codice prodotto numerico                    *
      *                  *---------------------------------------------*
           move      w-tes-num-pro        to   rf-lsd-num-pro         .
      *                  *---------------------------------------------*
      *                  * Prezzo                                      *
      *                  *---------------------------------------------*
           move      w-tes-prz-lst (2)    to   rf-lsd-prz-lst         .
      *                  *---------------------------------------------*
      *                  * Categoria provvigioni                       *
      *                  *---------------------------------------------*
           move      w-tes-cat-pvg (1)    to   rf-lsd-cat-pvg         .
      *                  *---------------------------------------------*
      *                  * % di provvigione associate direttamente al  *
      *                  * listino                                     *
      *                  *---------------------------------------------*
           move      w-tes-per-pvg (2, 1) to   rf-lsd-per-pvg (1)     .
           move      w-tes-per-pvg (2, 2) to   rf-lsd-per-pvg (2)     .
           move      w-tes-per-pvg (2, 3) to   rf-lsd-per-pvg (3)     .
      *                  *---------------------------------------------*
      *                  * Categoria sconto                            *
      *                  *---------------------------------------------*
           move      w-tes-cat-sco (1)    to   rf-lsd-cat-sco         .
      *                  *---------------------------------------------*
      *                  * % di sconto associate direttamente al li-   *
      *                  * stino                                       *
      *                  *---------------------------------------------*
           move      w-tes-per-sco (2, 1) to   rf-lsd-per-sco (1)     .
           move      w-tes-per-sco (2, 2) to   rf-lsd-per-sco (2)     .
           move      w-tes-per-sco (2, 3) to   rf-lsd-per-sco (3)     .
           move      w-tes-per-sco (2, 4) to   rf-lsd-per-sco (4)     .
           move      w-tes-per-sco (2, 5) to   rf-lsd-per-sco (5)     .
      *                  *---------------------------------------------*
      *                  * Si/no utilizzo del prezzo                   *
      *                  *---------------------------------------------*
           move      w-tes-snx-prz (2)    to   rf-lsd-snx-prz         .
      *                  *---------------------------------------------*
      *                  * Si/no utilizzo delle % di provvigione       *
      *                  *---------------------------------------------*
           move      w-tes-snx-pvg (2)    to   rf-lsd-snx-pvg         .
      *                  *---------------------------------------------*
      *                  * Si/no utilizzo delle % di sconto            *
      *                  *---------------------------------------------*
           move      w-tes-snx-sco (2)    to   rf-lsd-snx-sco         .
      *                  *---------------------------------------------*
      *                  * Data di validita' iniziale                  *
      *                  *---------------------------------------------*
           move      w-tes-dva-ini (2)    to   rf-lsd-dva-ini         .
      *                  *---------------------------------------------*
      *                  * Data di validita' finale                    *
      *                  *---------------------------------------------*
           move      w-tes-dva-fin (2)    to   rf-lsd-dva-fin         .
      *                  *---------------------------------------------*
      *                  * Data di validita' finale in formato 're-    *
      *                  * verse'                                      *
      *                  *---------------------------------------------*
           move      99999999             to   rf-lsd-dvf-rev         .
           if        rf-lsd-dva-fin       not  = zero
                     subtract   19000000  from rf-lsd-dvf-rev
                     subtract   rf-lsd-dva-fin
                                          from rf-lsd-dvf-rev         .
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni speciali         *
      *                  *---------------------------------------------*
           move      w-tes-alx-exp (2)    to   rf-lsd-alx-exp         .
       wrt-rec-lsd-300.
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       wrt-rec-lsd-999.
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
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-prz-lst       to   w-let-arc-dcp-prz      .
           move      rf-dcp-dec-prz       to   w-let-arc-dcp-dec      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      all   "."            to   w-let-arc-dcp-des      .
           go to     let-arc-dcp-600.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
       let-arc-dcp-600.
           move      zero                 to   w-let-arc-dcp-prz      .
           move      zero                 to   w-let-arc-dcp-dec      .
       let-arc-dcp-999.
           exit.

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
           go to     let-arc-zcs-999.
       let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-des      .
       let-arc-zcs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zpv]                         *
      *    *-----------------------------------------------------------*
       let-arc-zpv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zpv-cod    =    zero
                     go to let-arc-zpv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV    "         to   f-key                  .
           move      w-let-arc-zpv-tip    to   rf-zpv-tip-cpv         .
           move      w-let-arc-zpv-cod    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zpv-400.
       let-arc-zpv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zpv-des-cpv       to   w-let-arc-zpv-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zpv-999.
       let-arc-zpv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zpv-flg      .
           move      all   "."            to   w-let-arc-zpv-des      .
           go to     let-arc-zpv-999.
       let-arc-zpv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zpv-des      .
       let-arc-zpv-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione categoria di sconto prod.  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzcs3.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice categoria prov- *
      *    * vigioni legate al prodotto                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnzpv2.acs"                   .

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
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
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
           move      12                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
       box-msg-e02-999.
           exit.

