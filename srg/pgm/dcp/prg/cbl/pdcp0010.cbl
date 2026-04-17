       Identification Division.
       Program-Id.                                 pdcp0010           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:    prs                 *
      *                                   Fase:    dcp001              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 12/08/91    *
      *                       Ultima revisione:    NdK del 01/06/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione filtri per ordinamento e selezione *
      *                    su archivio [dcp]                           *
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
                     "dcp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "prs"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcp001"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcp0010"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   FILTRI PER ORDINAMENTO E SELEZIONE   "       .

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
      *            *---------------------------------------------------*
      *            * Flag di uscita per il tasto 'Exit'                *
      *            *---------------------------------------------------*
               10  w-cnt-acc-flg-exi      pic  x(01)                  .
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
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [zum]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzum"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                         .
      *        *-------------------------------------------------------*
      *        * [zp1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp1"                          .
      *        *-------------------------------------------------------*
      *        * [zp2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp2"                          .
      *        *-------------------------------------------------------*
      *        * [zp3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp3"                          .
      *        *-------------------------------------------------------*
      *        * [zps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzps"                          .
      *        *-------------------------------------------------------*
      *        * [zcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzcp"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [pdt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfpdt"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-tip-rec          pic  x(04)                  .
               10  w-tes-cod-flt          pic  9(07)                  .
               10  w-tes-cod-flt-aut      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-ide-dat          pic  9(07)                  .
               10  w-tes-ide-ute          pic  x(08)                  .
               10  w-tes-ide-fas          pic  x(06)                  .
               10  w-tes-cod-mne          pic  x(10)                  .
               10  w-tes-des-key          pic  x(40)                  .
               10  w-tes-flt-ute          pic  x(01)                  .
               10  w-tes-des-flt          pic  x(40)                  .
               10  w-tes-dat-dcp.
                   15  w-tes-tip-ord      pic  9(02)                  .
                   15  w-tes-cla-min      pic  9(05)                  .
                   15  w-tes-cla-max      pic  9(05)                  .
                   15  w-tes-gru-min      pic  9(05)                  .
                   15  w-tes-gru-max      pic  9(05)                  .
                   15  w-tes-sgr-min      pic  9(05)                  .
                   15  w-tes-sgr-max      pic  9(05)                  .
                   15  w-tes-cod-min      pic  x(14)                  .
                   15  w-tes-cod-max      pic  x(14)                  .
                   15  w-tes-des-min      pic  x(20)                  .
                   15  w-tes-des-max      pic  x(20)                  .
                   15  w-tes-cod-iva      pic  9(05)                  .
                   15  w-tes-tip-pro      pic  9(02)                  .
                   15  w-tes-ctp-ven      pic  9(07)                  .
                   15  w-tes-umi-ven      pic  x(03)                  .
                   15  w-tes-cod-s01      pic  9(05)                  .
                   15  w-tes-cod-s02      pic  9(05)                  .
                   15  w-tes-cod-s03      pic  9(05)                  .
                   15  w-tes-cod-fnt      pic  9(07)                  .
                   15  w-tes-cod-pdt      pic  9(07)                  .
                   15  w-tes-cfp-min      pic  x(20)                  .
                   15  w-tes-cfp-max      pic  x(20)                  .
                   15  w-tes-sta-tus      pic  9(02)                  .
                   15  w-tes-sta-tuw.
                       20  w-tes-sta-nor  pic  x(01)                  .
                       20  w-tes-sta-esa  pic  x(01)                  .
                       20  w-tes-sta-sos  pic  x(01)                  .
                       20  w-tes-sta-ces  pic  x(01)                  .
                       20  w-tes-sta-cms  pic  x(01)                  .
                       20  w-tes-sta-obs  pic  x(01)                  .
                       20  w-tes-sta-oms  pic  x(01)                  .
                   15  w-tes-spc-lib      pic  x(20)                  .
                   15  w-tes-icm-min      pic  9(07)                  .
                   15  w-tes-icm-max      pic  9(07)                  .
                   15  w-tes-cod-cpv      pic  x(03)                  .
               10  w-tes-des-dat.
                   15  w-tes-cod-iva-des  pic  x(15)                  .
                   15  w-tes-ctp-ven-des  pic  x(40)                  .
                   15  w-tes-umi-ven-des  pic  x(20)                  .
                   15  w-tes-cod-s01-des  pic  x(20)                  .
                   15  w-tes-cod-s02-des  pic  x(20)                  .
                   15  w-tes-cod-s03-des  pic  x(20)                  .
                   15  w-tes-cod-fnt-rag  pic  x(40)                  .
                   15  w-tes-cod-pdt-rag  pic  x(40)                  .
                   15  w-tes-cod-cpv-des  pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per comodi di accettazione                      *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Flag di non accettabilita' codice catalogo            *
      *        *-------------------------------------------------------*
           05  w-acc-cod-cpv              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Elenco elementi selezionati                           *
      *        *-------------------------------------------------------*
           05  w-acc-ser-els              pic  9(02)                  .
           05  w-acc-ser-sel.
               10  w-acc-ser-ele occurs 72.
                   15  w-acc-ser-sel-alf  pic  x(14)                  .
                   15  w-acc-ser-sel-num  pic  9(07)                  .
                   15  w-acc-ser-sel-des  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Area per impostazione serie elementi da selezionare   *
      *        *-------------------------------------------------------*
           05  w-acc-ser-edd.
               10  w-acc-ser-edd-max      pic  9(02) value 72         .
               10  w-acc-ser-edd-els      pic  9(02)                  .
               10  w-acc-ser-edd-pev      pic  9(02)                  .
               10  w-acc-ser-edd-nel      pic  9(02)                  .
               10  w-acc-ser-edd-n1v      pic  9(02)                  .
               10  w-acc-ser-edd-nev      pic  9(02)                  .
               10  w-acc-ser-edd-nec      pic  9(02)                  .
               10  w-acc-ser-edd-fce      pic  x(01)                  .
               10  w-acc-ser-edd-led.
                   15  w-acc-ser-edd-rig  pic  x(03)                  .
                   15  filler             pic  x(03)                  .
                   15  w-acc-ser-edd-cod  pic  x(14)                  .
                   15  filler             pic  x(03)                  .
                   15  w-acc-ser-edd-des  pic  x(40)                  .
                   15  filler             pic  x(13)                  .
               10  w-acc-ser-edd-c01      pic  9(02)                  .
               10  w-acc-ser-edd-c02      pic  9(02)                  .
               10  w-acc-ser-edd-c0a      pic  9(02)                  .
               10  w-acc-ser-edd-c0b      pic  9(02)                  .
               10  w-acc-ser-edd-c0c      pic  9(02)                  .
               10  w-acc-ser-edd-c0p      pic  9(02)                  .
               10  w-acc-ser-edd-c0q      pic  9(02)                  .
               10  w-acc-ser-edd-c0r      pic  9(02)                  .
               10  w-acc-ser-edd-spe      pic  x(14)                  .
               10  w-acc-ser-edd-ftr      pic  9(02)                  .
               10  w-acc-ser-edd-sgl      pic  x(03)                  .
               10  w-acc-ser-edd-tmf      pic  x(05)                  .
               10  w-acc-ser-edd-tmb      pic  x(05)                  .
               10  w-acc-ser-edd-svk      pic  x(08)                  .
               10  w-acc-ser-edd-stu      pic  x(08)                  .
               10  w-acc-ser-edd-fcl.
                   15  filler             pic  x(08)                  .
                   15  w-acc-ser-edd-070  pic  x(70)                  .
                   15  filler             pic  x(02)                  .
               10  w-acc-ser-edd-txt.
                   15  w-acc-ser-edd-rtr  occurs 03
                                          pic  x(40)                  .
               10  w-acc-ser-edd-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-acc-ser-edd-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-acc-ser-edd-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c. eventualmente passate   *
      *    * dal chiamante                                             *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Per selezione casuale prodotti                        *
      *        *-------------------------------------------------------*
           05  w-ipc-esl-dcp.
               10  w-ipc-esl-dcp-snx      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per uscita forzata dopo inserimento             *
      *    *-----------------------------------------------------------*
       01  w-usc.
      *        *-------------------------------------------------------*
      *        * Flag di mnemonico a spaces nell' ultimo record in     *
      *        * Inserimento                                           *
      *        *-------------------------------------------------------*
           05  w-usc-ult-mne-spc          pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dcp '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/wzosdcp0.wkl"                   .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dcp', se selezione casuale prodotti               *
      *    *-----------------------------------------------------------*
       01  w-zos-esl-dcp.
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento, selezione                           *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-tip          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero di elementi selezionati                        *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-els          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Filler                                                *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-fil.
               10  filler     occurs  4   pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Selezione prodotti                                    *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-sel.
               10  w-zos-esl-dcp-ele occurs 72.
                   15  w-zos-esl-dcp-num  pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no gestione catalogo per prodotti                  *
      *        *-------------------------------------------------------*
           05  w-prs-snx-cpv              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det esistenza filtro legato a catalogo       *
      *        *-------------------------------------------------------*
           05  w-det-flt-esi.
               10  w-det-flt-esi-flg      pic  x(01)                  .
               10  w-det-flt-esi-mne      pic  x(10)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-num      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zum]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zum.
               10  w-let-arc-zum-flg      pic  x(01)                  .
               10  w-let-arc-zum-cod      pic  x(03)                  .
               10  w-let-arc-zum-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zps]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zps.
               10  w-let-arc-zps-flg      pic  x(01)                  .
               10  w-let-arc-zps-tip      pic  9(02)                  .
               10  w-let-arc-zps-cod      pic  9(05)                  .
               10  w-let-arc-zps-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcp.
               10  w-let-arc-zcp-flg      pic  x(01)                  .
               10  w-let-arc-zcp-cod      pic  x(03)                  .
               10  w-let-arc-zcp-des      pic  x(40)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-fnt      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdt.
               10  w-let-arc-pdt-flg      pic  x(01)                  .
               10  w-let-arc-pdt-cod      pic  9(07)                  .
               10  w-let-arc-pdt-rag      pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Codice catalogo                                       *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cpv              pic  x(03)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Filtro dell'utente                         *
      *        *-------------------------------------------------------*
           05  w-exp-flt-ute.
               10  w-exp-flt-ute-num      pic  9(02)       value 02   .
               10  w-exp-flt-ute-lun      pic  9(02)       value 02   .
               10  w-exp-flt-ute-tbl.
                   15  filler             pic  x(02) value "No"       .
                   15  filler             pic  x(02) value "Si"       .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento, selezione                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 4    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(40) value
                            "Classe, gruppo, sottogr., cod. prodotto ".
                   15  filler             pic  x(40) value
                            "Classe, gruppo, sottogr., desc. prodotto".
                   15  filler             pic  x(40) value
                            "Codice prodotto                         ".
                   15  filler             pic  x(40) value
                            "Descrizione prodotto                    ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo prodotto                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-pro.
               10  w-exp-tip-pro-num      pic  9(02)       value 5    .
               10  w-exp-tip-pro-lun      pic  9(02)       value 25   .
               10  w-exp-tip-pro-tbl.
                   15  filler             pic  x(25) value
                            "Tutti                    "               .
                   15  filler             pic  x(25) value
                            "Merce                    "               .
                   15  filler             pic  x(25) value
                            "Servizio                 "               .
                   15  filler             pic  x(25) value
                            "Imballo                  "               .
                   15  filler             pic  x(25) value
                            "Extra attivita' aziendale"               .
      *        *-------------------------------------------------------*
      *        * Work per : Selezione su status commerciale            *
      *        *-------------------------------------------------------*
           05  w-exp-sta-tus.
               10  w-exp-sta-tus-num      pic  9(02)       value 7    .
               10  w-exp-sta-tus-lun      pic  9(02)       value 40   .
               10  w-exp-sta-tus-tbl.
                   15  filler             pic  x(40) value
                            "[ ] Normale                             ".
                   15  filler             pic  x(40) value
                            "[ ] Ad esaurimento                      ".
                   15  filler             pic  x(40) value
                            "[ ] Sostituito da ns. nuovo prodotto    ".
                   15  filler             pic  x(40) value
                            "[ ] Cessata commercializzazione         ".
                   15  filler             pic  x(40) value
                            "[ ] Cessata commercializzazione ma sost.".
                   15  filler             pic  x(40) value
                            "[ ] Obsoleto                            ".
                   15  filler             pic  x(40) value
                            "[ ] Obsoleto, ma sostituito             ".

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dcp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe prodotto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp10.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice gruppo prodotto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp20.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottogruppo prodotto    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp30.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione unita' di misura               *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 1            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzps1.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 2            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzps2.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 3            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzps3.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice catalogo                *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzcp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice Iva                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice fornitore commerciale   *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice casa produttrice        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnpdt0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      *    *===========================================================*
      *    * Work per attribuzione e ripristino codice automatico      *
      *    *-----------------------------------------------------------*
       01  w-enc-zos.
      *        *-------------------------------------------------------*
      *        * Tipo record interessato                               *
      *        *-------------------------------------------------------*
           05  w-enc-zos-tip-rec          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Massimo valore accettabile                            *
      *        *-------------------------------------------------------*
           05  w-enc-zos-val-max          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore pre incremento                                 *
      *        *-------------------------------------------------------*
           05  w-enc-zos-val-pre          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valore post incremento                                *
      *        *-------------------------------------------------------*
           05  w-enc-zos-val-pos          pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
      *                      * Test su variabile di i.p.c.             *
      *                      *-----------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "S" or
                     w-ipc-esl-dcp-snx    =    "X"
                     go to main-800
           else      go to main-100.
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
                     go to exe-acc-cmp-600.
      *              *-------------------------------------------------*
      *              * Se il campo impostato ha modificato il valore   *
      *              * precedente : ripetizione impostazione           *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
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
      *              * Se function key DELT si esce                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     go to exe-acc-cmp-999.
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
       exe-acc-cmp-600.
      *              *-------------------------------------------------*
      *              * Flag globale di avvenuta almeno una modifica    *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     move  "#"            to   w-cnt-acc-flg-aum      .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento              *
      *              *-------------------------------------------------*
           perform   vis-tip-fun-000      thru vis-tip-fun-999        .
       exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to exe-acc-cmp-850.
      *                  *---------------------------------------------*
      *                  * Subroutine di controllo per Exit            *
      *                  *---------------------------------------------*
           perform   exe-acc-cmp-exi-000  thru exe-acc-cmp-exi-999    .
      *                  *---------------------------------------------*
      *                  * Se Exit non attivato : a ripristino para-   *
      *                  * metri di impostazione                       *
      *                  *---------------------------------------------*
           if        w-cnt-acc-flg-exi    not  = spaces
                     go to exe-acc-cmp-400.
      *                  *---------------------------------------------*
      *                  * Altrimenti : ad uscita                      *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-900.
       exe-acc-cmp-850.
       exe-acc-cmp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-999.
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *                                                           *
      *    * Subroutine di controllo tasto 'Exit'                      *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-exi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-flg-exi      .
       exe-acc-cmp-exi-050.
      *              *-------------------------------------------------*
      *              * Se impostazione campi chiave : si ignora        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    "K"
                     go to exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Se in visualizzazione : si ignora               *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-fun    =    "V"
                     go to exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Se non avvenuta alcuna modifica : si ignora     *
      *              *-------------------------------------------------*
           if        w-cnt-acc-flg-aum    =    spaces
                     go to exe-acc-cmp-exi-920.
       exe-acc-cmp-exi-100.
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
      *              * Richiesta conferma di uscita                    *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#EXI"               to   v-not                  .
           move      "N"                  to   v-alf                  .
           move      "SN"                 to   v-msk                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "UP  "               to   v-pfk (02)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    spaces
                     if      v-alf        =    "S"
                             move  "EXIT" to   v-key
                     else    move  "UP  " to   v-key                  .
      *              *-------------------------------------------------*
      *              * Se non function key EXIT si ripristina          *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to exe-acc-cmp-exi-600.
      *              *-------------------------------------------------*
      *              * Altrimenti : ad Exit attivato                   *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-exi-800.
       exe-acc-cmp-exi-600.
      *              *-------------------------------------------------*
      *              * Se uscita senza Exit                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di Exit non attivato                   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-acc-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-exi-900.
       exe-acc-cmp-exi-800.
      *              *-------------------------------------------------*
      *              * Se uscita con Exit                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-acc-cmp-exi-900.
       exe-acc-cmp-exi-900.
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 di note operative      *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      w-cnt-acc-sav-l23    to   v-nt1                  .
           move      w-cnt-acc-sav-l24    to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-exi-920.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-acc-cmp-exi-999.
       exe-acc-cmp-exi-999.
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
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero livelli del piano dei conti          *
      *                  *---------------------------------------------*
           perform   prs-liv-pdc-000      thru prs-liv-pdc-999        .
      *                  *---------------------------------------------*
      *                  * Si/no gestione bar-code prodotti            *
      *                  *---------------------------------------------*
           perform   prs-snx-cpv-000      thru prs-snx-cpv-999        .
      *              *-------------------------------------------------*
      *              * Preparazione tipo record con : 'dcp'            *
      *              *-------------------------------------------------*
           move      "dcp "               to   w-tes-tip-rec          .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * la selezione casuale su prodotti                *
      *              *-------------------------------------------------*
           perform   ipc-esl-dcp-000      thru ipc-esl-dcp-999        .
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Test su variabile di i.p.c. letta               *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "X"
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice filtro          *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-opn-000  thru cod-zos-dcp-opn-999    .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Test su variabile di i.p.c. letta               *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "S"
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice classe          *
      *              *-------------------------------------------------*
           perform   cod-mne-zp1-opn-000  thru cod-mne-zp1-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice gruppo          *
      *              *-------------------------------------------------*
           perform   cod-mne-zp2-opn-000  thru cod-mne-zp2-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottogruppo     *
      *              *-------------------------------------------------*
           perform   cod-mne-zp3-opn-000  thru cod-mne-zp3-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione unita' di misura       *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-opn-000  thru cod-cod-zum-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 1    *
      *              *-------------------------------------------------*
           perform   cmn-zps-001-opn-000  thru cmn-zps-001-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 2    *
      *              *-------------------------------------------------*
           perform   cmn-zps-002-opn-000  thru cmn-zps-002-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 3    *
      *              *-------------------------------------------------*
           perform   cmn-zps-003-opn-000  thru cmn-zps-003-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice catalogo        *
      *              *-------------------------------------------------*
           perform   cod-cod-zcp-opn-000  thru cod-cod-zcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice Iva             *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-opn-000  thru cod-mne-zci-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice fornitore com-  *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-opn-000  thru cod-mne-dcf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice casa produttr.  *
      *              *-------------------------------------------------*
           perform   cod-mne-pdt-opn-000  thru cod-mne-pdt-opn-999    .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di mnemonico a spaces in   *
      *              * ultimo record in Inserimento                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-usc-ult-mne-spc      .
       pre-exe-pgm-999.
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
      *    * Lettura personalizzazione : Si/No gestione catalogo       *
      *    *                             prodotti                      *
      *    *-----------------------------------------------------------*
       prs-snx-cpv-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[snx-cpv]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-cpv
           else      move  spaces         to   w-prs-snx-cpv          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-cpv        not   = "S"
                     move  "N"            to   w-prs-snx-cpv          .
       prs-snx-cpv-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c.               *
      *    *-----------------------------------------------------------*
       ipc-esl-dcp-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'esl-dcp' dallo      *
      *              * stesso livello                                  *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "esl-dcp"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-esl-dcp-200
           else      go to ipc-esl-dcp-400.
       ipc-esl-dcp-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-esl-dcp-snx      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione della variabile             *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "S" and
                     w-ipc-esl-dcp-snx    not  = "X"
                     go to ipc-esl-dcp-400.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-esl-dcp-999.
       ipc-esl-dcp-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura dei valori                        *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-esl-dcp-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-esl-dcp-999.
       ipc-esl-dcp-999.
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
      *              * Test su variabile di i.p.c. letta               *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "X"
                     go to pos-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro         *
      *              *-------------------------------------------------*
           perform   cod-zos-dcp-cls-000  thru cod-zos-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Test su variabile di i.p.c. letta               *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "S"
                     go to pos-exe-pgm-999.
       pos-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice classe         *
      *              *-------------------------------------------------*
           perform   cod-mne-zp1-cls-000  thru cod-mne-zp1-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice gruppo         *
      *              *-------------------------------------------------*
           perform   cod-mne-zp2-cls-000  thru cod-mne-zp2-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottogruppo    *
      *              *-------------------------------------------------*
           perform   cod-mne-zp3-cls-000  thru cod-mne-zp3-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione unita' di misura      *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-cls-000  thru cod-cod-zum-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 1   *
      *              *-------------------------------------------------*
           perform   cmn-zps-001-cls-000  thru cmn-zps-001-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 2   *
      *              *-------------------------------------------------*
           perform   cmn-zps-002-cls-000  thru cmn-zps-002-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 3   *
      *              *-------------------------------------------------*
           perform   cmn-zps-003-cls-000  thru cmn-zps-003-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice catalogo       *
      *              *-------------------------------------------------*
           perform   cod-cod-zcp-cls-000  thru cod-cod-zcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice Iva            *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-cls-000  thru cod-mne-zci-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice fornitore com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcf-cls-000  thru cod-mne-dcf-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice casa produttr. *
      *              *-------------------------------------------------*
           perform   cod-mne-pdt-cls-000  thru cod-mne-pdt-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
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
      *              * Test su variabile di i.p.c. letta               *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to rou-opn-fls-999.
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
      *              *-------------------------------------------------*
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [zps]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
      *              *-------------------------------------------------*
      *              * [zcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcp                 .
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
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [pdt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              * Test su variabile di i.p.c. letta               *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to rou-cls-fls-999.
       rou-cls-fls-200.
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * [zum]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
      *              *-------------------------------------------------*
      *              * [zci]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *              *-------------------------------------------------*
      *              * [pdc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * [zps]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
      *              *-------------------------------------------------*
      *              * [zcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcp                 .
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
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [pdt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave della registrazione             *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *              *-------------------------------------------------*
      *              * Se flag di mnemonico a spaces in ultimo Inseri- *
      *              * mento : uscita come per Exit                    *
      *              *-------------------------------------------------*
           if        w-usc-ult-mne-spc    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-key-reg-999.
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
      *                  * Codice filtro                               *
      *                  *---------------------------------------------*
           perform   acc-cod-flt-000      thru acc-cod-flt-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
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
      *              * Codice filtro                                   *
      *              *-------------------------------------------------*
           perform   vis-cod-flt-000      thru vis-cod-flt-999        .
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
      *              * Codice filtro                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-flt-000      thru pmt-cod-flt-999        .
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
      *    * Visualizzazione prompts per Codice filtro                 *
      *    *-----------------------------------------------------------*
       pmt-cod-flt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice del filtro          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-flt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice filtro                 *
      *    *-----------------------------------------------------------*
       acc-cod-flt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su variabile di i.p.c. letta           *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "X"
                     go to acc-cod-flt-100.
      *                  *---------------------------------------------*
      *                  * Attribuzione codice automatico progressivo  *
      *                  *---------------------------------------------*
           move      w-tes-tip-rec        to   w-enc-zos-tip-rec      .
           move      9999998              to   w-enc-zos-val-max      .
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                  *---------------------------------------------*
      *                  * Codice automatico in campo di destinazione  *
      *                  *---------------------------------------------*
           move      w-enc-zos-val-pos    to   w-tes-cod-flt          .
      *                  *---------------------------------------------*
      *                  * Segnale di attribuzione codice eseguita     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-cod-flt-aut      .
      *                  *---------------------------------------------*
      *                  * Forzatura tasto 'Do'                        *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     acc-cod-flt-800.
       acc-cod-flt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dcp-ope      .
           move      w-tes-cod-flt        to   w-cod-zos-dcp-cod      .
           move      04                   to   w-cod-zos-dcp-lin      .
           move      30                   to   w-cod-zos-dcp-pos      .
           move      06                   to   w-cod-zos-dcp-dln      .
           move      30                   to   w-cod-zos-dcp-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-dcp-cll-000  thru cod-zos-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-dcp-foi-000  thru cod-zos-dcp-foi-999    .
       acc-cod-flt-110.
           perform   cod-zos-dcp-cll-000  thru cod-zos-dcp-cll-999    .
           if        w-cod-zos-dcp-ope    =    "F+"
                     go to acc-cod-flt-115.
           if        w-cod-zos-dcp-ope    =    "AC"
                     go to acc-cod-flt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-flt-115.
           perform   cod-zos-dcp-foi-000  thru cod-zos-dcp-foi-999    .
           go to     acc-cod-flt-110.
       acc-cod-flt-120.
           move      w-cod-zos-dcp-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-flt-999.
       acc-cod-flt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-flt          .
       acc-cod-flt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se il codice impostato *
      *                  * e' zero oppure diverso da zero              *
      *                  *---------------------------------------------*
           if        w-tes-cod-flt        =    zero
                     go to acc-cod-flt-410
           else      go to acc-cod-flt-600.
       acc-cod-flt-410.
      *                  *---------------------------------------------*
      *                  * Se il codice impostato e' zero              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo funzionamento codice in cre- *
      *                      * azione e' automatico                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Attribuzione codice automatico pro- *
      *                          * gressivo                            *
      *                          *-------------------------------------*
           move      w-tes-tip-rec        to   w-enc-zos-tip-rec      .
           move      9999998              to   w-enc-zos-val-max      .
           perform   att-cod-aut-000      thru att-cod-aut-999        .
      *                          *-------------------------------------*
      *                          * Codice automatico in campo di de-   *
      *                          * stinazione                          *
      *                          *-------------------------------------*
           move      w-enc-zos-val-pos    to   w-tes-cod-flt          .
      *                          *-------------------------------------*
      *                          * Segnale di attribuzione codice ese- *
      *                          * guita automaticamente               *
      *                          *-------------------------------------*
           move      "#"                  to   w-tes-cod-flt-aut      .
      *                          *-------------------------------------*
      *                          * Visualizzazione del codice          *
      *                          *-------------------------------------*
           perform   vis-cod-flt-000      thru vis-cod-flt-999        .
       acc-cod-flt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-flt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-flt-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-flt-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-flt-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-flt-999.
       acc-cod-flt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice filtro              *
      *    *-----------------------------------------------------------*
       vis-cod-flt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cod-flt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-flt-999.
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
                     move  all "#"        to   w-cnt-sts-ing-pte
                     move  all "#"        to   w-cnt-sts-imp-pte      .
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
      *                          *-------------------------------------*
      *                          * Test su variabile di i.p.c.         *
      *                          *-------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "X"    and
                     w-acc-ser-sel-alf (1)
                                          =    spaces and
                     w-acc-ser-edd-svk    =    "EXIT"
                     go to acc-nok-reg-860.
      *                          *-------------------------------------*
      *                          * Accettazione conferma               *
      *                          *-------------------------------------*
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
      *                      *-----------------------------------------*
      *                      * Subroutine di controllo per Exit        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Exit non attivato : come 'Up'    *
      *                          *-------------------------------------*
           perform   exe-acc-cmp-exi-000  thru exe-acc-cmp-exi-999    .
           if        w-cnt-acc-flg-exi    not  = spaces
                     go to acc-nok-reg-870.
      *                          *-------------------------------------*
      *                          * Se Exit attivato                    *
      *                          *-------------------------------------*
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
      *              * La testata e' composta di nr. 3 pagine          *
      *              *-------------------------------------------------*
           move      3                    to   w-cnt-sts-imp-mpt      .
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
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     snp-tes-reg-100
                     snp-tes-reg-200
                     snp-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina numero 1                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre gestita                              *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina numero 2                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se presente codice catalogo            *
      *                  *---------------------------------------------*
           if        w-tes-cod-cpv (1)    not  = spaces
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Test se variabile di i.p.c. lo prevede      *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina numero 3                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se presente codice catalogo            *
      *                  *---------------------------------------------*
           if        w-tes-cod-cpv (1)    not  = spaces
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Test se variabile di i.p.c. lo prevede      *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Test se codice fornitore preferenziale      *
      *                  *---------------------------------------------*
           if        w-tes-cod-fnt (1)    =    zero
                     move  "#"            to   w-cnt-sts-imp-snp
                     go to snp-tes-reg-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
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
                     acc-tes-reg-200
                     acc-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Test su variabile di i.p.c. letta           *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Si inizia sempre dalla descrizione          *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-110.
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Codice catalogo                             *
      *                  *---------------------------------------------*
           perform   acc-cod-cpv-000      thru acc-cod-cpv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Descrizione filtro                          *
      *                  *---------------------------------------------*
           perform   acc-des-flt-000      thru acc-des-flt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Filtro dell'utente                          *
      *                  *---------------------------------------------*
           perform   acc-flt-ute-000      thru acc-flt-ute-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Selezione casuale prodotti                  *
      *                  *---------------------------------------------*
           perform   acc-esl-dcp-000      thru acc-esl-dcp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento, selezione                 *
      *                  *---------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
           if        v-key                =    "DOWN"
                     go to acc-tes-reg-145.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di ordina-  *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           if        w-tes-tip-ord (1)    =    1
                     go to acc-tes-reg-145
           else if   w-tes-tip-ord (1)    =    2
                     go to acc-tes-reg-145
           else if   w-tes-tip-ord (1)    =    3
                     go to acc-tes-reg-175
           else if   w-tes-tip-ord (1)    =    4
                     go to acc-tes-reg-185
           else      go to acc-tes-reg-145.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * Classe min                                  *
      *                  *---------------------------------------------*
           perform   acc-cla-min-000      thru acc-cla-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Classe max                                  *
      *                  *---------------------------------------------*
           perform   acc-cla-max-000      thru acc-cla-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
       acc-tes-reg-155.
      *                  *---------------------------------------------*
      *                  * Gruppo min                                  *
      *                  *---------------------------------------------*
           perform   acc-gru-min-000      thru acc-gru-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Gruppo max                                  *
      *                  *---------------------------------------------*
           perform   acc-gru-max-000      thru acc-gru-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-155.
       acc-tes-reg-165.
      *                  *---------------------------------------------*
      *                  * Sottogruppo min                             *
      *                  *---------------------------------------------*
           perform   acc-sgr-min-000      thru acc-sgr-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
       acc-tes-reg-170.
      *                  *---------------------------------------------*
      *                  * Sottogruppo max                             *
      *                  *---------------------------------------------*
           perform   acc-sgr-max-000      thru acc-sgr-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-165.
       acc-tes-reg-175.
      *                  *---------------------------------------------*
      *                  * Codice min                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-min-000      thru acc-cod-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-170.
       acc-tes-reg-180.
      *                  *---------------------------------------------*
      *                  * Codice max                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-max-000      thru acc-cod-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-175.
       acc-tes-reg-185.
      *                  *---------------------------------------------*
      *                  * Descrizione min                             *
      *                  *---------------------------------------------*
           perform   acc-des-min-000      thru acc-des-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-180.
       acc-tes-reg-190.
      *                  *---------------------------------------------*
      *                  * Descrizione max                             *
      *                  *---------------------------------------------*
           perform   acc-des-max-000      thru acc-des-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-185.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 1                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-190.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina 2                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data inizio commercializzazione min         *
      *                  *---------------------------------------------*
           perform   acc-icm-min-000      thru acc-icm-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-202.
      *                  *---------------------------------------------*
      *                  * Data inizio commercializzazione max         *
      *                  *---------------------------------------------*
           perform   acc-icm-max-000      thru acc-icm-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-204.
      *                  *---------------------------------------------*
      *                  * Tipo prodotto                               *
      *                  *---------------------------------------------*
           perform   acc-tip-pro-000      thru acc-tip-pro-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-202.
       acc-tes-reg-210.
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-iva-000      thru acc-cod-iva-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-220.
      *                  *---------------------------------------------*
      *                  * Codice contropartita                        *
      *                  *---------------------------------------------*
           perform   acc-ctp-ven-000      thru acc-ctp-ven-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-210.
       acc-tes-reg-230.
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
           perform   acc-umi-ven-000      thru acc-umi-ven-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-220.
       acc-tes-reg-240.
      *                  *---------------------------------------------*
      *                  * Specifica libera                            *
      *                  *---------------------------------------------*
           perform   acc-spc-lib-000      thru acc-spc-lib-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-230.
       acc-tes-reg-250.
      *                  *---------------------------------------------*
      *                  * Codice statistico 1                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s01-000      thru acc-cod-s01-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-240.
       acc-tes-reg-260.
      *                  *---------------------------------------------*
      *                  * Codice statistico 2                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s02-000      thru acc-cod-s02-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-250.
       acc-tes-reg-270.
      *                  *---------------------------------------------*
      *                  * Codice statistico 3                         *
      *                  *---------------------------------------------*
           perform   acc-cod-s03-000      thru acc-cod-s03-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-260.
       acc-tes-reg-275.
      *                  *---------------------------------------------*
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           perform   acc-cod-fnt-000      thru acc-cod-fnt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-270.
       acc-tes-reg-280.
      *                  *---------------------------------------------*
      *                  * Codice casa produttrice                     *
      *                  *---------------------------------------------*
           perform   acc-cod-pdt-000      thru acc-cod-pdt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-275.
       acc-tes-reg-285.
      *                  *---------------------------------------------*
      *                  * Status prodotto                             *
      *                  *---------------------------------------------*
           perform   acc-sta-tus-000      thru acc-sta-tus-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-280.
       acc-tes-reg-290.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice prodotto fornitore min               *
      *                  *---------------------------------------------*
           perform   acc-cfp-min-000      thru acc-cfp-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-302.
      *                  *---------------------------------------------*
      *                  * Codice prodotto fornitore max               *
      *                  *---------------------------------------------*
           perform   acc-cfp-max-000      thru acc-cfp-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-390.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
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
                     vis-tes-reg-200
                     vis-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice catalogo                                 *
      *              *-------------------------------------------------*
           perform   vis-cod-cpv-000      thru vis-cod-cpv-999        .
           perform   vis-cod-cpv-des-000  thru vis-cod-cpv-des-999    .
      *              *-------------------------------------------------*
      *              * Descrizione filtro                              *
      *              *-------------------------------------------------*
           perform   vis-des-flt-000      thru vis-des-flt-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Filtro dell'utente                              *
      *              *-------------------------------------------------*
           perform   vis-flt-ute-000      thru vis-flt-ute-999        .
      *              *-------------------------------------------------*
      *              * Selezione casuale prodotti                      *
      *              *-------------------------------------------------*
           perform   vis-esl-dcp-000      thru vis-esl-dcp-999        .
      *              *-------------------------------------------------*
      *              * Tipo ordinamento, selezione                     *
      *              *-------------------------------------------------*
           perform   vis-tip-ord-000      thru vis-tip-ord-999        .
      *              *-------------------------------------------------*
      *              * Classe min                                      *
      *              *-------------------------------------------------*
           perform   vis-cla-min-000      thru vis-cla-min-999        .
      *              *-------------------------------------------------*
      *              * Classe max                                      *
      *              *-------------------------------------------------*
           perform   vis-cla-max-000      thru vis-cla-max-999        .
      *              *-------------------------------------------------*
      *              * Gruppo min                                      *
      *              *-------------------------------------------------*
           perform   vis-gru-min-000      thru vis-gru-min-999        .
      *              *-------------------------------------------------*
      *              * Gruppo max                                      *
      *              *-------------------------------------------------*
           perform   vis-gru-max-000      thru vis-gru-max-999        .
      *              *-------------------------------------------------*
      *              * Sottogruppo min                                 *
      *              *-------------------------------------------------*
           perform   vis-sgr-min-000      thru vis-sgr-min-999        .
      *              *-------------------------------------------------*
      *              * Sottogruppo max                                 *
      *              *-------------------------------------------------*
           perform   vis-sgr-max-000      thru vis-sgr-max-999        .
      *              *-------------------------------------------------*
      *              * Codice min                                      *
      *              *-------------------------------------------------*
           perform   vis-cod-min-000      thru vis-cod-min-999        .
      *              *-------------------------------------------------*
      *              * Codice max                                      *
      *              *-------------------------------------------------*
           perform   vis-cod-max-000      thru vis-cod-max-999        .
      *              *-------------------------------------------------*
      *              * Descrizione min                                 *
      *              *-------------------------------------------------*
           perform   vis-des-min-000      thru vis-des-min-999        .
      *              *-------------------------------------------------*
      *              * Descrizione max                                 *
      *              *-------------------------------------------------*
           perform   vis-des-max-000      thru vis-des-max-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Data inizio commercializzazione min             *
      *              *-------------------------------------------------*
           perform   vis-icm-min-000      thru vis-icm-min-999        .
      *              *-------------------------------------------------*
      *              * Data inizio commercializzazione max             *
      *              *-------------------------------------------------*
           perform   vis-icm-max-000      thru vis-icm-max-999        .
      *              *-------------------------------------------------*
      *              * Tipo prodotto                                   *
      *              *-------------------------------------------------*
           perform   vis-tip-pro-000      thru vis-tip-pro-999        .
      *              *-------------------------------------------------*
      *              * Codice iva                                      *
      *              *-------------------------------------------------*
           perform   vis-cod-iva-000      thru vis-cod-iva-999        .
           perform   vis-des-iva-000      thru vis-des-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice contropartita                            *
      *              *-------------------------------------------------*
           perform   vis-ctp-ven-000      thru vis-ctp-ven-999        .
           perform   vis-des-ctp-000      thru vis-des-ctp-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   vis-umi-ven-000      thru vis-umi-ven-999        .
           perform   vis-des-umi-000      thru vis-des-umi-999        .
      *              *-------------------------------------------------*
      *              * Specifica libera                                *
      *              *-------------------------------------------------*
           perform   vis-spc-lib-000      thru vis-spc-lib-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 1                             *
      *              *-------------------------------------------------*
           perform   vis-cod-s01-000      thru vis-cod-s01-999        .
           perform   vis-des-cs1-000      thru vis-des-cs1-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 2                             *
      *              *-------------------------------------------------*
           perform   vis-cod-s02-000      thru vis-cod-s02-999        .
           perform   vis-des-cs2-000      thru vis-des-cs2-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 3                             *
      *              *-------------------------------------------------*
           perform   vis-cod-s03-000      thru vis-cod-s03-999        .
           perform   vis-des-cs3-000      thru vis-des-cs3-999        .
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   vis-cod-fnt-000      thru vis-cod-fnt-999        .
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
      *              *-------------------------------------------------*
      *              * Codice casa produttrice                         *
      *              *-------------------------------------------------*
           perform   vis-cod-pdt-000      thru vis-cod-pdt-999        .
           perform   vis-cod-pdt-rag-000  thru vis-cod-pdt-rag-999    .
      *              *-------------------------------------------------*
      *              * Status prodotto                                 *
      *              *-------------------------------------------------*
           perform   vis-sta-tus-000      thru vis-sta-tus-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Codice prodotto fornitore min                   *
      *              *-------------------------------------------------*
           perform   vis-cfp-min-000      thru vis-cfp-min-999        .
      *              *-------------------------------------------------*
      *              * Codice prodotto fornitore max                   *
      *              *-------------------------------------------------*
           perform   vis-cfp-max-000      thru vis-cfp-max-999        .
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
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-100
                     pmt-tes-reg-200
                     pmt-tes-reg-300
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Codice catalogo                                 *
      *              *-------------------------------------------------*
           perform   pmt-cod-cpv-000      thru pmt-cod-cpv-999        .
      *              *-------------------------------------------------*
      *              * Descrizione filtro                              *
      *              *-------------------------------------------------*
           perform   pmt-des-flt-000      thru pmt-des-flt-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Filtro dell'utente                              *
      *              *-------------------------------------------------*
           perform   pmt-flt-ute-000      thru pmt-flt-ute-999        .
      *              *-------------------------------------------------*
      *              * Tipo ordinamento, selezione                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-ord-000      thru pmt-tip-ord-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Data inizio commercializzazione min             *
      *              *-------------------------------------------------*
           perform   pmt-icm-min-000      thru pmt-icm-min-999        .
      *              *-------------------------------------------------*
      *              * Data inizio commercializzazione max             *
      *              *-------------------------------------------------*
           perform   pmt-icm-max-000      thru pmt-icm-max-999        .
      *              *-------------------------------------------------*
      *              * Tipo prodotto                                   *
      *              *-------------------------------------------------*
           perform   pmt-tip-pro-000      thru pmt-tip-pro-999        .
      *              *-------------------------------------------------*
      *              * Codice iva                                      *
      *              *-------------------------------------------------*
           perform   pmt-cod-iva-000      thru pmt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Codice contropartita                            *
      *              *-------------------------------------------------*
           perform   pmt-ctp-ven-000      thru pmt-ctp-ven-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   pmt-umi-ven-000      thru pmt-umi-ven-999        .
      *              *-------------------------------------------------*
      *              * Specifica libera                                *
      *              *-------------------------------------------------*
           perform   pmt-spc-lib-000      thru pmt-spc-lib-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 1                             *
      *              *-------------------------------------------------*
           perform   pmt-cod-s01-000      thru pmt-cod-s01-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 2                             *
      *              *-------------------------------------------------*
           perform   pmt-cod-s02-000      thru pmt-cod-s02-999        .
      *              *-------------------------------------------------*
      *              * Codice statistico 3                             *
      *              *-------------------------------------------------*
           perform   pmt-cod-s03-000      thru pmt-cod-s03-999        .
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-fnt-000      thru pmt-cod-fnt-999        .
      *              *-------------------------------------------------*
      *              * Codice casa produttrice                         *
      *              *-------------------------------------------------*
           perform   pmt-cod-pdt-000      thru pmt-cod-pdt-999        .
      *              *-------------------------------------------------*
      *              * Status prodotto                                 *
      *              *-------------------------------------------------*
           perform   pmt-sta-tus-000      thru pmt-sta-tus-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Codice prodotto fornitore min                   *
      *              *-------------------------------------------------*
           perform   pmt-cfp-min-000      thru pmt-cfp-min-999        .
      *              *-------------------------------------------------*
      *              * Codice prodotto fornitore max                   *
      *              *-------------------------------------------------*
           perform   pmt-cfp-max-000      thru pmt-cfp-max-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice catalogo                  *
      *    *-----------------------------------------------------------*
       pmt-cod-cpv-000.
      *              *-------------------------------------------------*
      *              * Test se personalizzazione attiva                *
      *              *-------------------------------------------------*
           if        w-prs-snx-cpv        not  = "S"
                     go to pmt-cod-cpv-999.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "S"
                     go to pmt-cod-cpv-999.
       pmt-cod-cpv-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice catalogo            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione                      *
      *    *-----------------------------------------------------------*
       pmt-des-flt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-flt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice mnemonico                 *
      *    *-----------------------------------------------------------*
       pmt-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice mnemonico           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Filtro dell'utente               *
      *    *-----------------------------------------------------------*
       pmt-flt-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Filtro personale           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-flt-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo ordinamento, selezione      *
      *    *-----------------------------------------------------------*
       pmt-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "S"
                     go to pmt-tip-ord-999.
       pmt-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo ordinamento           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * 'Selezioni su'                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezioni su"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "------ minimo ------    ----- massimo ------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Classe merceologica                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  - Classe    merceologica :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Gruppo merceologico                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  - Gruppo    merceologico :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sottogruppo merceologico                        *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  - Sottogr.  merceologico :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  - Codice      prodotto   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  - Descrizione prodotto   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data inizio commercializ. min    *
      *    *-----------------------------------------------------------*
       pmt-icm-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data codifica          dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-icm-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Data inizio commercializ. max    *
      *    *-----------------------------------------------------------*
       pmt-icm-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      "al :"               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-icm-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo prodotto                    *
      *    *-----------------------------------------------------------*
       pmt-tip-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo prodotto              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice iva                       *
      *    *-----------------------------------------------------------*
       pmt-cod-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice iva                 :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice contropartita             *
      *    *-----------------------------------------------------------*
       pmt-ctp-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice contropartita       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Unita' di misura                 *
      *    *-----------------------------------------------------------*
       pmt-umi-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-umi-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Specifica libera                 *
      *    *-----------------------------------------------------------*
       pmt-spc-lib-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Specifica libera           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-spc-lib-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 1              *
      *    *-----------------------------------------------------------*
       pmt-cod-s01-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 1        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 2              *
      *    *-----------------------------------------------------------*
       pmt-cod-s02-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 2        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 3              *
      *    *-----------------------------------------------------------*
       pmt-cod-s03-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 3        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Prompt codice fornitore                                   *
      *    *-----------------------------------------------------------*
       pmt-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fornitore preferenziale    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Prompt codice casa produttrice                            *
      *    *-----------------------------------------------------------*
       pmt-cod-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice casa produttrice    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Status prodotto                  *
      *    *-----------------------------------------------------------*
       pmt-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Status prodotto            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice prodotto fornitore min    *
      *    *-----------------------------------------------------------*
       pmt-cfp-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "------ minimo ------    ----- massimo ------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice prodotto per il     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "fornitore preferenziale     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cfp-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice prodotto fornitore max    *
      *    *-----------------------------------------------------------*
       pmt-cfp-max-000.
       pmt-cfp-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
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
           move      "DO  "               to   v-pfk (05)             .
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
      *    * Accettazione campo : Codice catalogo                      *
      *    *-----------------------------------------------------------*
       acc-cod-cpv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se personalizzazione attiva            *
      *                  *---------------------------------------------*
           if        w-prs-snx-cpv        not  = "S"
                     go to acc-cod-cpv-999.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-cod-cpv-999.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare in virtu' di al- *
      *                  * campi inseriti                              *
      *                  *---------------------------------------------*
           if        w-acc-cod-cpv        not  = spaces
                     go to acc-cod-cpv-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cpv (1)    to   w-sav-cod-cpv          .
       acc-cod-cpv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zcp-ope      .
           move      w-tes-cod-cpv (1)    to   w-cod-cod-zcp-cod      .
           move      06                   to   w-cod-cod-zcp-lin      .
           move      30                   to   w-cod-cod-zcp-pos      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-zcp-cll-000  thru cod-cod-zcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zcp-foi-000  thru cod-cod-zcp-foi-999    .
       acc-cod-cpv-110.
           perform   cod-cod-zcp-cll-000  thru cod-cod-zcp-cll-999    .
           if        w-cod-cod-zcp-ope    =    "F+"
                     go to acc-cod-cpv-115.
           if        w-cod-cod-zcp-ope    =    "AC"
                     go to acc-cod-cpv-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cpv-115.
           perform   cod-cod-zcp-foi-000  thru cod-cod-zcp-foi-999    .
           go to     acc-cod-cpv-110.
       acc-cod-cpv-120.
           move      w-cod-cod-zcp-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cpv-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-cpv-999.
       acc-cod-cpv-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-cpv (1)      .
       acc-cod-cpv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura file [zcp]                          *
      *                  *---------------------------------------------*
           move      w-tes-cod-cpv (1)    to   w-let-arc-zcp-cod      .
           perform   let-arc-zcp-000      thru let-arc-zcp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zcp-des    to   w-tes-cod-cpv-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-cpv-des-000  thru vis-cod-cpv-des-999    .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zcp-flg    not  = spaces
                     go to acc-cod-cpv-100.
       acc-cod-cpv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se impostato un codice diverso da spaces si *
      *                  * normalizza il tipo ordinamento, si forza    *
      *                  * la descrizione filtro ed il mnemonico       *
      *                  *---------------------------------------------*
           if        w-tes-cod-cpv (1)    =    spaces
                     go to acc-cod-cpv-800.
      *                  *---------------------------------------------*
      *                  * Test se codice variato                      *
      *                  *---------------------------------------------*
           if        w-tes-cod-cpv (1)    =    w-sav-cod-cpv
                     go to acc-cod-cpv-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione tipo ordinamento            *
      *                  *---------------------------------------------*
           move      89                   to   w-tes-tip-ord (1)      .
      *                  *---------------------------------------------*
      *                  * Forzatura descrizione filtro                *
      *                  *---------------------------------------------*
           move      w-let-arc-zcp-des    to   w-tes-des-flt (1)      .
           perform   vis-des-flt-000      thru vis-des-flt-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura mnemonico filtro                  *
      *                  *---------------------------------------------*
           move      w-tes-cod-cpv (1)    to   w-tes-cod-mne (1)      .
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
      *                  *---------------------------------------------*
      *                  * Test se codice gia' inserito                *
      *                  *---------------------------------------------*
           move      w-tes-cod-mne (1)    to   w-det-flt-esi-mne      .
           perform   det-flt-esi-000      thru det-flt-esi-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-det-flt-esi-flg    =    spaces
                     go to acc-cod-cpv-800.
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "Esiste gia' un filtro assegnato al codice catalogo
      -              ""                   to   w-all-str-cat (01)     .
           move      w-det-flt-esi-mne    to   w-all-str-cat (02)     .
           move      "!"                  to   w-all-str-cat (03)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      w-all-str-alf        to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Box di errore                               *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-cod-cpv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-cpv-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-cpv-100.
       acc-cod-cpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice catalogo                   *
      *    *-----------------------------------------------------------*
       vis-cod-cpv-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-cpv (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cpv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice catalogo, descrizione      *
      *    *-----------------------------------------------------------*
       vis-cod-cpv-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-cpv-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cpv-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione filtro           *
      *    *-----------------------------------------------------------*
       acc-des-flt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-flt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-flt (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-flt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-flt-999.
       acc-des-flt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-flt (1)      .
       acc-des-flt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore sia diverso da spaces in *
      *                  * caso che si stia trattando il filtro stan-  *
      *                  * dard 9999999, a meno che non si sia in Up   *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)    not  = spaces
                     go to acc-des-flt-500.
           if        w-tes-cod-flt        not  = 9999999
                     go to acc-des-flt-600.
           if        v-key                =    "UP  "
                     go to acc-des-flt-600
           else      go to acc-des-flt-100.
       acc-des-flt-500.
      *                  *---------------------------------------------*
      *                  * Test che il primo carattere non sia spaces  *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)
                    (01 : 01)             =    spaces
                     go to acc-des-flt-100.
       acc-des-flt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-flt (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
      *                  *---------------------------------------------*
      *                  * Se descrizione a spaces : mnemonico a spa-  *
      *                  * ces, a meno che non si tratti del filtro    *
      *                  * standard con codice 9999999                 *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)    not  = spaces
                     go to acc-des-flt-800.
           if        w-tes-cod-flt        =    9999999
                     go to acc-des-flt-800.
           move      spaces               to   w-tes-cod-mne (1)      .
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
           move      spaces               to   w-tes-flt-ute (1)      .
           perform   vis-flt-ute-000      thru vis-flt-ute-999        .
       acc-des-flt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-flt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-flt-100.
       acc-des-flt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione filtro        *
      *    *-----------------------------------------------------------*
       vis-des-flt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-flt (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-flt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice mnemonico             *
      *    *-----------------------------------------------------------*
       acc-cod-mne-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)    not  = spaces
                     go to acc-cod-mne-100.
           if        w-tes-cod-flt        =    9999999
                     go to acc-cod-mne-100
           else      go to acc-cod-mne-999.
       acc-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mne-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-mne-999.
       acc-cod-mne-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-mne (1)      .
       acc-cod-mne-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-tes-cod-mne (1)    to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-mne-100.
      *                  *---------------------------------------------*
      *                  * Test che il valore sia diverso da spaces, a *
      *                  * meno che non si sia in Up o che non si stia *
      *                  * trattando il filtro standard 9999999        *
      *                  *---------------------------------------------*
           if        w-tes-cod-mne (1)    not  = spaces
                     go to acc-cod-mne-600.
           if        w-tes-cod-flt        =    9999999
                     go to acc-cod-mne-600.
           if        v-key                =    "UP  "
                     go to acc-cod-mne-600
           else      go to acc-cod-mne-100.
       acc-cod-mne-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mne-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-mne-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-mne-100.
       acc-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice mnemonico          *
      *    *-----------------------------------------------------------*
       vis-cod-mne-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Filtro dell'utente           *
      *    *-----------------------------------------------------------*
       acc-flt-ute-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)    not  = spaces
                     go to acc-flt-ute-100.
           if        w-tes-cod-flt        =    9999999
                     go to acc-flt-ute-100
           else      go to acc-flt-ute-999.
       acc-flt-ute-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-flt-ute-lun    to   v-car                  .
           move      w-exp-flt-ute-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-flt-ute-tbl    to   v-txt                  .
           if        w-tes-flt-ute (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-flt-ute (1)    =    "#"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-flt-ute-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-flt-ute-999.
       acc-flt-ute-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  spaces         to   w-tes-flt-ute (1)
           else if   v-num                =    02
                     move  "#"            to   w-tes-flt-ute (1)
           else      move  spaces         to   w-tes-flt-ute (1)      .
       acc-flt-ute-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        w-tes-flt-ute (1)    not  = zero
                     go to acc-flt-ute-600.
           if        v-key                =    "UP  "
                     go to acc-flt-ute-600
           else      go to acc-flt-ute-100.
       acc-flt-ute-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-flt-ute-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-flt-ute-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-flt-ute-100.
       acc-flt-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Filtro dell'utente        *
      *    *-----------------------------------------------------------*
       vis-flt-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-flt-ute-lun    to   v-car                  .
           move      w-exp-flt-ute-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-flt-ute-tbl    to   v-txt                  .
           if        w-tes-flt-ute (1)    =    spaces
                     move  01             to   v-num
           else if   w-tes-flt-ute (1)    =    "#"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-flt-ute-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Serie elementi da selezionare              *
      *    *-----------------------------------------------------------*
       acc-esl-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "N"
                     go to acc-esl-dcp-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
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
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titoli all'interno del box                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       LISTA CODICI PRODOTTO A SCE
      -              "LTA                       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titoli                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura inferiore titoli             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero 1                              *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-n1v      .
           perform   acc-esl-dcp-900      thru acc-esl-dcp-909        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Primo elemento visualizzato : 1             *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-pev      .
      *                  *---------------------------------------------*
      *                  * Numero elemento in accettazione : 1         *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-esl-dcp-100.
       acc-esl-dcp-080.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina se necessario            *
      *              *-------------------------------------------------*
       acc-esl-dcp-082.
           move      w-acc-ser-edd-pev    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       11                   to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-nel    <    w-acc-ser-edd-c0a or
                     w-acc-ser-edd-nel    >    w-acc-ser-edd-c0b
                     go to acc-esl-dcp-084
           else      go to acc-esl-dcp-086.
       acc-esl-dcp-084.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-n1v      .
           subtract  1                    from w-acc-ser-edd-n1v      .
           divide    12                   into w-acc-ser-edd-n1v      .
           multiply  12                   by   w-acc-ser-edd-n1v      .
           add       1                    to   w-acc-ser-edd-n1v      .
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-pev      .
           perform   acc-esl-dcp-900      thru acc-esl-dcp-909        .
       acc-esl-dcp-086.
           go to     acc-esl-dcp-100.
       acc-esl-dcp-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione elementi selezionati            *
      *              *-------------------------------------------------*
           perform   acc-esl-dcp-950      thru acc-esl-dcp-959        .
      *              *-------------------------------------------------*
      *              * Accettazione linea                              *
      *              *-------------------------------------------------*
       acc-esl-dcp-120.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero di pagina                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina in corso di    *
      *                  * trattamento                                 *
      *                  *---------------------------------------------*
           if        w-acc-ser-edd-nel    >    60
                     move  6              to   w-acc-ser-edd-lt1
           else if   w-acc-ser-edd-nel    >    48
                     move  5              to   w-acc-ser-edd-lt1
           else if   w-acc-ser-edd-nel    >    36
                     move  4              to   w-acc-ser-edd-lt1
           else if   w-acc-ser-edd-nel    >    24
                     move  3              to   w-acc-ser-edd-lt1
           else if   w-acc-ser-edd-nel    >    12
                     move  2              to   w-acc-ser-edd-lt1
           else      move  1              to   w-acc-ser-edd-lt1      .
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina totale         *
      *                  *---------------------------------------------*
           if        w-acc-ser-els        >    60
                     move  6              to   w-acc-ser-edd-lt2
           else if   w-acc-ser-els        >    48
                     move  5              to   w-acc-ser-edd-lt2
           else if   w-acc-ser-els        >    36
                     move  4              to   w-acc-ser-edd-lt2
           else if   w-acc-ser-els        >    24
                     move  3              to   w-acc-ser-edd-lt2
           else if   w-acc-ser-els        >    12
                     move  2              to   w-acc-ser-edd-lt2
           else      move  1              to   w-acc-ser-edd-lt2      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-acc-ser-edd-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-esl-dcp-150.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti linea         *
      *                  *---------------------------------------------*
           move      w-acc-ser-sel-alf
                    (w-acc-ser-edd-nel)   to   w-acc-ser-edd-spe      .
       acc-esl-dcp-200.
      *                  *---------------------------------------------*
      *                  * Accettazione codice elemento                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri                  *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      w-acc-ser-sel-alf
                    (w-acc-ser-edd-nel)   to   w-cod-cod-dcp-alf      .
           move      w-acc-ser-sel-num
                    (w-acc-ser-edd-nel)   to   w-cod-cod-dcp-num      .
      *                          *-------------------------------------*
      *                          * Coordinate di posizionamento        *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           move      w-acc-ser-edd-c0r    to   w-cod-cod-dcp-lin      .
           move      09                   to   w-cod-cod-dcp-pos      .
           move      w-cod-cod-dcp-lin    to   w-cod-cod-dcp-dln      .
           move      w-cod-cod-dcp-pos    to   w-cod-cod-dcp-dps      .
           add       08                   to   w-cod-cod-dcp-dps      .
      *                          *-------------------------------------*
      *                          * Tasto 'Up'                          *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Down'                        *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Find'                        *
      *                          *-------------------------------------*
           move      "FIND"               to   v-pfk (03)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Insr' : disattivato          *
      *                          *-------------------------------------*
           move      spaces               to   v-pfk (04)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Do'                          *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Remove'                      *
      *                          *-------------------------------------*
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-nel)   not  = spaces
                     go to acc-esl-dcp-202.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-esl-dcp-204.
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0a)   =    spaces
                     go to acc-esl-dcp-204.
       acc-esl-dcp-202.
           move      "REMV"               to   v-pfk (06)             .
       acc-esl-dcp-204.
      *                          *-------------------------------------*
      *                          * Tasto 'Previous screen'             *
      *                          *-------------------------------------*
           move      "PRSC"               to   v-pfk (07)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Next screen'                 *
      *                          *-------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Back'                        *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    >    1
                     move  "BACK"         to   v-pfk (09)             .
           if        w-acc-ser-edd-nel    =    w-acc-ser-edd-max
                     go to acc-esl-dcp-206.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-nel)   =    spaces and
                     w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0a)   =    spaces
                     go to acc-esl-dcp-206.
      *                          *-------------------------------------*
      *                          * Tasto 'Tab'                         *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Exit'                        *
      *                          *-------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
       acc-esl-dcp-206.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-esl-dcp-208.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-esl-dcp-210.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-esl-dcp-212.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-esl-dcp-210.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-esl-dcp-208.
       acc-esl-dcp-212.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           move      w-cod-cod-dcp-num    to   v-num                  .
       acc-esl-dcp-215.
      *                      *-----------------------------------------*
      *                      * Valore in campo di destinazione         *
      *                      *-----------------------------------------*
           move      v-alf                to   w-acc-ser-sel-alf
                                              (w-acc-ser-edd-nel)     .
           move      v-num                to   w-acc-ser-sel-num
                                              (w-acc-ser-edd-nel)     .
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
______*              move  spaces         to   v-key
                     go to acc-esl-dcp-800.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
           if        v-key                =    spaces
                     go to acc-esl-dcp-425.
       acc-esl-dcp-220.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-esl-dcp-225.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-920      thru acc-esl-dcp-929        .
      *                          *-------------------------------------*
      *                          * Se sul primo : uscita               *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    =    zero
                     move  "UP  "         to   v-key
                     go to acc-esl-dcp-800
           else      go to acc-esl-dcp-080.
       acc-esl-dcp-225.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-esl-dcp-250.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-920      thru acc-esl-dcp-929        .
      *                          *-------------------------------------*
      *                          * Se sull'ultimo : uscita             *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-fce    =    spaces
                     add   1              to   w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-esl-dcp-800.
           if        w-acc-ser-edd-nel    =    1
                     go to acc-esl-dcp-230
           else      go to acc-esl-dcp-235.
       acc-esl-dcp-230.
           if        w-acc-ser-sel-alf (1)
                                          =    spaces and
                     w-acc-ser-sel-alf (2)
                                          =    spaces
                     move  "DOWN"         to   v-key
                     go to acc-esl-dcp-800
           else      go to acc-esl-dcp-080.
       acc-esl-dcp-235.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0a)   =    spaces and
                     w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0b)   =    spaces
                     move  "DOWN"         to   v-key
                     go to acc-esl-dcp-800
           else      go to acc-esl-dcp-080.
       acc-esl-dcp-250.
      *                      *-----------------------------------------*
      *                      * Se Insr                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-esl-dcp-275.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-940      thru acc-esl-dcp-949        .
           go to     acc-esl-dcp-080.
       acc-esl-dcp-275.
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-esl-dcp-300.
      *                          *-------------------------------------*
      *                          * Controlli per tasto Do              *
      *                          *-------------------------------------*
       acc-esl-dcp-280.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-920      thru acc-esl-dcp-929        .
      *                          *-------------------------------------*
      *                          * Normalizzazione function-key        *
      *                          *-------------------------------------*
           move      spaces               to   v-key                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     acc-esl-dcp-800.
       acc-esl-dcp-300.
      *                      *-----------------------------------------*
      *                      * Se Remv                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-esl-dcp-325.
      *                          *-------------------------------------*
      *                          * Compattamento in ogni caso          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-930      thru acc-esl-dcp-939        .
      *                          *-------------------------------------*
      *                          * A reimpostare                       *
      *                          *-------------------------------------*
           go to     acc-esl-dcp-080.
       acc-esl-dcp-325.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-esl-dcp-350.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-920      thru acc-esl-dcp-929        .
      *                          *-------------------------------------*
      *                          * Se su prima facciata : uscita       *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    not  > 14
                     move  "UP  "         to   v-key
                     go to acc-esl-dcp-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * precedente                          *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           divide    14                   into w-acc-ser-edd-nel      .
           subtract  1                    from w-acc-ser-edd-nel      .
           multiply  14                   by   w-acc-ser-edd-nel      .
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-esl-dcp-080.
       acc-esl-dcp-350.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-esl-dcp-375.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-920      thru acc-esl-dcp-929        .
      *                          *-------------------------------------*
      *                          * Se su ultima facciata : uscita      *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
           divide    12                   into w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           multiply  12                   by   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-esl-dcp-800.
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0a)   =    spaces
                     move  "DOWN"         to   v-key
                     go to acc-esl-dcp-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * successiva                          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-nel      .
           go to     acc-esl-dcp-080.
       acc-esl-dcp-375.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-esl-dcp-400.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-920      thru acc-esl-dcp-929        .
      *                          *-------------------------------------*
      *                          * Al primo elemento                   *
      *                          *-------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
           go to     acc-esl-dcp-080.
       acc-esl-dcp-400.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-esl-dcp-425.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-esl-dcp-920      thru acc-esl-dcp-929        .
      *                          *-------------------------------------*
      *                          * Dopo l'ultimo elemento inserito     *
      *                          *-------------------------------------*
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-max)   not  = spaces
                     move  w-acc-ser-edd-max
                                          to   w-acc-ser-edd-nel
                     go to acc-esl-dcp-080.
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-nel      .
       acc-esl-dcp-405.
           if        w-acc-ser-edd-nel    =    zero
                     go to acc-esl-dcp-410.
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-nel)   =    spaces
                     subtract  1          from w-acc-ser-edd-nel
                     go to     acc-esl-dcp-405.
       acc-esl-dcp-410.
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-esl-dcp-080.
       acc-esl-dcp-425.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      w-acc-ser-sel-num
                    (w-acc-ser-edd-nel)   to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                          *-------------------------------------*
      *                          * Valori letti                        *
      *                          *-------------------------------------*
           move      w-let-arc-dcp-des    to   w-acc-ser-sel-des
                                              (w-acc-ser-edd-nel)     .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           perform   acc-esl-dcp-910      thru acc-esl-dcp-919        .
      *                          *-------------------------------------*
      *                          * Se record non trovato : reimposta-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-esl-dcp-200.
      *                          *-------------------------------------*
      *                          * Controllo valore impostato          *
      *                          *-------------------------------------*
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-nel)   =    spaces
                     go to acc-esl-dcp-450
           else      go to acc-esl-dcp-500.
       acc-esl-dcp-450.
      *                          *-------------------------------------*
      *                          * Se impostato zero                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il valore precedente era a   *
      *                              * zero : come per Down            *
      *                              *---------------------------------*
           if        w-acc-ser-edd-spe    =    spaces
                     move  "DOWN"         to   v-key
                     go to acc-esl-dcp-225.
      *                              *---------------------------------*
      *                              * Altrimenti come per Remv        *
      *                              *---------------------------------*
           move      "REMV"               to   v-key                  .
           go to     acc-esl-dcp-300.
       acc-esl-dcp-500.
      *                  *---------------------------------------------*
      *                  * Passaggio ad elemento successivo            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione elementi selezionati    *
      *                      *-----------------------------------------*
           perform   acc-esl-dcp-960      thru acc-esl-dcp-969        .
      *                      *-----------------------------------------*
      *                      * Incremento numero elemento              *
      *                      *-----------------------------------------*
           add       1                    to   w-acc-ser-edd-nel      .
      *                      *-----------------------------------------*
      *                      * Se fine elementi : uscita               *
      *                      *-----------------------------------------*
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  spaces         to   v-key
                     go to acc-esl-dcp-800.
      *                      *-----------------------------------------*
      *                      * Altrimenti : ad impostazione linea      *
      *                      *-----------------------------------------*
           go to     acc-esl-dcp-080.
       acc-esl-dcp-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio v-key                           *
      *                  *---------------------------------------------*
           move      v-key                to   w-acc-ser-edd-svk      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino  immagine video                  *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino v-key                            *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-svk    to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Fine routine                                *
      *                  *---------------------------------------------*
           go to     acc-esl-dcp-999.
       acc-esl-dcp-890.
      *              *-------------------------------------------------*
      *              * Subroutines interne                             *
      *              *-------------------------------------------------*
       acc-esl-dcp-900.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero w-acc-ser-edd-n1v              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-c0p      .
           subtract  1                    from w-acc-ser-edd-c0p      .
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-c0q      .
           add       12                   to   w-acc-ser-edd-c0q      .
       acc-esl-dcp-901.
           add       1                    to   w-acc-ser-edd-c0p      .
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-max
                     go to acc-esl-dcp-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-esl-dcp-909.
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nev    to   w-acc-ser-edd-c0r      .
       acc-esl-dcp-903.
           if        w-acc-ser-edd-c0r    >    12
                     subtract  12         from w-acc-ser-edd-c0r
                     go to     acc-esl-dcp-903.
           add       6                    to   w-acc-ser-edd-c0r      .
           perform   acc-esl-dcp-910      thru acc-esl-dcp-919        .
           go to     acc-esl-dcp-901.
       acc-esl-dcp-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-esl-dcp-909.
           add       1                    to   w-acc-ser-edd-c0r      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0p      .
           go to     acc-esl-dcp-905.
       acc-esl-dcp-909.
           exit.
       acc-esl-dcp-910.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elemento indirizzato da     *
      *                  * w-acc-ser-edd-nev a linea w-acc-ser-edd-c0r *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-led      .
      *                      *-----------------------------------------*
      *                      * Composizione linea da visualizzare      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing numero riga                 *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-acc-ser-edd-nev    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-rig      .
      *                          *-------------------------------------*
      *                          * Codice alfanumerico prodotto        *
      *                          *-------------------------------------*
           move      w-acc-ser-sel-alf
                    (w-acc-ser-edd-nev)   to   w-acc-ser-edd-cod      .
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
           move      w-acc-ser-sel-des
                    (w-acc-ser-edd-nev)   to   w-acc-ser-edd-des      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-acc-ser-edd-led    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-esl-dcp-919.
           exit.
       acc-esl-dcp-920.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec se necessario             *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-fce      .
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-nec)   not  = spaces
                     go to acc-esl-dcp-929.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-esl-dcp-929.
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0b)   =    spaces
                     go to acc-esl-dcp-929.
           perform   acc-esl-dcp-930      thru acc-esl-dcp-939        .
           move      "#"                  to   w-acc-ser-edd-fce      .
       acc-esl-dcp-929.
           exit.
       acc-esl-dcp-930.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec in ogni caso              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
       acc-esl-dcp-931.
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-esl-dcp-932.
           move      w-acc-ser-ele
                    (w-acc-ser-edd-c0b)   to   w-acc-ser-ele
                                              (w-acc-ser-edd-c0a)     .
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0b)   =    spaces
                     go to acc-esl-dcp-933.
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           go to     acc-esl-dcp-931.
       acc-esl-dcp-932.
           move      spaces               to   w-acc-ser-sel-alf
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   w-acc-ser-sel-des
                                              (w-acc-ser-edd-c0a)     .
       acc-esl-dcp-933.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-esl-dcp-934.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-esl-dcp-934.
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       6                    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0c      .
           add       1                    to   w-acc-ser-edd-c0c      .
       acc-esl-dcp-935.
           if        w-acc-ser-edd-c0a    =    12
                     go to acc-esl-dcp-936.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0c      .
           go to     acc-esl-dcp-935.
       acc-esl-dcp-936.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-esl-dcp-937.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-esl-dcp-937.
           subtract  w-acc-ser-edd-c0a    from 12
                                        giving w-acc-ser-edd-c0b      .
           add       w-acc-ser-edd-nec
                     w-acc-ser-edd-c0b  giving w-acc-ser-edd-c0c      .
           if        w-acc-ser-edd-c0c    >    w-acc-ser-edd-max
                     go to acc-esl-dcp-938.
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0c)   =    spaces
                     go to acc-esl-dcp-938.
           move      w-acc-ser-edd-c0c    to   w-acc-ser-edd-nev      .
           move      18                   to   w-acc-ser-edd-c0r      .
           perform   acc-esl-dcp-910      thru acc-esl-dcp-919        .
           go to     acc-esl-dcp-939.
       acc-esl-dcp-938.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-esl-dcp-939.
           exit.
       acc-esl-dcp-940.
      *                  *---------------------------------------------*
      *                  * Inserimento dell' elemento indirizzato da   *
      *                  * w-acc-ser-edd-nec                           *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
       acc-esl-dcp-941.
           move      w-acc-ser-ele
                    (w-acc-ser-edd-c0a)   to   w-acc-ser-ele
                                              (w-acc-ser-edd-c0b)     .
           if        w-acc-ser-edd-c0a    =    w-acc-ser-edd-nec
                     go to acc-esl-dcp-942.
           subtract  1                    from w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           go to     acc-esl-dcp-941.
       acc-esl-dcp-942.
           move      spaces               to   w-acc-ser-sel-alf
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   w-acc-ser-sel-des
                                              (w-acc-ser-edd-c0a)     .
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-esl-dcp-943.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-esl-dcp-943.
           add       6                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    =    18
                     go to acc-esl-dcp-945.
           move      17                   to   w-acc-ser-edd-c0b      .
           move      18                   to   w-acc-ser-edd-c0c      .
       acc-esl-dcp-944.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        w-acc-ser-edd-c0b    =    w-acc-ser-edd-c0a
                     go to acc-esl-dcp-945.
           subtract  1                    from w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0c      .
           go to     acc-esl-dcp-944.
       acc-esl-dcp-945.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0a    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-esl-dcp-949.
           exit.
       acc-esl-dcp-950.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi selezionati        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-acc-ser-els        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-esl-dcp-959.
           exit.
       acc-esl-dcp-960.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi da trattare        *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c0a      .
       acc-esl-dcp-962.
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-esl-dcp-964.
           if        w-acc-ser-sel-alf
                    (w-acc-ser-edd-c0a)   =    spaces
                     go to acc-esl-dcp-964.
           go to     acc-esl-dcp-962.
       acc-esl-dcp-964.
           subtract  1                    from w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-els          .
      *              *-------------------------------------------------*
      *              * Visualizzazione elementi selezionati            *
      *              *-------------------------------------------------*
           perform   acc-esl-dcp-950      thru acc-esl-dcp-959        .
       acc-esl-dcp-969.
           exit.
       acc-esl-dcp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione codici selezionati                        *
      *    *-----------------------------------------------------------*
       vis-esl-dcp-000.
       vis-esl-dcp-999.
           exit.
           
      *    *===========================================================*
      *    * Accettazione campo testata : Tipo ordinamento, selezione  *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-tip-ord-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-tip-ord-999.
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      "1234"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      w-tes-tip-ord (1)    to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-ord-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-ord-999.
       acc-tip-ord-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-tip-ord (1)      .
       acc-tip-ord-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        w-tes-tip-ord (1)    not  = zero
                     go to acc-tip-ord-600.
           if        v-key                =    "UP  "
                     go to acc-tip-ord-600
           else      go to acc-tip-ord-100.
       acc-tip-ord-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ord-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-ord-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-ord-100.
       acc-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo ordinamento          *
      *    *-----------------------------------------------------------*
       vis-tip-ord-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      w-tes-tip-ord (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Classe min                 *
      *    *-----------------------------------------------------------*
       acc-cla-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-cla-min-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-cla-min-999.
       acc-cla-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp1-ope      .
           move      w-tes-cla-min (1)    to   w-cod-mne-zp1-cla      .
           move      15                   to   w-cod-mne-zp1-lin      .
           move      30                   to   w-cod-mne-zp1-pos      .
           move      zero                 to   w-cod-mne-zp1-dln      .
           move      zero                 to   w-cod-mne-zp1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp1-cll-000  thru cod-mne-zp1-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp1-foi-000  thru cod-mne-zp1-foi-999    .
       acc-cla-min-110.
           perform   cod-mne-zp1-cll-000  thru cod-mne-zp1-cll-999    .
           if        w-cod-mne-zp1-ope    =    "F+"
                     go to acc-cla-min-115.
           if        w-cod-mne-zp1-ope    =    "AC"
                     go to acc-cla-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cla-min-115.
           perform   cod-mne-zp1-foi-000  thru cod-mne-zp1-foi-999    .
           go to     acc-cla-min-110.
       acc-cla-min-120.
           move      w-cod-mne-zp1-cla    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cla-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cla-min-999.
       acc-cla-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cla-min (1)      .
       acc-cla-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cla-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di accettabilita'      *
      *                  * codice catalogo                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-acc-cod-cpv          .
       acc-cla-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cla-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cla-min-100.
       acc-cla-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Classe min                *
      *    *-----------------------------------------------------------*
       vis-cla-min-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cla-min (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cla-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Classe max                 *
      *    *-----------------------------------------------------------*
       acc-cla-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-cla-max-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-cla-max-999.
       acc-cla-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp1-ope      .
           move      w-tes-cla-max (1)    to   w-cod-mne-zp1-cla      .
           move      15                   to   w-cod-mne-zp1-lin      .
           move      54                   to   w-cod-mne-zp1-pos      .
           move      zero                 to   w-cod-mne-zp1-dln      .
           move      zero                 to   w-cod-mne-zp1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp1-cll-000  thru cod-mne-zp1-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp1-foi-000  thru cod-mne-zp1-foi-999    .
       acc-cla-max-110.
           perform   cod-mne-zp1-cll-000  thru cod-mne-zp1-cll-999    .
           if        w-cod-mne-zp1-ope    =    "F+"
                     go to acc-cla-max-115.
           if        w-cod-mne-zp1-ope    =    "AC"
                     go to acc-cla-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cla-max-115.
           perform   cod-mne-zp1-foi-000  thru cod-mne-zp1-foi-999    .
           go to     acc-cla-max-110.
       acc-cla-max-120.
           move      w-cod-mne-zp1-cla    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cla-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cla-max-999.
       acc-cla-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cla-max (1)      .
       acc-cla-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cla-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cla-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cla-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cla-max-100.
       acc-cla-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Classe max                *
      *    *-----------------------------------------------------------*
       vis-cla-max-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-tes-cla-max (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cla-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Gruppo min                 *
      *    *-----------------------------------------------------------*
       acc-gru-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-gru-min-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-gru-min-999.
       acc-gru-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp2-ope      .
           move      w-tes-cla-min (1)    to   w-cod-mne-zp2-cla      .
           move      w-tes-gru-min (1)    to   w-cod-mne-zp2-gru      .
           move      16                   to   w-cod-mne-zp2-lin      .
           move      30                   to   w-cod-mne-zp2-pos      .
           move      zero                 to   w-cod-mne-zp2-dln      .
           move      zero                 to   w-cod-mne-zp2-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp2-cll-000  thru cod-mne-zp2-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp2-foi-000  thru cod-mne-zp2-foi-999    .
       acc-gru-min-110.
           perform   cod-mne-zp2-cll-000  thru cod-mne-zp2-cll-999    .
           if        w-cod-mne-zp2-ope    =    "F+"
                     go to acc-gru-min-115.
           if        w-cod-mne-zp2-ope    =    "AC"
                     go to acc-gru-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-gru-min-115.
           perform   cod-mne-zp2-foi-000  thru cod-mne-zp2-foi-999    .
           go to     acc-gru-min-110.
       acc-gru-min-120.
           move      w-cod-mne-zp2-gru    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-gru-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-gru-min-999.
       acc-gru-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-gru-min (1)      .
       acc-gru-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-gru-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-gru-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-gru-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-gru-min-100.
       acc-gru-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Gruppo min                *
      *    *-----------------------------------------------------------*
       vis-gru-min-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-gru-min (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-gru-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Gruppo max                 *
      *    *-----------------------------------------------------------*
       acc-gru-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-gru-max-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-gru-max-999.
       acc-gru-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp2-ope      .
           move      w-tes-cla-max (1)    to   w-cod-mne-zp2-cla      .
           move      w-tes-gru-max (1)    to   w-cod-mne-zp2-gru      .
           move      16                   to   w-cod-mne-zp2-lin      .
           move      54                   to   w-cod-mne-zp2-pos      .
           move      zero                 to   w-cod-mne-zp2-dln      .
           move      zero                 to   w-cod-mne-zp2-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp2-cll-000  thru cod-mne-zp2-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp2-foi-000  thru cod-mne-zp2-foi-999    .
       acc-gru-max-110.
           perform   cod-mne-zp2-cll-000  thru cod-mne-zp2-cll-999    .
           if        w-cod-mne-zp2-ope    =    "F+"
                     go to acc-gru-max-115.
           if        w-cod-mne-zp2-ope    =    "AC"
                     go to acc-gru-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-gru-max-115.
           perform   cod-mne-zp2-foi-000  thru cod-mne-zp2-foi-999    .
           go to     acc-gru-max-110.
       acc-gru-max-120.
           move      w-cod-mne-zp2-gru    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-gru-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-gru-max-999.
       acc-gru-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-gru-max (1)      .
       acc-gru-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-gru-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-gru-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-gru-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-gru-max-100.
       acc-gru-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Gruppo max                *
      *    *-----------------------------------------------------------*
       vis-gru-max-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-tes-gru-max (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-gru-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Sottogruppo min            *
      *    *-----------------------------------------------------------*
       acc-sgr-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-sgr-min-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-sgr-min-999.
       acc-sgr-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp3-ope      .
           move      w-tes-cla-min (1)    to   w-cod-mne-zp3-cla      .
           move      w-tes-gru-min (1)    to   w-cod-mne-zp3-gru      .
           move      w-tes-sgr-min (1)    to   w-cod-mne-zp3-sgr      .
           move      17                   to   w-cod-mne-zp3-lin      .
           move      30                   to   w-cod-mne-zp3-pos      .
           move      zero                 to   w-cod-mne-zp3-dln      .
           move      zero                 to   w-cod-mne-zp3-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp3-cll-000  thru cod-mne-zp3-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp3-foi-000  thru cod-mne-zp3-foi-999    .
       acc-sgr-min-110.
           perform   cod-mne-zp3-cll-000  thru cod-mne-zp3-cll-999    .
           if        w-cod-mne-zp3-ope    =    "F+"
                     go to acc-sgr-min-115.
           if        w-cod-mne-zp3-ope    =    "AC"
                     go to acc-sgr-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgr-min-115.
           perform   cod-mne-zp3-foi-000  thru cod-mne-zp3-foi-999    .
           go to     acc-sgr-min-110.
       acc-sgr-min-120.
           move      w-cod-mne-zp3-sgr    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-min-999.
       acc-sgr-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sgr-min (1)      .
       acc-sgr-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sgr-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sgr-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sgr-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sgr-min-100.
       acc-sgr-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sottogruppo min           *
      *    *-----------------------------------------------------------*
       vis-sgr-min-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-sgr-min (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgr-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Sottogruppo max            *
      *    *-----------------------------------------------------------*
       acc-sgr-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-sgr-max-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-sgr-max-999.
       acc-sgr-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp3-ope      .
           move      w-tes-cla-max (1)    to   w-cod-mne-zp3-cla      .
           move      w-tes-gru-max (1)    to   w-cod-mne-zp3-gru      .
           move      w-tes-sgr-max (1)    to   w-cod-mne-zp3-sgr      .
           move      17                   to   w-cod-mne-zp3-lin      .
           move      54                   to   w-cod-mne-zp3-pos      .
           move      zero                 to   w-cod-mne-zp3-dln      .
           move      zero                 to   w-cod-mne-zp3-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zp3-cll-000  thru cod-mne-zp3-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zp3-foi-000  thru cod-mne-zp3-foi-999    .
       acc-sgr-max-110.
           perform   cod-mne-zp3-cll-000  thru cod-mne-zp3-cll-999    .
           if        w-cod-mne-zp3-ope    =    "F+"
                     go to acc-sgr-max-115.
           if        w-cod-mne-zp3-ope    =    "AC"
                     go to acc-sgr-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgr-max-115.
           perform   cod-mne-zp3-foi-000  thru cod-mne-zp3-foi-999    .
           go to     acc-sgr-max-110.
       acc-sgr-max-120.
           move      w-cod-mne-zp3-sgr    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sgr-max-999.
       acc-sgr-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-sgr-max (1)      .
       acc-sgr-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sgr-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sgr-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sgr-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sgr-max-100.
       acc-sgr-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sottogruppo max           *
      *    *-----------------------------------------------------------*
       vis-sgr-max-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-tes-sgr-max (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sgr-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice min                 *
      *    *-----------------------------------------------------------*
       acc-cod-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-cod-min-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-cod-min-999.
       acc-cod-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "L"                  to   w-cod-cod-dcp-tac      .
           move      zero                 to   w-cod-cod-dcp-num      .
           move      w-tes-cod-min (1)    to   w-cod-cod-dcp-alf      .
           move      19                   to   w-cod-cod-dcp-lin      .
           move      30                   to   w-cod-cod-dcp-pos      .
           move      zero                 to   w-cod-cod-dcp-dln      .
           move      zero                 to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-cod-min-110.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-cod-min-115.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-cod-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-min-115.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-cod-min-110.
       acc-cod-min-120.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-min-999.
       acc-cod-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-min (1)      .
       acc-cod-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-min-100.
       acc-cod-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice min                *
      *    *-----------------------------------------------------------*
       vis-cod-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-min (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice max                 *
      *    *-----------------------------------------------------------*
       acc-cod-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-cod-max-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-cod-max-999.
       acc-cod-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "L"                  to   w-cod-cod-dcp-tac      .
           move      zero                 to   w-cod-cod-dcp-num      .
           move      w-tes-cod-max (1)    to   w-cod-cod-dcp-alf      .
           move      19                   to   w-cod-cod-dcp-lin      .
           move      54                   to   w-cod-cod-dcp-pos      .
           move      zero                 to   w-cod-cod-dcp-dln      .
           move      zero                 to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-cod-max-110.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-cod-max-115.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-cod-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-max-115.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-cod-max-110.
       acc-cod-max-120.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-max-999.
       acc-cod-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cod-max (1)      .
       acc-cod-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-max-100.
       acc-cod-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice max                *
      *    *-----------------------------------------------------------*
       vis-cod-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-tes-cod-max (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Descrizione min            *
      *    *-----------------------------------------------------------*
       acc-des-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-des-min-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-des-min-999.
       acc-des-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-min (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-min-999.
       acc-des-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-min (1)      .
       acc-des-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-min-100.
       acc-des-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione min           *
      *    *-----------------------------------------------------------*
       vis-des-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-min (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Descrizione max            *
      *    *-----------------------------------------------------------*
       acc-des-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-des-max-999.
           if        w-tes-cod-cpv (1)    not  = spaces
                     go to acc-des-max-999.
       acc-des-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-max (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-max-999.
       acc-des-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-max (1)      .
       acc-des-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-des-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-des-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-max-100.
       acc-des-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione max           *
      *    *-----------------------------------------------------------*
       vis-des-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-tes-des-max (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data inizio commercializzazione min  *
      *    *-----------------------------------------------------------*
       acc-icm-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-icm-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-icm-min (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-icm-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-icm-min-999.
       acc-icm-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-icm-min (1)      .
       acc-icm-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-icm-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-icm-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-icm-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-icm-min-100.
       acc-icm-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data inizio commercializ. min     *
      *    *-----------------------------------------------------------*
       vis-icm-min-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-icm-min (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-icm-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data inizio commercializzazione max  *
      *    *-----------------------------------------------------------*
       acc-icm-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-icm-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-icm-max (1)    to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-icm-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-icm-max-999.
       acc-icm-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-tes-icm-max (1)      .
       acc-icm-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-icm-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-icm-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-icm-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-icm-max-100.
       acc-icm-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data inizio commercializ. max     *
      *    *-----------------------------------------------------------*
       vis-icm-max-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-tes-icm-max (1)    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-icm-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Tipo prodotto                *
      *    *-----------------------------------------------------------*
       acc-tip-pro-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-pro-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pro-lun    to   v-car                  .
           move      w-exp-tip-pro-num    to   v-ldt                  .
           move      "TMSIE#"             to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-pro-tbl    to   v-txt                  .
           if        w-tes-tip-pro (1)    =    99
                     move  01             to   v-num
           else if   w-tes-tip-pro (1)    =    01
                     move  02             to   v-num
           else if   w-tes-tip-pro (1)    =    02
                     move  03             to   v-num
           else if   w-tes-tip-pro (1)    =    03
                     move  04             to   v-num
           else if   w-tes-tip-pro (1)    =    09
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-pro-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-pro-999.
       acc-tip-pro-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  99             to   w-tes-tip-pro (1)
           else if   v-num                =    02
                     move  01             to   w-tes-tip-pro (1)
           else if   v-num                =    03
                     move  02             to   w-tes-tip-pro (1)
           else if   v-num                =    04
                     move  03             to   w-tes-tip-pro (1)
           else if   v-num                =    05
                     move  09             to   w-tes-tip-pro (1)
           else      move  zero           to   w-tes-tip-pro (1)      .
       acc-tip-pro-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-tes-tip-pro (1)    not  = zero
                     go to acc-tip-pro-600.
           if        v-key                =    "UP  "
                     go to acc-tip-pro-600
           else      go to acc-tip-pro-100.
       acc-tip-pro-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-pro-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-pro-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-pro-100.
       acc-tip-pro-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo prodotto             *
      *    *-----------------------------------------------------------*
       vis-tip-pro-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pro-lun    to   v-car                  .
           move      w-exp-tip-pro-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-pro-tbl    to   v-txt                  .
           if        w-tes-tip-pro (1)    =    99
                     move  01             to   v-num
           else if   w-tes-tip-pro (1)    =    01
                     move  02             to   v-num
           else if   w-tes-tip-pro (1)    =    02
                     move  03             to   v-num
           else if   w-tes-tip-pro (1)    =    03
                     move  04             to   v-num
           else if   w-tes-tip-pro (1)    =    09
                     move  05             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-pro-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice iva                   *
      *    *-----------------------------------------------------------*
       acc-cod-iva-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-iva-100.
      *              *-------------------------------------------------*
      *              * Editing preliminare per l'accettazione          *
      *              *-------------------------------------------------*
           move      w-tes-cod-iva (1)    to   w-edt-iva-cod          .
           perform   edt-cod-iva-000      thru edt-cod-iva-999        .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zci-ope      .
           move      w-edt-iva-cie        to   w-cod-mne-zci-cod      .
           move      09                   to   w-cod-mne-zci-lin      .
           move      30                   to   w-cod-mne-zci-pos      .
           move      09                   to   w-cod-mne-zci-dln      .
           move      41                   to   w-cod-mne-zci-dps      .
           move      "<BD"                to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
       acc-cod-iva-110.
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
           if        w-cod-mne-zci-ope    =    "F+"
                     go to acc-cod-iva-115.
           if        w-cod-mne-zci-ope    =    "AC"
                     go to acc-cod-iva-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-iva-115.
           perform   cod-mne-zci-foi-000  thru cod-mne-zci-foi-999    .
           go to     acc-cod-iva-110.
       acc-cod-iva-120.
      *              *-------------------------------------------------*
      *              * Editing valore impostato                        *
      *              *-------------------------------------------------*
           move      w-cod-mne-zci-cod    to   w-edt-iva-cie          .
           perform   edt-iva-cod-000      thru edt-iva-cod-999        .
           move      w-edt-iva-cod        to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-iva-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-iva-999.
       acc-cod-iva-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-iva (1)      .
       acc-cod-iva-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura descrizione                         *
      *                  *---------------------------------------------*
           move      w-tes-cod-iva (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-cod-iva-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-iva-000      thru vis-des-iva-999        .
      *                  *---------------------------------------------*
      *                  * Se codice iva non esistente : reimpostaz.   *
      *                  *---------------------------------------------*
           if        w-let-arc-zci-flg    not  = spaces
                     go to acc-cod-iva-100.
       acc-cod-iva-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-iva-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-iva-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-iva-100.
       acc-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice iva                *
      *    *-----------------------------------------------------------*
       vis-cod-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-iva (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-iva-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione iva           *
      *    *-----------------------------------------------------------*
       vis-des-iva-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-iva-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-iva-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice contropartita         *
      *    *-----------------------------------------------------------*
       acc-ctp-ven-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ctp-ven-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ctp-ven (1)    to   w-cod-mne-pdc-cod      .
           move      10                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      10                   to   w-cod-mne-pdc-dln      .
           move      41                   to   w-cod-mne-pdc-dps      .
           move      "B"                  to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
       acc-ctp-ven-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ctp-ven-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ctp-ven-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ctp-ven-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ctp-ven-110.
       acc-ctp-ven-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-ven-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-ven-999.
       acc-ctp-ven-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ctp-ven (1)      .
       acc-ctp-ven-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-des-ctp-000      thru vis-des-ctp-999        .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-ctp-ven-100.
       acc-ctp-ven-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ctp-ven-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ctp-ven-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ctp-ven-100.
       acc-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice contropartita      *
      *    *-----------------------------------------------------------*
       vis-ctp-ven-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ctp-ven (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione contropartita *
      *    *-----------------------------------------------------------*
       vis-des-ctp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ctp-ven-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-ctp-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Accettazione campo testata : Unita' di misura             *
      *    *-----------------------------------------------------------*
       acc-umi-ven-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-umi-ven-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-umi-ope      .
           move      w-tes-umi-ven (1)    to   w-cod-cod-umi-cod      .
           move      12                   to   w-cod-cod-umi-lin      .
           move      30                   to   w-cod-cod-umi-pos      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
       acc-umi-ven-110.
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           if        w-cod-cod-umi-ope    =    "F+"
                     go to acc-umi-ven-115.
           if        w-cod-cod-umi-ope    =    "AC"
                     go to acc-umi-ven-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-umi-ven-115.
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
           go to     acc-umi-ven-110.
       acc-umi-ven-120.
           move      w-cod-cod-umi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-umi-ven-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-umi-ven-999.
       acc-umi-ven-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-umi-ven (1)      .
       acc-umi-ven-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella unita' di misura            *
      *                  *---------------------------------------------*
           move      w-tes-umi-ven (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati associati al codice     *
      *                  * unita' di misura                            *
      *                  *---------------------------------------------*
           move      w-let-arc-zum-des    to   w-tes-umi-ven-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione unita' di mi-   *
      *                  * sura                                        *
      *                  *---------------------------------------------*
           perform   vis-des-umi-000      thru vis-des-umi-999        .
      *                  *---------------------------------------------*
      *                  * Se unita' di misura non esistente : reimpo- *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-let-arc-zum-flg    not  = spaces
                     go to acc-umi-ven-100.
       acc-umi-ven-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-umi-ven-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-umi-ven-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-umi-ven-100.
       acc-umi-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Unita' di misura                  *
      *    *-----------------------------------------------------------*
       vis-umi-ven-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-umi-ven (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umi-ven-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione unita' di misura      *
      *    *-----------------------------------------------------------*
       vis-des-umi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-umi-ven-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-umi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Specifica libera                     *
      *    *-----------------------------------------------------------*
       acc-spc-lib-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-spc-lib-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-spc-lib (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-spc-lib-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-spc-lib-999.
       acc-spc-lib-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-spc-lib (1)      .
       acc-spc-lib-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-spc-lib-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-spc-lib-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-spc-lib-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-spc-lib-100.
       acc-spc-lib-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Specifica libera                  *
      *    *-----------------------------------------------------------*
       vis-spc-lib-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-spc-lib (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-spc-lib-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 1          *
      *    *-----------------------------------------------------------*
       acc-cod-s01-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s01-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zps-001-ope      .
           move      w-tes-cod-s01 (1)    to   w-cmn-zps-001-cod      .
           move      15                   to   w-cmn-zps-001-lin      .
           move      30                   to   w-cmn-zps-001-pos      .
           move      15                   to   w-cmn-zps-001-dln      .
           move      41                   to   w-cmn-zps-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zps-001-cll-000  thru cmn-zps-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zps-001-foi-000  thru cmn-zps-001-foi-999    .
       acc-cod-s01-110.
           perform   cmn-zps-001-cll-000  thru cmn-zps-001-cll-999    .
           if        w-cmn-zps-001-ope    =    "F+"
                     go to acc-cod-s01-115.
           if        w-cmn-zps-001-ope    =    "AC"
                     go to acc-cod-s01-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s01-115.
           perform   cmn-zps-001-foi-000  thru cmn-zps-001-foi-999    .
           go to     acc-cod-s01-110.
       acc-cod-s01-120.
           move      w-cmn-zps-001-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s01-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s01-999.
       acc-cod-s01-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s01 (1)      .
       acc-cod-s01-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zps-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zps-cod      .
           perform   let-arc-zps-000      thru let-arc-zps-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zps-des    to   w-tes-cod-s01-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs1-000      thru vis-des-cs1-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zps-flg    not  = spaces
                     go to acc-cod-s01-100.
       acc-cod-s01-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s01-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s01-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s01-100.
       acc-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 1       *
      *    *-----------------------------------------------------------*
       vis-cod-s01-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s01 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s01-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 1                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs1-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-s01-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs1-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 2          *
      *    *-----------------------------------------------------------*
       acc-cod-s02-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s02-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zps-002-ope      .
           move      w-tes-cod-s02 (1)    to   w-cmn-zps-002-cod      .
           move      16                   to   w-cmn-zps-002-lin      .
           move      30                   to   w-cmn-zps-002-pos      .
           move      16                   to   w-cmn-zps-002-dln      .
           move      41                   to   w-cmn-zps-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zps-002-cll-000  thru cmn-zps-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zps-002-foi-000  thru cmn-zps-002-foi-999    .
       acc-cod-s02-110.
           perform   cmn-zps-002-cll-000  thru cmn-zps-002-cll-999    .
           if        w-cmn-zps-002-ope    =    "F+"
                     go to acc-cod-s02-115.
           if        w-cmn-zps-002-ope    =    "AC"
                     go to acc-cod-s02-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s02-115.
           perform   cmn-zps-002-foi-000  thru cmn-zps-002-foi-999    .
           go to     acc-cod-s02-110.
       acc-cod-s02-120.
           move      w-cmn-zps-002-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s02-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s02-999.
       acc-cod-s02-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s02 (1)      .
       acc-cod-s02-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zps-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zps-cod      .
           perform   let-arc-zps-000      thru let-arc-zps-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zps-des    to   w-tes-cod-s02-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs2-000      thru vis-des-cs2-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zps-flg    not  = spaces
                     go to acc-cod-s02-100.
       acc-cod-s02-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s02-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s02-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s02-100.
       acc-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 2       *
      *    *-----------------------------------------------------------*
       vis-cod-s02-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s02 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s02-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 2                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs2-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-s02-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs2-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice statistico 3          *
      *    *-----------------------------------------------------------*
       acc-cod-s03-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-s03-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zps-003-ope      .
           move      w-tes-cod-s03 (1)    to   w-cmn-zps-003-cod      .
           move      17                   to   w-cmn-zps-003-lin      .
           move      30                   to   w-cmn-zps-003-pos      .
           move      17                   to   w-cmn-zps-003-dln      .
           move      41                   to   w-cmn-zps-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cmn-zps-003-cll-000  thru cmn-zps-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zps-003-foi-000  thru cmn-zps-003-foi-999    .
       acc-cod-s03-110.
           perform   cmn-zps-003-cll-000  thru cmn-zps-003-cll-999    .
           if        w-cmn-zps-003-ope    =    "F+"
                     go to acc-cod-s03-115.
           if        w-cmn-zps-003-ope    =    "AC"
                     go to acc-cod-s03-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s03-115.
           perform   cmn-zps-003-foi-000  thru cmn-zps-003-foi-999    .
           go to     acc-cod-s03-110.
       acc-cod-s03-120.
           move      w-cmn-zps-003-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s03-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-s03-999.
       acc-cod-s03-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-s03 (1)      .
       acc-cod-s03-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella                             *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-zps-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zps-cod      .
           perform   let-arc-zps-000      thru let-arc-zps-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zps-des    to   w-tes-cod-s03-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs3-000      thru vis-des-cs3-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zps-flg    not  = spaces
                     go to acc-cod-s03-100.
       acc-cod-s03-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-s03-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-s03-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-s03-100.
       acc-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Codice statistico 3       *
      *    *-----------------------------------------------------------*
       vis-cod-s03-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-s03 (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione codice sta-   *
      *    * tistico 3                                                 *
      *    *-----------------------------------------------------------*
       vis-des-cs3-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-s03-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs3-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice fornitore             *
      *    *-----------------------------------------------------------*
       acc-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-fnt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcf-ope      .
           move      w-tes-cod-fnt (1)    to   w-cod-mne-dcf-cod      .
           move      19                   to   w-cod-mne-dcf-lin      .
           move      30                   to   w-cod-mne-dcf-pos      .
           move      19                   to   w-cod-mne-dcf-rln      .
           move      41                   to   w-cod-mne-dcf-rps      .
           move      zero                 to   w-cod-mne-dcf-vln      .
           move      zero                 to   w-cod-mne-dcf-vps      .
           move      zero                 to   w-cod-mne-dcf-lln      .
           move      zero                 to   w-cod-mne-dcf-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
       acc-cod-fnt-110.
           perform   cod-mne-dcf-cll-000  thru cod-mne-dcf-cll-999    .
           if        w-cod-mne-dcf-ope    =    "F+"
                     go to acc-cod-fnt-115.
           if        w-cod-mne-dcf-ope    =    "AC"
                     go to acc-cod-fnt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-fnt-115.
           perform   cod-mne-dcf-foi-000  thru cod-mne-dcf-foi-999    .
           go to     acc-cod-fnt-110.
       acc-cod-fnt-120.
           move      w-cod-mne-dcf-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fnt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-fnt-999.
       acc-cod-fnt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-fnt (1)      .
       acc-cod-fnt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-fnt-410.
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica contabile del     *
      *                  * fornitore preferenziale                     *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt (1)    to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Lettura record anagrafica commerciale del   *
      *                  * fornitore preferenziale                     *
      *                  *---------------------------------------------*
           move      w-tes-cod-fnt (1)    to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
       acc-cod-fnt-420.
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale del forni-   *
      *                  * tore preferenziale                          *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     move  w-let-arc-dcf-rag
                                          to   w-tes-cod-fnt-rag (1)
           else      move  w-let-arc-fnt-rag
                                          to   w-tes-cod-fnt-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale del forni-  *
      *                  * tore preferenziale                          *
      *                  *---------------------------------------------*
           perform   vis-cod-fnt-rag-000  thru vis-cod-fnt-rag-999    .
       acc-cod-fnt-430.
      *                  *---------------------------------------------*
      *                  * Se mancano sia l'anagrafica contabile che   *
      *                  * quella commerciale : reimpostazione         *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-flg    not  = spaces and
                     w-let-arc-dcf-flg    not  = spaces
                     go to acc-cod-fnt-100.
      *                  *---------------------------------------------*
      *                  * Se manca l'anagrafica commerciale : messag- *
      *                  * gio e reimpostazione                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcf-flg    =    spaces
                     go to acc-cod-fnt-440.
           move      "Mancano i dati commerciali del fornitore !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           go to     acc-cod-fnt-100.
       acc-cod-fnt-440.
      *                  *---------------------------------------------*
      *                  * A dipendenze da impostazione                *
      *                  *---------------------------------------------*
           go to     acc-cod-fnt-600.
       acc-cod-fnt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-fnt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-fnt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-fnt-100.
       acc-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice fornitore                  *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-fnt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale fornitore pre-    *
      *    *                         ferenziale                        *
      *    *-----------------------------------------------------------*
       vis-cod-fnt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-fnt-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-fnt-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice casa produttrice      *
      *    *-----------------------------------------------------------*
       acc-cod-pdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-pdt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdt-ope      .
           move      w-tes-cod-pdt (1)    to   w-cod-mne-pdt-cod      .
           move      20                   to   w-cod-mne-pdt-lin      .
           move      30                   to   w-cod-mne-pdt-pos      .
           move      20                   to   w-cod-mne-pdt-rln      .
           move      41                   to   w-cod-mne-pdt-rps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-mne-pdt-cll-000  thru cod-mne-pdt-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-pdt-foi-000  thru cod-mne-pdt-foi-999    .
       acc-cod-pdt-110.
           perform   cod-mne-pdt-cll-000  thru cod-mne-pdt-cll-999    .
           if        w-cod-mne-pdt-ope    =    "F+"
                     go to acc-cod-pdt-115.
           if        w-cod-mne-pdt-ope    =    "AC"
                     go to acc-cod-pdt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-pdt-115.
           perform   cod-mne-pdt-foi-000  thru cod-mne-pdt-foi-999    .
           go to     acc-cod-pdt-110.
       acc-cod-pdt-120.
           move      w-cod-mne-pdt-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cod-pdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cod-pdt-999.
       acc-cod-pdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-pdt (1)      .
       acc-cod-pdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [pdt]                      *
      *                  *---------------------------------------------*
           move      w-tes-cod-pdt (1)    to   w-let-arc-pdt-cod      .
           perform   let-arc-pdt-000      thru let-arc-pdt-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      w-let-arc-pdt-rag    to   w-tes-cod-pdt-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale             *
      *                  *---------------------------------------------*
           perform   vis-cod-pdt-rag-000  thru vis-cod-pdt-rag-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-pdt-flg    not  = spaces
                     go to acc-cod-pdt-100.
       acc-cod-pdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-pdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cod-pdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cod-pdt-100.
       acc-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice casa produttrice           *
      *    *-----------------------------------------------------------*
       vis-cod-pdt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-pdt (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Ragione sociale casa produttrice  *
      *    *-----------------------------------------------------------*
       vis-cod-pdt-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cod-pdt-rag (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pdt-rag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Status commerciale           *
      *    *-----------------------------------------------------------*
       acc-sta-tus-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sta-tus-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      "X"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-sta-tuw (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tus-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-sta-tus-999.
       acc-sta-tus-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-sta-tuw (1)      .
       acc-sta-tus-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-sta-tus-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sta-tus-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-sta-tus-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-sta-tus-100.
       acc-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Status commerciale        *
      *    *-----------------------------------------------------------*
       vis-sta-tus-000.
           move      "DS"                 to   v-ope                  .
           move      "C"                  to   v-tip                  .
           move      w-exp-sta-tus-lun    to   v-car                  .
           move      w-exp-sta-tus-num    to   v-ldt                  .
           move      "B"                  to   v-edm                  .
           move      21                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-sta-tus-tbl    to   v-txt                  .
           move      w-tes-sta-tuw (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice prodotto fornitore min        *
      *    *-----------------------------------------------------------*
       acc-cfp-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-cfp-min-999.
       acc-cfp-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cfp-min (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cfp-min-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cfp-min-999.
       acc-cfp-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cfp-min (1)      .
       acc-cfp-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cfp-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cfp-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cfp-min-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cfp-min-100.
       acc-cfp-min-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice prodotto fornitore min     *
      *    *-----------------------------------------------------------*
       vis-cfp-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cfp-min (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cfp-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice prodotto fornitore max        *
      *    *-----------------------------------------------------------*
       acc-cfp-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to acc-cfp-max-999.
       acc-cfp-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-cfp-max (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cfp-max-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cfp-max-999.
       acc-cfp-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-cfp-max (1)      .
       acc-cfp-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cfp-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cfp-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cfp-max-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cfp-max-100.
       acc-cfp-max-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice prodotto fornitore max     *
      *    *-----------------------------------------------------------*
       vis-cfp-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-tes-cfp-max (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cfp-max-999.
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
           if        w-tes-cod-flt        =    zero
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
      *              *-------------------------------------------------*
      *              * Test su variabile di i.p.c. letta               *
      *              *-------------------------------------------------*
           if        w-ipc-esl-dcp-snx    not  = "N"
                     go to cnt-tdo-nok-800.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione                        *
      *              *-------------------------------------------------*
           if        w-tes-des-flt (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           if        w-tes-cod-flt        not  = 9999999
                     go to cnt-tdo-nok-200.
           move      "Manca la descrizione per il filtro"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su mnemonico                          *
      *              *-------------------------------------------------*
           if        w-tes-cod-mne (1)    not  = spaces
                     go to cnt-tdo-nok-300.
           if        w-tes-cod-flt        =    9999999
                     go to cnt-tdo-nok-300.
           if        w-tes-des-flt (1)    =    spaces
                     go to cnt-tdo-nok-300.
           move      "Manca il mnemonico per il filtro"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo su codice minimo e massimo            *
      *              *-------------------------------------------------*
           if        w-tes-cod-max (1)    =    spaces
                     go to cnt-tdo-nok-400.
           if        w-tes-cod-max (1)    not  < w-tes-cod-min (1)
                     go to cnt-tdo-nok-400.
           move      "Errore tra i valori minimo e massimo del codice"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione minima e massima       *
      *              *-------------------------------------------------*
           if        w-tes-des-max (1)    =    spaces
                     go to cnt-tdo-nok-500.
           if        w-tes-des-max (1)    not  < w-tes-des-min (1)
                     go to cnt-tdo-nok-500.
           move      "Errore tra i valori minimo e massimo della descriz
      -              "ione"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-500.
      *              *-------------------------------------------------*
      *              * Controllo su data inizio commercializzazione    *
      *              * min e max                                       *
      *              *-------------------------------------------------*
           if        w-tes-icm-max (1)    =    zero
                     go to cnt-tdo-nok-600.
           if        w-tes-icm-max (1)    not  < w-tes-icm-min (1)
                     go to cnt-tdo-nok-600.
           move      "La data codifica finale non puo' essere inferiore 
      -              "alla  iniziale !"   to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Controllo su selezioni status prodotto          *
      *              *-------------------------------------------------*
           if        w-tes-sta-tuw (1)    not  = spaces
                     go to cnt-tdo-nok-800.
           move      "Selezionare almeno uno 'status' prodotti"
                                          to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Se controlli tutti superati                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione mnemonico                   *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)    =    spaces
                     move  spaces         to   w-tes-cod-mne (1)      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione filtro dell'utente          *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)    =    spaces
                     move  spaces         to   w-tes-flt-ute (1)      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore e set del flag di *
      *              * uscita ad errore                                *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore in uscita                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-nok-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
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
           move      zero                 to   w-tes-cod-flt          .
           move      spaces               to   w-tes-cod-flt-aut      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati non chiave testata         *
      *              *-------------------------------------------------*
           move      zero                 to   w-tes-ide-dat (1)      .
           move      spaces               to   w-tes-ide-ute (1)      .
           move      spaces               to   w-tes-ide-fas (1)      .
           move      spaces               to   w-tes-cod-mne (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-flt-ute (1)      .
           move      spaces               to   w-tes-des-flt (1)      .
           move      zero                 to   w-tes-tip-ord (1)      .
           move      zero                 to   w-tes-cla-min (1)      .
           move      zero                 to   w-tes-cla-max (1)      .
           move      zero                 to   w-tes-gru-min (1)      .
           move      zero                 to   w-tes-gru-max (1)      .
           move      zero                 to   w-tes-sgr-min (1)      .
           move      zero                 to   w-tes-sgr-max (1)      .
           move      spaces               to   w-tes-cod-min (1)      .
           move      spaces               to   w-tes-cod-max (1)      .
           move      spaces               to   w-tes-des-min (1)      .
           move      spaces               to   w-tes-des-max (1)      .
           move      zero                 to   w-tes-tip-pro (1)      .
           move      zero                 to   w-tes-cod-iva (1)      .
           move      zero                 to   w-tes-ctp-ven (1)      .
           move      spaces               to   w-tes-umi-ven (1)      .
           move      zero                 to   w-tes-cod-s01 (1)      .
           move      zero                 to   w-tes-cod-s02 (1)      .
           move      zero                 to   w-tes-cod-s03 (1)      .
           move      zero                 to   w-tes-cod-fnt (1)      .
           move      zero                 to   w-tes-cod-pdt (1)      .
           move      zero                 to   w-tes-sta-tus (1)      .
           move      all spaces           to   w-tes-sta-tuw (1)      .
           move      spaces               to   w-tes-spc-lib (1)      .
           move      zero                 to   w-tes-icm-min (1)      .
           move      zero                 to   w-tes-icm-max (1)      .
           move      spaces               to   w-tes-cfp-min (1)      .
           move      spaces               to   w-tes-cfp-max (1)      .
           move      spaces               to   w-tes-cod-cpv (1)      .
           move      spaces               to   w-tes-cod-iva-des (1)  .
           move      spaces               to   w-tes-ctp-ven-des (1)  .
           move      spaces               to   w-tes-umi-ven-des (1)  .
           move      spaces               to   w-tes-cod-s01-des (1)  .
           move      spaces               to   w-tes-cod-s02-des (1)  .
           move      spaces               to   w-tes-cod-s03-des (1)  .
           move      spaces               to   w-tes-cod-cpv-des (1)  .
           move      spaces               to   w-tes-cod-fnt-rag (1)  .
           move      spaces               to   w-tes-cod-pdt-rag (1)  .
       nor-nok-tes-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di accettazione          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-ser-els          .
           move      zero                 to   w-acc-ser-edd-c01      .
       nor-nok-tes-300.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to nor-nok-tes-400.
           move      spaces               to   w-acc-ser-sel-alf
                                              (w-acc-ser-edd-c01)     .
           move      spaces               to   w-acc-ser-sel-des
                                              (w-acc-ser-edd-c01)     .
           go to     nor-nok-tes-300.
       nor-nok-tes-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati per area specifica 'dcp'   *
      *              *-------------------------------------------------*
           perform   nor-zos-dcp-000      thru nor-zos-dcp-999        .
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
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFLT    "         to   f-key                  .
           move      w-tes-tip-rec        to   rf-zos-tip-rec         .
           move      w-tes-cod-flt        to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-100.
      *                  *---------------------------------------------*
      *                  * Se anagrafica trovata                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Area chiave                         *
      *                          *-------------------------------------*
           move      rf-zos-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-zos-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-zos-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-zos-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-zos-des-key       to   w-tes-des-key (1)      .
           move      rf-zos-des-flt       to   w-tes-des-flt (1)      .
      *                          *-------------------------------------*
      *                          * Area dati in work di trattamento    *
      *                          * per test preliminare                *
      *                          *-------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-dcp              .
       rou-let-reg-300.
      *                          *-------------------------------------*
      *                          * Test su variabile di i.p.c. letta   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        w-ipc-esl-dcp-snx    =    "N"
                     go to rou-let-reg-400.
      *                              *---------------------------------*
      *                              * Test su tipo ordinamento        *
      *                              *---------------------------------*
           if        w-zos-dcp-tip-ord    =    99
                     go to rou-let-reg-320.
      *                              *---------------------------------*
      *                              * Flag di uscita                  *
      *                              *---------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                              *---------------------------------*
      *                              * Messaggio                       *
      *                              *---------------------------------*
           move      "Filtro non gestibile dal programma !              
      -              "               "    to   w-err-box-err-msg      .
      *                              *---------------------------------*
      *                              * Box di errore                   *
      *                              *---------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * Ad uscita                       *
      *                              *---------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-320.
      *                              *---------------------------------*
      *                              * Filtro selezione casuale        *
      *                              *---------------------------------*
           perform   rou-let-reg-esl-000  thru rou-let-reg-esl-999    .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     rou-let-reg-800.
       rou-let-reg-400.
      *                          *-------------------------------------*
      *                          * Area dati in work di trattamento    *
      *                          *-------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-dcp              .
      *                          *-------------------------------------*
      *                          * Dal tipo ordinamento si desume se   *
      *                          * il filtro riguarda una selezione    *
      *                          * casuale prodotti                    *
      *                          *-------------------------------------*
           if        w-zos-dcp-tip-ord    not  =    99
                     go to rou-let-reg-420.
      *                          *-------------------------------------*
      *                          * Flag di uscita                      *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                          *-------------------------------------*
      *                          * Messaggio                           *
      *                          *-------------------------------------*
           move      "Filtro non gestibile dal programma !              
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * Box di errore                       *
      *                          *-------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-420.
           move      w-zos-dcp-tip-ord    to   w-tes-tip-ord (1)      .
           move      w-zos-dcp-cla-min    to   w-tes-cla-min (1)      .
           move      w-zos-dcp-cla-max    to   w-tes-cla-max (1)      .
           move      w-zos-dcp-gru-min    to   w-tes-gru-min (1)      .
           move      w-zos-dcp-gru-max    to   w-tes-gru-max (1)      .
           move      w-zos-dcp-sgr-min    to   w-tes-sgr-min (1)      .
           move      w-zos-dcp-sgr-max    to   w-tes-sgr-max (1)      .
           move      w-zos-dcp-alf-min    to   w-tes-cod-min (1)      .
           move      w-zos-dcp-alf-max    to   w-tes-cod-max (1)      .
           move      w-zos-dcp-des-min    to   w-tes-des-min (1)      .
           move      w-zos-dcp-des-max    to   w-tes-des-max (1)      .
           move      w-zos-dcp-tip-pro    to   w-tes-tip-pro (1)      .
           if        w-tes-tip-pro (1)    =    00
                     move  99             to   w-tes-tip-pro (1)      .
           move      w-zos-dcp-cod-iva    to   w-tes-cod-iva (1)      .
           move      w-zos-dcp-ctp-ven    to   w-tes-ctp-ven (1)      .
           move      w-zos-dcp-umi-ven    to   w-tes-umi-ven (1)      .
           move      w-zos-dcp-cod-s01    to   w-tes-cod-s01 (1)      .
           move      w-zos-dcp-cod-s02    to   w-tes-cod-s02 (1)      .
           move      w-zos-dcp-cod-s03    to   w-tes-cod-s03 (1)      .
           move      w-zos-dcp-cod-fnt    to   w-tes-cod-fnt (1)      .
           move      w-zos-dcp-cod-pdt    to   w-tes-cod-pdt (1)      .
           move      w-zos-dcp-sta-tuw    to   w-tes-sta-tuw (1)      .
           move      w-zos-dcp-sta-tus    to   w-tes-sta-tus (1)      .
           move      w-zos-dcp-spc-lib    to   w-tes-spc-lib (1)      .
           move      w-zos-dcp-icm-min    to   w-tes-icm-min (1)      .
           move      w-zos-dcp-icm-max    to   w-tes-icm-max (1)      .
           move      w-zos-dcp-cfp-min    to   w-tes-cfp-min (1)      .
           move      w-zos-dcp-cfp-max    to   w-tes-cfp-max (1)      .
           move      w-zos-dcp-cod-cpv    to   w-tes-cod-cpv (1)      .
           move      w-zos-dcp-flt-ute    to   w-tes-flt-ute (1)      .
      *                              *---------------------------------*
      *                              * Normalizzazione cautelativa     *
      *                              * dei campi aggiunti successiva-  *
      *                              * mente                           *
      *                              *---------------------------------*
           if        w-tes-sta-tus (1)    not  numeric
                     move  zero           to   w-tes-sta-tus (1)      .
           if        w-tes-cod-fnt (1)    not  numeric
                     move  zero           to   w-tes-cod-fnt (1)      .
           if        w-tes-icm-min (1)    not  numeric
                     move  zero           to   w-tes-icm-min (1)      .
           if        w-tes-icm-max (1)    not  numeric
                     move  zero           to   w-tes-icm-max (1)      .
           if        w-tes-cod-pdt (1)    not  numeric
                     move  zero           to   w-tes-cod-pdt (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zos]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura tabella [codiva]        *
      *                              *---------------------------------*
           move      w-tes-cod-iva (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-cod-iva-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [pdc]          *
      *                              *---------------------------------*
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zum]          *
      *                              *---------------------------------*
           move      w-tes-umi-ven (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-ven-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zps]          *
      *                              *---------------------------------*
           move      01                   to   w-let-arc-zps-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zps-cod      .
           perform   let-arc-zps-000      thru let-arc-zps-999        .
           move      w-let-arc-zps-des    to   w-tes-cod-s01-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zps]          *
      *                              *---------------------------------*
           move      02                   to   w-let-arc-zps-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zps-cod      .
           perform   let-arc-zps-000      thru let-arc-zps-999        .
           move      w-let-arc-zps-des    to   w-tes-cod-s02-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zps]          *
      *                              *---------------------------------*
           move      03                   to   w-let-arc-zps-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zps-cod      .
           perform   let-arc-zps-000      thru let-arc-zps-999        .
           move      w-let-arc-zps-des    to   w-tes-cod-s03-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zcp]          *
      *                              *---------------------------------*
           move      w-tes-cod-cpv (1)    to   w-let-arc-zcp-cod      .
           perform   let-arc-zcp-000      thru let-arc-zcp-999        .
           move      w-let-arc-zcp-des    to   w-tes-cod-cpv-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [dcf]          *
      *                              *---------------------------------*
           move      w-tes-cod-fnt (1)    to   w-let-arc-dcf-fnt      .
           move      spaces               to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
           move      w-let-arc-dcf-rag    to   w-tes-cod-fnt-rag (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [pdt]          *
      *                              *---------------------------------*
           move      w-tes-cod-pdt (1)    to   w-let-arc-pdt-cod      .
           perform   let-arc-pdt-000      thru let-arc-pdt-999        .
           move      w-let-arc-pdt-rag    to   w-tes-cod-pdt-rag (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
       rou-let-reg-800.
      *              *-------------------------------------------------*
      *              * Test per sola visualizzazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su filtro dell'utente                  *
      *                  *---------------------------------------------*
           if        w-tes-flt-ute (1)    =    spaces
                     go to rou-let-reg-900.
      *                  *---------------------------------------------*
      *                  * Richiesta informazioni generali da segrete- *
      *                  * ria                                         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Confronto tra il codice utente in uso e     *
      *                  * quello in record [zos]                      *
      *                  *---------------------------------------------*
           if        s-ute                =    w-tes-ide-ute (1)
                     go to rou-let-reg-900.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento : Visualizzazione        *
      *                  *---------------------------------------------*
           move      "V"                  to   w-cnt-mfu-tip-fun      .
      *                  *---------------------------------------------*
      *                  * _N.B.: Se si vuole raffinare il test per    *
      *                  *        l'utente 'supervisore' bisogna fare  *
      *                  *        una lettura di 'auc'(vedi pxpg2210)  *
      *                  *---------------------------------------------*
       rou-let-reg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-999.
       rou-let-reg-999.
           exit.

      *    *===========================================================*
      *    * Lettura registrazione pre-esistente                       *
      *    *                                                           *
      *    * Subroutine di lettura selezione casuale                   *
      *    *-----------------------------------------------------------*
       rou-let-reg-esl-000.
      *              *-------------------------------------------------*
      *              * Area dati in work di trattamento                *
      *              *-------------------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-esl-dcp          .
      *              *-------------------------------------------------*
      *              * Numero elementi della selezione                 *
      *              *-------------------------------------------------*
           move      w-zos-esl-dcp-els    to   w-acc-ser-els          .
       rou-let-reg-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di lettura                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       rou-let-reg-esl-200.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to rou-let-reg-esl-900.
           if        w-acc-ser-els        >    w-acc-ser-edd-max
                     go to rou-let-reg-esl-900.
      *                  *---------------------------------------------*
      *                  * Codice numerico prodotto                    *
      *                  *---------------------------------------------*
           move      w-zos-esl-dcp-num
                    (w-acc-ser-edd-c01)   to   w-acc-ser-sel-num
                                              (w-acc-ser-edd-c01)     .
           if        w-zos-esl-dcp-num
                    (w-acc-ser-edd-c01)   not  numeric  
                     move  zero           to   w-acc-ser-sel-num
                                              (w-acc-ser-edd-c01)     .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-acc-ser-sel-num
                    (w-acc-ser-edd-c01)   to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Valori letti                                *
      *                  *---------------------------------------------*
           move      w-let-arc-dcp-des    to   w-acc-ser-sel-des
                                              (w-acc-ser-edd-c01)     .
           move      w-let-arc-dcp-alf    to   w-acc-ser-sel-alf
                                              (w-acc-ser-edd-c01)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     rou-let-reg-esl-200.
       rou-let-reg-esl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-let-reg-esl-999.
       rou-let-reg-esl-999.
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
      *              * Default per status prodotto                     *
      *              *-------------------------------------------------*
           move      "X"                  to   w-tes-sta-nor (1)      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di accettabilita' codice   *
      *              * catalogo                                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-cod-cpv          .
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
      *              * Normalizzazione flag di accettabilita' codice   *
      *              * catalogo                                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-cod-cpv          .
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
       pre-acc-vis-999.
           exit.

      *    *===========================================================*
      *    * Routine post-exit su inserimento                          *
      *    *-----------------------------------------------------------*
       pos-exi-ins-000.
      *              *-------------------------------------------------*
      *              * Se e' stata eseguita l'attribuzione del codice  *
      *              * in automatico, si ripristina, se possibile, il  *
      *              * codice al valore precedente l'incremento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non attribuzione automatica : uscita     *
      *                  *---------------------------------------------*
           if        w-tes-cod-flt-aut    =    spaces
                     go to pos-exi-ins-999.
      *                  *---------------------------------------------*
      *                  * Ripristino codice automatico progressivo    *
      *                  *---------------------------------------------*
           perform   rip-cod-aut-000      thru rip-cod-aut-999        .
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
       pos-cnf-ins-500.
      *              *-------------------------------------------------*
      *              * Richiesta informazioni generali da segreteria   *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se livello non superiore a 1 : nessuna azione   *
      *              *-------------------------------------------------*
           if        s-liv                not  > 01
                     go to pos-cnf-ins-999.
      *              *-------------------------------------------------*
      *              * Se mnemonico a non-spaces : nessuna azione      *
      *              *-------------------------------------------------*
______*    if        w-tes-cod-mne (1)    not  = spaces
______*              go to pos-cnf-ins-999.
       pos-cnf-ins-600.
      *              *-------------------------------------------------*
      *              * Se mnemonico a spaces e livello di profondita'  *
      *              * applicativa superiore a 1                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di mnemonico a spaces in ultimo Inse-  *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-usc-ult-mne-spc      .
      *                  *---------------------------------------------*
      *                  * Put della variabile di i.p.c. 'cod-flt' per *
      *                  * il programma chiamante                      *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-flt"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-tes-cod-flt        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
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
      *              * Trattamento file [zos]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [zos]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zos-000      thru wrt-rec-zos-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zos]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zos-000      thru rew-rec-zos-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [zos]                             *
      *              *-------------------------------------------------*
           perform   del-rec-zos-000      thru del-rec-zos-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zos]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zos-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       cmp-rec-zos-100.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-tip-rec        to   rf-zos-tip-rec         .
           move      w-tes-cod-flt        to   rf-zos-cod-flt         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-zos-ide-dat         .
           move      s-ute                to   rf-zos-ide-ute         .
           move      s-fas                to   rf-zos-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-zos-cod-mne         .
           move      w-tes-des-key (1)    to   rf-zos-des-key         .
           move      w-tes-des-flt (1)    to   rf-zos-des-flt         .
           move      zero                 to   rf-zos-ult-cod         .
           move      spaces               to   w-zos-dcp              .
       cmp-rec-zos-200.
      *                  *---------------------------------------------*
      *                  * Test su variabile di i.p.c. letta           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-ipc-esl-dcp-snx    =    "N"
                     go to cmp-rec-zos-400.
      *                      *-----------------------------------------*
      *                      * Filtro selezione casuale                *
      *                      *-----------------------------------------*
           perform   cmp-rec-zos-esl-000  thru cmp-rec-zos-esl-999    .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cmp-rec-zos-999.
       cmp-rec-zos-400.
      *                  *---------------------------------------------*
      *                  * Area dati in work di trattamento            *
      *                  *---------------------------------------------*
           move      w-tes-tip-ord (1)    to   w-zos-dcp-tip-ord      .
           move      w-tes-cla-min (1)    to   w-zos-dcp-cla-min      .
           move      w-tes-cla-max (1)    to   w-zos-dcp-cla-max      .
           move      w-tes-gru-min (1)    to   w-zos-dcp-gru-min      .
           move      w-tes-gru-max (1)    to   w-zos-dcp-gru-max      .
           move      w-tes-sgr-min (1)    to   w-zos-dcp-sgr-min      .
           move      w-tes-sgr-max (1)    to   w-zos-dcp-sgr-max      .
           move      w-tes-cod-min (1)    to   w-zos-dcp-alf-min      .
           move      w-tes-cod-max (1)    to   w-zos-dcp-alf-max      .
           move      w-tes-des-min (1)    to   w-zos-dcp-des-min      .
           move      w-tes-des-max (1)    to   w-zos-dcp-des-max      .
           move      w-tes-tip-pro (1)    to   w-zos-dcp-tip-pro      .
           if        w-zos-dcp-tip-pro    =    99
                     move  00             to   w-zos-dcp-tip-pro      .
           move      w-tes-cod-iva (1)    to   w-zos-dcp-cod-iva      .
           move      w-tes-ctp-ven (1)    to   w-zos-dcp-ctp-ven      .
           move      w-tes-umi-ven (1)    to   w-zos-dcp-umi-ven      .
           move      w-tes-cod-s01 (1)    to   w-zos-dcp-cod-s01      .
           move      w-tes-cod-s02 (1)    to   w-zos-dcp-cod-s02      .
           move      w-tes-cod-s03 (1)    to   w-zos-dcp-cod-s03      .
           move      w-tes-cod-fnt (1)    to   w-zos-dcp-cod-fnt      .
           move      w-tes-sta-tus (1)    to   w-zos-dcp-sta-tus      .
           move      w-tes-sta-tuw (1)    to   w-zos-dcp-sta-tuw      .
           move      w-tes-flt-ute (1)    to   w-zos-dcp-flt-ute      .
           move      w-tes-spc-lib (1)    to   w-zos-dcp-spc-lib      .
           move      w-tes-icm-min (1)    to   w-zos-dcp-icm-min      .
           move      w-tes-icm-max (1)    to   w-zos-dcp-icm-max      .
           move      w-tes-cod-pdt (1)    to   w-zos-dcp-cod-pdt      .
           move      w-tes-cfp-min (1)    to   w-zos-dcp-cfp-min      .
           move      w-tes-cfp-max (1)    to   w-zos-dcp-cfp-max      .
           move      w-tes-cod-cpv (1)    to   w-zos-dcp-cod-cpv      .
           move      spaces               to   w-zos-dcp-fil-ler      .
           move      w-zos-dcp            to   rf-zos-dat-flt         .
       cmp-rec-zos-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zos]                                 *
      *    *                                                           *
      *    * Subroutine di composizione selezione casuale              *
      *    *-----------------------------------------------------------*
       cmp-rec-zos-esl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione descrizione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se esistente                           *
      *                  *---------------------------------------------*
           if        w-tes-des-flt (1)    not  = spaces
                     move  w-tes-des-flt (1)
                                          to   rf-zos-des-flt
                     go to cmp-rec-zos-esl-080.
      *                  *---------------------------------------------*
      *                  * Preparazione literal                        *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Sequenza :"         to   w-all-str-cat (1)      .
           move      w-acc-ser-sel-alf (1)
                                          to   w-all-str-cat (2)      .
           move      "-"                  to   w-all-str-cat (3)      .
           move      w-acc-ser-sel-alf
                    (w-acc-ser-els)       to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Allineamento a destra                       *
      *                  *---------------------------------------------*
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                  *---------------------------------------------*
      *                  * Valore allineato in campo di uscita         *
      *                  *---------------------------------------------*
           move      w-all-str-alf        to   rf-zos-des-flt         .
       cmp-rec-zos-esl-080.
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      rf-zos-des-flt       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   rf-zos-des-key         .
       cmp-rec-zos-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scrittura                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       cmp-rec-zos-esl-200.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to cmp-rec-zos-esl-800.
      *                  *---------------------------------------------*
      *                  * Codice numerico prodotto                    *
      *                  *---------------------------------------------*
           move      w-acc-ser-sel-num
                    (w-acc-ser-edd-c01)   to   w-zos-esl-dcp-num
                                              (w-acc-ser-edd-c01)     .
           if        w-acc-ser-sel-num
                    (w-acc-ser-edd-c01)   not  numeric  
                     move  zero           to   w-zos-esl-dcp-num
                                              (w-acc-ser-edd-c01)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cmp-rec-zos-esl-200.
       cmp-rec-zos-esl-800.
      *              *-------------------------------------------------*
      *              * In area di scrittura                            *
      *              *-------------------------------------------------*
           move      99                   to   w-zos-esl-dcp-tip      .
           move      w-acc-ser-els        to   w-zos-esl-dcp-els      .
           move      spaces               to   w-zos-esl-dcp-fil      .
           move      w-zos-esl-dcp        to   rf-zos-dat-flt         .
       cmp-rec-zos-esl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-rec-zos-esl-999.
       cmp-rec-zos-esl-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zos]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zos-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zos-000      thru cmp-rec-zos-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       wrt-rec-zos-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zos]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zos-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zos-000      thru cmp-rec-zos-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       rew-rec-zos-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zos]                                *
      *    *-----------------------------------------------------------*
       del-rec-zos-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zos-000      thru cmp-rec-zos-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       del-rec-zos-999.
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
           move      07                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
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
           move      70                   to   v-pos                  .
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
           move      71                   to   v-pos                  .
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
      *    * Determinazione se filtro esistente                        *
      *    *-----------------------------------------------------------*
       det-flt-esi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-flt-esi-flg      .
       det-flt-esi-100.
      *              *-------------------------------------------------*
      *              * Start per mnemonico                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "dcp "               to   rf-zos-tip-rec         .
           move      w-det-flt-esi-mne    to   rf-zos-cod-mne         .
           move      zero                 to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-flt-esi-900.
       det-flt-esi-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale per mnemonico               *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-flt-esi-900.
       det-flt-esi-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        rf-zos-tip-rec       not  = "dcp "
                     go to det-flt-esi-900.
           if        rf-zos-cod-mne       not  = w-det-flt-esi-mne
                     go to det-flt-esi-900.
       det-flt-esi-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Area dati in work di trattamento            *
      *                  *---------------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-dcp              .
      *                  *---------------------------------------------*
      *                  * Test su tipo ordinamento                    *
      *                  *---------------------------------------------*
           if        w-zos-dcp-tip-ord    not  = 89
                     go to det-flt-esi-200.
       det-flt-esi-600.
      *              *-------------------------------------------------*
      *              * Flag di uscita                                  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-det-flt-esi-flg      .
       det-flt-esi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-flt-esi-999.
       det-flt-esi-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp]                            *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice Prodotto di vendita a zero       *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-let-arc-dcp-num    to   rf-dcp-num-pro         .
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
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
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
           move      spaces               to   w-let-arc-dcp-alf      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zum]                             *
      *    *-----------------------------------------------------------*
       let-arc-zum-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice unita' di misura a spaces        *
      *              *-------------------------------------------------*
           if        w-let-arc-zum-cod    =    spaces
                     go to let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODUMI    "         to   f-key                  .
           move      w-let-arc-zum-cod    to   rf-zum-cod-umi         .
           move      "pgm/dcp/fls/ioc/obj/iofzum"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zum                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zum-400.
       let-arc-zum-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zum-des-umi       to   w-let-arc-zum-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zum-999.
       let-arc-zum-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zum-flg      .
           move      all   "."            to   w-let-arc-zum-des      .
           go to     let-arc-zum-999.
       let-arc-zum-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zum-des      .
       let-arc-zum-999.
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
      *    * Routine di lettura archivio [zps]                         *
      *    *-----------------------------------------------------------*
       let-arc-zps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice statistico a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-zps-cod    =    zero
                     go to let-arc-zps-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      w-let-arc-zps-tip    to   rf-zps-tip-cls         .
           move      w-let-arc-zps-cod    to   rf-zps-cod-cls         .
           move      "pgm/dcp/fls/ioc/obj/iofzps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zps-400.
       let-arc-zps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zps-des-cls       to   w-let-arc-zps-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zps-999.
       let-arc-zps-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zps-flg      .
           move      all   "."            to   w-let-arc-zps-des      .
           go to     let-arc-zps-999.
       let-arc-zps-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zps-des      .
       let-arc-zps-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-zcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-zcp-cod    =    spaces
                     go to let-arc-zcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV    "         to   f-key                  .
           move      w-let-arc-zcp-cod    to   rf-zcp-cod-cpv         .
           move      "pgm/dcp/fls/ioc/obj/iofzcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcp-400.
       let-arc-zcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcp-des-cpv       to   w-let-arc-zcp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcp-999.
       let-arc-zcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcp-flg      .
           move      all   "."            to   w-let-arc-zcp-des      .
           go to     let-arc-zcp-600.
       let-arc-zcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcp-des      .
       let-arc-zcp-600.
       let-arc-zcp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [fnt]                            *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
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
           go to     let-arc-fnt-600.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-600.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-piv      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcf]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
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
           go to     let-arc-dcf-999.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [pdt]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice casa produttrice a zero          *
      *              *-------------------------------------------------*
           if        w-let-arc-pdt-cod    =    zero
                     go to let-arc-pdt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDT"             to   f-key                  .
           move      w-let-arc-pdt-cod    to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdt-400.
       let-arc-pdt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdt-rag-soc       to   w-let-arc-pdt-rag      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdt-999.
       let-arc-pdt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdt-flg      .
           move      all   "."            to   w-let-arc-pdt-rag      .
           go to     let-arc-pdt-999.
       let-arc-pdt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdt-rag      .
       let-arc-pdt-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione codice automatico progressivo     *
      *    *-----------------------------------------------------------*
       att-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura record [zos] codice numerico 0000000    *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFLT    "         to   f-key                  .
           move      w-enc-zos-tip-rec    to   rf-zos-tip-rec         .
           move      zero                 to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                =    e-not-err
                     go to att-cod-aut-200.
       att-cod-aut-100.
      *              *-------------------------------------------------*
      *              * Record non esistente                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                      *-----------------------------------------*
      *                      * Composizione record                     *
      *                      *-----------------------------------------*
           move      w-enc-zos-tip-rec    to   rf-zos-tip-rec         .
           move      zero                 to   rf-zos-cod-flt         .
      *                      *-----------------------------------------*
      *                      * Scrittura record                        *
      *                      *-----------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-cod-aut-000.
       att-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Record esistente                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore pre incremento    *
      *                  *---------------------------------------------*
           move      rf-zos-ult-cod       to   w-enc-zos-val-pre      .
      *                  *---------------------------------------------*
      *                  * Incremento del valore                       *
      *                  *---------------------------------------------*
           move      w-enc-zos-val-pre    to   w-enc-zos-val-pos      .
           add       1                    to   w-enc-zos-val-pos      .
       att-cod-aut-300.
      *                  *---------------------------------------------*
      *                  * Se l'incremento porta a zero si forza il    *
      *                  * valore a 1                                  *
      *                  *---------------------------------------------*
           if        w-enc-zos-val-pos    =    zero
                     move  1              to   w-enc-zos-val-pos      .
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo valore impostabile  *
      *                  * si ricicla da 1                             *
      *                  *---------------------------------------------*
           if        w-enc-zos-val-pos    >    w-enc-zos-val-max
                     move  1              to   w-enc-zos-val-pos      .
      *                  *---------------------------------------------*
      *                  * Controllo se esiste gia' un record con il   *
      *                  * codice pari al valore incrementato          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFLT    "         to   f-key                  .
           move      w-enc-zos-tip-rec    to   rf-zos-tip-rec         .
           move      w-enc-zos-val-pos    to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                =    e-not-err
                     go to att-cod-aut-400
           else      go to att-cod-aut-500.
       att-cod-aut-400.
      *                  *---------------------------------------------*
      *                  * Se esiste gia' un record con il codice pari *
      *                  * al valore incrementato                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ulteriore incremento del valore         *
      *                      *-----------------------------------------*
           add       1                    to   w-enc-zos-val-pos      .
      *                      *-----------------------------------------*
      *                      * Riciclo a controllo di esistenza        *
      *                      *-----------------------------------------*
           go to     att-cod-aut-300.
       att-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se non esiste gia' un record con il codice  *
      *                  * pari al valore incrementato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rilettura record [zos] con codice nume- *
      *                      * rico 0000000                            *
      *                      *-----------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODFLT    "         to   f-key                  .
           move      w-enc-zos-tip-rec    to   rf-zos-tip-rec         .
           move      zero                 to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                =    e-not-err
                     go to att-cod-aut-700.
       att-cod-aut-600.
      *                      *-----------------------------------------*
      *                      * Se non esistente                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ripetizione dell'intera operazione  *
      *                          *-------------------------------------*
           go to     att-cod-aut-000.
       att-cod-aut-700.
      *                      *-----------------------------------------*
      *                      * Se esistente                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del valore at- *
      *                          * tuale rispetto al valore pre-incre- *
      *                          * mento                               *
      *                          *-------------------------------------*
           if        rf-zos-ult-cod       =    w-enc-zos-val-pre
                     go to att-cod-aut-900.
       att-cod-aut-800.
      *                          *-------------------------------------*
      *                          * Se valore attuale diverso dal valo- *
      *                          * re pre-incremento                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Unlock                          *
      *                              *---------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                              *---------------------------------*
      *                              * Ripetizione dell'intera opera-  *
      *                              * zione                           *
      *                              *---------------------------------*
           go to     att-cod-aut-000.
       att-cod-aut-900.
      *                          *-------------------------------------*
      *                          * Se valore attuale uguale al valore  *
      *                          * pre-incremento                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento ultimo codice u-  *
      *                              * tilizzato                       *
      *                              *---------------------------------*
           move      w-enc-zos-val-pos    to   rf-zos-ult-cod         .
      *                              *---------------------------------*
      *                              * Riscrittura                     *
      *                              *---------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                              *---------------------------------*
      *                              * Se errori : unlock e ripetizio- *
      *                              * ne dell'intera operazione       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-cod-aut-800.
      *                              *---------------------------------*
      *                              * Unlock                          *
      *                              *---------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     att-cod-aut-999.
       att-cod-aut-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino codice automatico progressivo       *
      *    *-----------------------------------------------------------*
       rip-cod-aut-000.
      *              *-------------------------------------------------*
      *              * Lettura record [zos] codice numerico 0000000    *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODFLT    "         to   f-key                  .
           move      w-enc-zos-tip-rec    to   rf-zos-tip-rec         .
           move      zero                 to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                =    e-not-err
                     go to rip-cod-aut-400.
       rip-cod-aut-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Unlock                                      *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il valore attuale ed il valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
           if        rf-zos-ult-cod       =    w-enc-zos-val-pos
                     go to rip-cod-aut-600.
       rip-cod-aut-500.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale non e' uguale al valo- *
      *                  * re post incremento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-600.
      *                  *---------------------------------------------*
      *                  * Se il valore attuale e' uguale al valore    *
      *                  * post incremento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento del codice automatico con *
      *                      * il valore pre incremento                *
      *                      *-----------------------------------------*
           move      w-enc-zos-val-pre    to   rf-zos-ult-cod         .
      *                          *-------------------------------------*
      *                          * Riscrittura                         *
      *                          *-------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                          *-------------------------------------*
      *                          * Se errori : unlock ed uscita        *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rip-cod-aut-500.
      *                          *-------------------------------------*
      *                          * Unlock                              *
      *                          *-------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     rip-cod-aut-999.
       rip-cod-aut-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [dcp]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/azosdcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp10.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice gruppo          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp20.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottogruppo     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp30.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione unita' di misura           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 1    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzps1.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 2    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzps2.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 3    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzps3.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice catalogo            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice Iva             *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice fornitore com-  *
      *    * merciale                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmndcf0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice casa produttr.  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnpdt0.acs"                   .

      *    *===========================================================*
      *    * Work-area per la normalizzazione dell'area libera per il  *
      *    * filtro 'dcp '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/wzosdcp0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

