       Identification Division.
       Program-Id.                                 pscf0300           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:    tab                 *
      *                                   Fase:    scf030              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 05/11/91    *
      *                       Ultima revisione:    NdK del 20/12/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio tipi operazione per       *
      *                    scadenze fornitori                          *
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
                     "scf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "scf030"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pscf0300"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " TIPI OPERAZIONE PER SCADENZE FORNITORI "       .

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
      *        * [yop]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfyop"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione testata                     *
      *    *-----------------------------------------------------------*
       01  w-tes.
      *        *-------------------------------------------------------*
      *        * Valori chiave                                         *
      *        *-------------------------------------------------------*
           05  w-tes-val-key.
               10  w-tes-cod-top          pic  9(04)                  .
               10  w-tes-cod-top-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Valori attuali e precedenti                           *
      *        *-------------------------------------------------------*
           05  w-tes-val-aep occurs 2.
               10  w-tes-cod-mne          pic  x(04)                  .
               10  w-tes-des-key          pic  x(50)                  .
               10  w-tes-des-top          pic  x(50)                  .
               10  w-tes-cau-cge          pic  9(03)                  .
               10  w-tes-cau-cge-des      pic  x(40)                  .
               10  w-tes-stc-cge          pic  9(07)                  .
               10  w-tes-stc-cge-des      pic  x(40)                  .
               10  w-tes-ctp-cge          pic  9(07)                  .
               10  w-tes-ctp-cge-des      pic  x(40)                  .
               10  w-tes-f01-top          pic  9(02)                  .
               10  w-tes-f02-top          pic  9(02)                  .
               10  w-tes-f03-top          pic  9(02)                  .
               10  w-tes-f04-top          pic  9(02)                  .
               10  w-tes-f05-top          pic  9(02)                  .
               10  w-tes-snx-agg          pic  x(01)                  .
               10  w-tes-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione tipo operazione per scadenze   *
      *    * fornitori                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/acmnyop0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale contabile       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcc.
               10  w-let-arc-zcc-flg      pic  x(01)                  .
               10  w-let-arc-zcc-cod      pic  9(03)                  .
               10  w-let-arc-zcc-des      pic  x(40)                  .
               10  w-let-arc-zcc-tmi      pic  x(01)                  .
               10  w-let-arc-zcc-snb      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per contatori ed indici                         *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore generico                                    *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area di comodo per trattamento prompt                *
      *    *-----------------------------------------------------------*
       01  w-pmt.
      *        *-------------------------------------------------------*
      *        * Per prompt causale contabile                          *
      *        *-------------------------------------------------------*
           05  w-pmt-cau-001              pic  x(28)                  .
           05  w-pmt-cau-002              pic  x(28)                  .
      *        *-------------------------------------------------------*
      *        * Per prompt sottoconto contabile                       *
      *        *-------------------------------------------------------*
           05  w-pmt-stc-001              pic  x(28)                  .
           05  w-pmt-stc-002              pic  x(28)                  .
      *        *-------------------------------------------------------*
      *        * Per prompt contropartita contabile                    *
      *        *-------------------------------------------------------*
           05  w-pmt-ctp-001              pic  x(28)                  .
           05  w-pmt-ctp-002              pic  x(28)                  .
           05  w-pmt-ctp-003              pic  x(28)                  .

      *    *===========================================================*
      *    * Work per tabella precablata in memoria                    *
      *    *-----------------------------------------------------------*
       01  w-tpm.
      *        *-------------------------------------------------------*
      *        * Massimo numero di elementi in tabella                 *
      *        *-------------------------------------------------------*
           05  w-tpm-tip-ope-max          pic  9(03)   value 047      .
      *        *-------------------------------------------------------*
      *        * Tabella dei valori precablati                         *
      *        *-------------------------------------------------------*
           05  w-tpm-tip-ope-val.
      *
               10  filler                 pic  9(04) value       0001 .
               10  filler                 pic  x(50) value
                 "Registrazione Fattura Fornitore                   " .
               10  filler                 pic  x(04) value
                 "F   "                                               .
      *
               10  filler                 pic  9(04) value       0002 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota di Addebito Fornitore          " .
               10  filler                 pic  x(04) value
                 "ND  "                                               .
      *
               10  filler                 pic  9(04) value       0003 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota di Accredito Fornitore         " .
               10  filler                 pic  x(04) value
                 "NC  "                                               .
      *
               10  filler                 pic  9(04) value       0007 .
               10  filler                 pic  x(50) value
                 "Registrazione Fattura Fornitore - Reverse Charge  " .
               10  filler                 pic  x(04) value
                 "FRC "                                               .
      *
               10  filler                 pic  9(04) value       0008 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota Addebito Fornitore - Rev. Ch.  " .
               10  filler                 pic  x(04) value
                 "NDRC"                                               .
      *
               10  filler                 pic  9(04) value       0009 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota Credito Fornitore - Rev. Ch.   " .
               10  filler                 pic  x(04) value
                 "NCRC"                                               .
      *
               10  filler                 pic  9(04) value       0011 .
               10  filler                 pic  x(50) value
                 "Registrazione Fattura CEE - MERCE                 " .
               10  filler                 pic  x(04) value
                 "FCM "                                               .
      *
               10  filler                 pic  9(04) value       0012 .
               10  filler                 pic  x(50) value
                 "Registrazione Fattura CEE - SERVIZI               " .
               10  filler                 pic  x(04) value
                 "FCS "                                               .
      *
               10  filler                 pic  9(04) value       0022 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota di Addebito Fornitore CEE      " .
               10  filler                 pic  x(04) value
                 "NDC "                                               .
      *
               10  filler                 pic  9(04) value       0033 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota di Accredito Fornitore CEE     " .
               10  filler                 pic  x(04) value
                 "NCC "                                               .
      *
               10  filler                 pic  9(04) value       0111 .
               10  filler                 pic  x(50) value
                 "Registrazione Fattura ESTERO - MERCE              " .
               10  filler                 pic  x(04) value
                 "FEM "                                               .
      *
               10  filler                 pic  9(04) value       0112 .
               10  filler                 pic  x(50) value
                 "Registrazione Fattura ESTERO - SERVIZI            " .
               10  filler                 pic  x(04) value
                 "FES "                                               .
      *
               10  filler                 pic  9(04) value       0222 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota di Addebito Fornitore Estero   " .
               10  filler                 pic  x(04) value
                 "NDE "                                               .
      *
               10  filler                 pic  9(04) value       0333 .
               10  filler                 pic  x(50) value
                 "Registrazione Nota di Accredito Fornitore Estero  " .
               10  filler                 pic  x(04) value
                 "NCE "                                               .
      *
               10  filler                 pic  9(04) value       0444 .
               10  filler                 pic  x(50) value
                 "Registrazione Bolla Doganale                      " .
               10  filler                 pic  x(04) value
                 "BD  "                                               .
      *
               10  filler                 pic  9(04) value       1101 .
               10  filler                 pic  x(50) value
                 "Registrazione Rimessa Diretta                     " .
               10  filler                 pic  x(04) value
                 "RD  "                                               .
      *
               10  filler                 pic  9(04) value       1102 .
               10  filler                 pic  x(50) value
                 "Registrazione Incasso Elettronico                 " .
               10  filler                 pic  x(04) value
                 "IE  "                                               .
      *
               10  filler                 pic  9(04) value       1103 .
               10  filler                 pic  x(50) value
                 "Registrazione Ri.Ba.                              " .
               10  filler                 pic  x(04) value
                 "RIBA"                                               .
      *
               10  filler                 pic  9(04) value       1104 .
               10  filler                 pic  x(50) value
                 "Registrazione C.d.O.                              " .
               10  filler                 pic  x(04) value
                 "CDO "                                               .
      *
               10  filler                 pic  9(04) value       1105 .
               10  filler                 pic  x(50) value
                 "Registrazione M.Av.                               " .
               10  filler                 pic  x(04) value
                 "MAV "                                               .
      *
               10  filler                 pic  9(04) value       1106 .
               10  filler                 pic  x(50) value
                 "Registrazione R.I.D.                              " .
               10  filler                 pic  x(04) value
                 "RID "                                               .
      *
               10  filler                 pic  9(04) value       1107 .
               10  filler                 pic  x(50) value
                 "Registrazione Bonifico Bancario                   " .
               10  filler                 pic  x(04) value
                 "BB  "                                               .
      *
               10  filler                 pic  9(04) value       1108 .
               10  filler                 pic  x(50) value
                 "Registrazione C/C Postale                         " .
               10  filler                 pic  x(04) value
                 "CCP "                                               .
      *
               10  filler                 pic  9(04) value       1109 .
               10  filler                 pic  x(50) value
                 "Registrazione Ricevuta Bancaria                   " .
               10  filler                 pic  x(04) value
                 "RB  "                                               .
      *
               10  filler                 pic  9(04) value       1110 .
               10  filler                 pic  x(50) value
                 "Registrazione Tratta                              " .
               10  filler                 pic  x(04) value
                 "TR  "                                               .
      *
               10  filler                 pic  9(04) value       1111 .
               10  filler                 pic  x(50) value
                 "Registrazione Paghero' Cambiario                  " .
               10  filler                 pic  x(04) value
                 "PC  "                                               .
      *
               10  filler                 pic  9(04) value       1127 .
               10  filler                 pic  x(50) value
                 "Registrazione Bonifico Bancario su Estero         " .
               10  filler                 pic  x(04) value
                 "BBE "                                               .
      *
               10  filler                 pic  9(04) value       1161 .
               10  filler                 pic  x(50) value
                 "Cessione Paghero' Cambiario a Fornitore           " .
               10  filler                 pic  x(04) value
                 "CPF "                                               .
      *
               10  filler                 pic  9(04) value       1200 .
               10  filler                 pic  x(50) value
                 "Storno Scadenza                                   " .
               10  filler                 pic  x(04) value
                 "STO "                                               .
      *
               10  filler                 pic  9(04) value       1510 .
               10  filler                 pic  x(50) value
                 "Riscossione per Contanti                          " .
               10  filler                 pic  x(04) value
                 "RPC "                                               .
      *
               10  filler                 pic  9(04) value       1520 .
               10  filler                 pic  x(50) value
                 "Riscossione con Assegno                           " .
               10  filler                 pic  x(04) value
                 "RCA "                                               .
      *
               10  filler                 pic  9(04) value       1530 .
               10  filler                 pic  x(50) value
                 "Riscossione contro accredito in C/C Bancario      " .
               10  filler                 pic  x(04) value
                 "RCC "                                               .
      *
               10  filler                 pic  9(04) value       1610 .
               10  filler                 pic  x(50) value
                 "Pagamento per Contanti                            " .
               10  filler                 pic  x(04) value
                 "PCO "                                               .
      *
               10  filler                 pic  9(04) value       1620 .
               10  filler                 pic  x(50) value
                 "Pagamento con Assegno                             " .
               10  filler                 pic  x(04) value
                 "PAS "                                               .
      *
               10  filler                 pic  9(04) value       1630 .
               10  filler                 pic  x(50) value
                 "Pagamento contro Addebito in C/C Bancario         " .
               10  filler                 pic  x(04) value
                 "PAB "                                               .
      *
               10  filler                 pic  9(04) value       1640 .
               10  filler                 pic  x(50) value
                 "Pagamento contro Addebito in C/C Postale          " .
               10  filler                 pic  x(04) value
                 "PAP "                                               .
      *
               10  filler                 pic  9(04) value       1650 .
               10  filler                 pic  x(50) value
                 "Compensazione Scadenze                            " .
               10  filler                 pic  x(04) value
                 "CMP "                                               .
      *
               10  filler                 pic  9(04) value       1730 .
               10  filler                 pic  x(50) value
                 "Ordine di Bonifico Bancario                       " .
               10  filler                 pic  x(04) value
                 "OBB "                                               .
      *
               10  filler                 pic  9(04) value       1731 .
               10  filler                 pic  x(50) value
                 "Ordine di Bonifico Bancario su Estero             " .
               10  filler                 pic  x(04) value
                 "OBE "                                               .
      *
               10  filler                 pic  9(04) value       1740 .
               10  filler                 pic  x(50) value
                 "Bollettino di C/C Postale                         " .
               10  filler                 pic  x(04) value
                 "BCP "                                               .
      *
               10  filler                 pic  9(04) value       1750 .
               10  filler                 pic  x(50) value
                 "Incarico di pagamento Avviso di Scadenza          " .
               10  filler                 pic  x(04) value
                 "IAS "                                               .
      *
               10  filler                 pic  9(04) value       1830 .
               10  filler                 pic  x(50) value
                 "Addebito Bonifici Bancari                         " .
               10  filler                 pic  x(04) value
                 "ABB "                                               .
      *
               10  filler                 pic  9(04) value       1831 .
               10  filler                 pic  x(50) value
                 "Addebito Bonifici Bancari su Estero               " .
               10  filler                 pic  x(04) value
                 "ABE "                                               .
      *
               10  filler                 pic  9(04) value       1840 .
               10  filler                 pic  x(50) value
                 "Addebito Bollettini di C/C Postale                " .
               10  filler                 pic  x(04) value
                 "ACP "                                               .
      *
               10  filler                 pic  9(04) value       1850 .
               10  filler                 pic  x(50) value
                 "Addebito Avvisi di Scadenza                       " .
               10  filler                 pic  x(04) value
                 "AAS "                                               .
      *
               10  filler                 pic  9(04) value       4730 .
               10  filler                 pic  x(50) value
                 "Stampa Ordini di Bonifico Bancario                " .
               10  filler                 pic  x(04) value
                 "SOB "                                               .
      *
               10  filler                 pic  9(04) value       4731 .
               10  filler                 pic  x(50) value
                 "Stampa Ordini di Bonifico Bancario su Estero      " .
               10  filler                 pic  x(04) value
                 "SOE "                                               .
      *
               10  filler                 pic  9(04) value       4735 .
               10  filler                 pic  x(50) value
                 "Stampa conferma a Fornitore di avvenuto Bonifico  " .
               10  filler                 pic  x(04) value
                 "SAB "                                               .
      *
               10  filler                 pic  9(04) value       4736 .
               10  filler                 pic  x(50) value
                 "Stampa conferma a Fornitore per Bonifico Estero   " .
               10  filler                 pic  x(04) value
                 "SABE"                                               .
      *
               10  filler                 pic  9(04) value       4750 .
               10  filler                 pic  x(50) value
                 "Stampa Incarichi di pagamento Avvisi di Scadenza  " .
               10  filler                 pic  x(04) value
                 "SIP "                                               .
      *        *-------------------------------------------------------*
      *        * Ridefinizione tabella                                 *
      *        *-------------------------------------------------------*
           05  w-tpm-tip-ope-r01 redefines
               w-tpm-tip-ope-val.
      *            *---------------------------------------------------*
      *            * Elementi tabella                                  *
      *            *---------------------------------------------------*
               10  w-tpm-tip-ope-ele occurs 047
                                  indexed by   w-tpm-tip-ope-inx      .
      *                *-----------------------------------------------*
      *                * Codice tipo operazione                        *
      *                *-----------------------------------------------*
                   15  w-tpm-tip-ope-cod  pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Descrizione tipo operazione                   *
      *                *-----------------------------------------------*
                   15  w-tpm-tip-ope-des  pic  x(50)                  .
      *                *-----------------------------------------------*
      *                * Codice mnemonico tipo operazione              *
      *                *-----------------------------------------------*
                   15  w-tpm-tip-ope-mne  pic  x(04)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .

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
                     go to exe-acc-cmp-600.
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
      *              * Test se nessun record presente nel file [yop]   *
      *              *-------------------------------------------------*
           perform   ctl-esi-yop-000      thru ctl-esi-yop-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione numero livelli del    *
      *              * piano dei conti                                 *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo operazione per    *
      *              * scadenze fornitori                              *
      *              *-------------------------------------------------*
           perform   cod-mne-yop-opn-000  thru cod-mne-yop-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice causale         *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-opn-000  thru cod-mne-zcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottoconto      *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-opn-000  thru cod-mne-pdc-opn-999    .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo operazione per   *
      *              * scadenze fornitori                              *
      *              *-------------------------------------------------*
           perform   cod-mne-yop-cls-000  thru cod-mne-yop-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice causale        *
      *              *-------------------------------------------------*
           perform   cod-mne-zcc-cls-000  thru cod-mne-zcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottoconto     *
      *              *-------------------------------------------------*
           perform   cod-mne-pdc-cls-000  thru cod-mne-pdc-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine di verifica se record presenti in file [yop]      *
      *    *-----------------------------------------------------------*
       ctl-esi-yop-000.
      *              *-------------------------------------------------*
      *              * Open file [yop]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *              *-------------------------------------------------*
      *              * Start su file [yop]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTOP    "         to   f-key                  .
           move      zero                 to   rf-yop-cod-top         .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *                  *---------------------------------------------*
      *                  * Se Start a buon fine : uscita               *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to ctl-esi-yop-900.
      *                  *---------------------------------------------*
      *                  * Se nessun record presente nel file [yop] :  *
      *                  * caricamento tabella precablata in memoria   *
      *                  * nel file [yop]                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione contatore generico         *
      *                      *-----------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       ctl-esi-yop-100.
      *                      *-----------------------------------------*
      *                      * Ciclo di scrittura file [yop]           *
      *                      *-----------------------------------------*
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    w-tpm-tip-ope-max
                     go to ctl-esi-yop-900.
      *                          *-------------------------------------*
      *                          * Composizione area w-tes da valori   *
      *                          * tabella precablata                  *
      *                          *-------------------------------------*
           move      w-tpm-tip-ope-cod
                    (w-cix-ctr-001)       to   w-tes-cod-top          .
           move      w-tpm-tip-ope-des
                    (w-cix-ctr-001)       to   w-tes-des-top (1)      .
      *
           move      w-tpm-tip-ope-des
                    (w-cix-ctr-001)       to   w-all-str-alf          .
           move      50                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
      *
           move      w-tpm-tip-ope-mne
                    (w-cix-ctr-001)       to   w-tes-cod-mne (1)      .
           move      zero                 to   w-tes-cau-cge (1)      .
           move      zero                 to   w-tes-stc-cge (1)      .
           move      zero                 to   w-tes-ctp-cge (1)      .
           move      zero                 to   w-tes-f01-top (1)      .
           move      zero                 to   w-tes-f02-top (1)      .
           move      zero                 to   w-tes-f03-top (1)      .
           move      zero                 to   w-tes-f04-top (1)      .
           move      zero                 to   w-tes-f05-top (1)      .
           move      spaces               to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Write record [yop]                  *
      *                          *-------------------------------------*
           perform   wrt-rec-yop-000      thru wrt-rec-yop-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     ctl-esi-yop-100.
       ctl-esi-yop-900.
      *              *-------------------------------------------------*
      *              * Chiusura file [yop]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
       ctl-esi-yop-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [yop]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *              *-------------------------------------------------*
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [yop]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *              *-------------------------------------------------*
      *              * [zcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
      *                  * Codice tipo operazione                      *
      *                  *---------------------------------------------*
           perform   acc-cod-top-000      thru acc-cod-top-999        .
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
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           perform   vis-cod-top-000      thru vis-cod-top-999        .
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
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           perform   pmt-cod-top-000      thru pmt-cod-top-999        .
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
      *    * Visualizzazione prompts per Codice tipo operazione        *
      *    *-----------------------------------------------------------*
       pmt-cod-top-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo operazione            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-cod-top-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo chiave : Codice tipo operazione        *
      *    *-----------------------------------------------------------*
       acc-cod-top-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-top-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-yop-ope      .
           move      w-tes-cod-top        to   w-cod-mne-yop-cod      .
           move      04                   to   w-cod-mne-yop-lin      .
           move      30                   to   w-cod-mne-yop-pos      .
           move      06                   to   w-cod-mne-yop-dln      .
           move      30                   to   w-cod-mne-yop-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-yop-cll-000  thru cod-mne-yop-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-yop-foi-000  thru cod-mne-yop-foi-999    .
       acc-cod-top-110.
           perform   cod-mne-yop-cll-000  thru cod-mne-yop-cll-999    .
           if        w-cod-mne-yop-ope    =    "F+"
                     go to acc-cod-top-115.
           if        w-cod-mne-yop-ope    =    "AC"
                     go to acc-cod-top-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-top-115.
           perform   cod-mne-yop-foi-000  thru cod-mne-yop-foi-999    .
           go to     acc-cod-top-110.
       acc-cod-top-120.
           move      w-cod-mne-yop-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-top-999.
       acc-cod-top-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cod-top          .
       acc-cod-top-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : uscita                            *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to acc-cod-top-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione indice                      *
      *                  *---------------------------------------------*
           set       w-tpm-tip-ope-inx    to   1                      .
      *                  *---------------------------------------------*
      *                  * Test che il valore esista in tabella        *
      *                  *---------------------------------------------*
           search    w-tpm-tip-ope-ele
                     when    w-tpm-tip-ope-cod
                            (w-tpm-tip-ope-inx)
                                          =    w-tes-cod-top
                             go to   acc-cod-top-520.
      *                  *---------------------------------------------*
      *                  * A elemento non trovato                      *
      *                  *---------------------------------------------*
           go to     acc-cod-top-500.
       acc-cod-top-500.
      *                  *---------------------------------------------*
      *                  * Se valore non trovato in tabella : visua-   *
      *                  * lizzazione di puntini in descrizione e re-  *
      *                  * impostazione                                *
      *                  *---------------------------------------------*
           move      all "."              to   w-tes-cod-top-des      .
           perform   vis-cod-dtp-000      thru vis-cod-dtp-999        .
           go to     acc-cod-top-100.
       acc-cod-top-520.
           perform   vis-cod-dtp-000      thru vis-cod-dtp-999        .
       acc-cod-top-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-top-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cod-top-999.
           perform   cnt-key-vuo-000      thru cnt-key-vuo-999        .
           if        w-cnt-key-vuo-flg    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-cod-top-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-cod-top-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-cod-top-999.
       acc-cod-top-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Codice tipo operazione     *
      *    *-----------------------------------------------------------*
       vis-cod-top-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-top        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-top-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo chiave : Descrizione tipo operazio- *
      *    *                                ne                         *
      *    *-----------------------------------------------------------*
       vis-cod-dtp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-top-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-dtp-999.
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
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Descrizione tipo operazione                 *
      *                  *---------------------------------------------*
           perform   acc-des-top-000      thru acc-des-top-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Codice mnemonico                            *
      *                  *---------------------------------------------*
           perform   acc-cod-mne-000      thru acc-cod-mne-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Causale contabile                           *
      *                  *---------------------------------------------*
           perform   acc-cau-cge-000      thru acc-cau-cge-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Sottoconto contabile                        *
      *                  *---------------------------------------------*
           perform   acc-stc-cge-000      thru acc-stc-cge-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Contropartita contabile                     *
      *                  *---------------------------------------------*
           perform   acc-ctp-cge-000      thru acc-ctp-cge-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
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
      *              * Descrizione tipo operazione                     *
      *              *-------------------------------------------------*
           perform   vis-des-top-000      thru vis-des-top-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Causale contabile                               *
      *              *-------------------------------------------------*
           perform   vis-cau-cge-000      thru vis-cau-cge-999        .
           perform   vis-des-cau-000      thru vis-des-cau-999        .
      *              *-------------------------------------------------*
      *              * Sottoconto contabile                            *
      *              *-------------------------------------------------*
           perform   vis-stc-cge-000      thru vis-stc-cge-999        .
           perform   vis-des-stc-000      thru vis-des-stc-999        .
      *              *-------------------------------------------------*
      *              * Contropartita contabile                         *
      *              *-------------------------------------------------*
           perform   vis-ctp-cge-000      thru vis-ctp-cge-999        .
           perform   vis-des-ctp-000      thru vis-des-ctp-999        .
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Descrizione tipo operazione                     *
      *              *-------------------------------------------------*
           perform   pmt-des-top-000      thru pmt-des-top-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Causale contabile                               *
      *              *-------------------------------------------------*
           perform   pmt-cau-cge-000      thru pmt-cau-cge-999        .
      *              *-------------------------------------------------*
      *              * Sottoconto contabile                            *
      *              *-------------------------------------------------*
           perform   pmt-stc-cge-000      thru pmt-stc-cge-999        .
      *              *-------------------------------------------------*
      *              * Contropartita contabile                         *
      *              *-------------------------------------------------*
           perform   pmt-ctp-cge-000      thru pmt-ctp-cge-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione tipo operazione      *
      *    *-----------------------------------------------------------*
       pmt-des-top-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione operazione     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-des-top-999.
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
           move      "Mnemonico per l'operazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Causale contabile                *
      *    *-----------------------------------------------------------*
       pmt-cau-cge-000.
      *              *-------------------------------------------------*
      *              * Test su codice tipo operazione                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    0001 or
                     w-tes-cod-top        =    0002 or
                     w-tes-cod-top        =    0003 or
                     w-tes-cod-top        =    0007 or
                     w-tes-cod-top        =    0008 or
                     w-tes-cod-top        =    0009 or
                     w-tes-cod-top        =    0011 or
                     w-tes-cod-top        =    0012 or
                     w-tes-cod-top        =    0022 or
                     w-tes-cod-top        =    0033 or
                     w-tes-cod-top        =    0111 or
                     w-tes-cod-top        =    0112 or
                     w-tes-cod-top        =    0222 or
                     w-tes-cod-top        =    0333 or
                     w-tes-cod-top        =    0444
                     go to pmt-cau-cge-050
           else if   w-tes-cod-top        =    1101 or
                     w-tes-cod-top        =    1102 or
                     w-tes-cod-top        =    1103 or
                     w-tes-cod-top        =    1104 or
                     w-tes-cod-top        =    1105 or
                     w-tes-cod-top        =    1106 or
                     w-tes-cod-top        =    1107 or
                     w-tes-cod-top        =    1108 or
                     w-tes-cod-top        =    1109 or
                     w-tes-cod-top        =    1110 or
                     w-tes-cod-top        =    1111 or
                     w-tes-cod-top        =    1127
                     go to pmt-cau-cge-100
           else if   w-tes-cod-top        =    1161
                     go to pmt-cau-cge-120
           else if   w-tes-cod-top        =    1200
                     go to pmt-cau-cge-140
           else if   w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
      *
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640
                     go to pmt-cau-cge-200
           else if   w-tes-cod-top        =    1650
                     go to pmt-cau-cge-350
           else if   w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to pmt-cau-cge-400
           else      go to pmt-cau-cge-999.
       pmt-cau-cge-050.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali 0001..0444      *
      *              *-------------------------------------------------*
           move      "Causale contabile per la   :"
                                          to   w-pmt-cau-001          .
           move      " registrazione documento    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-100.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali 1101..1111      *
      *              *-------------------------------------------------*
           move      "Causale contabile per la   :"
                                          to   w-pmt-cau-001          .
           move      "  registrazione scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-120.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 1161            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "   per cessione scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-140.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 1200            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "     per storno scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-200.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 16xx      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "  per pagamento scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-350.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 1650      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "       per compensazione    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-400.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 18xx      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "          per l'addebito    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-900.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt relativo                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pmt-cau-001        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pmt-cau-002        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cau-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Sottoconto contabile             *
      *    *-----------------------------------------------------------*
       pmt-stc-cge-000.
      *              *-------------------------------------------------*
      *              * Test su codice tipo operazione                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    1101 or
                     w-tes-cod-top        =    1102 or
                     w-tes-cod-top        =    1103 or
                     w-tes-cod-top        =    1104 or
                     w-tes-cod-top        =    1105 or
                     w-tes-cod-top        =    1106 or
                     w-tes-cod-top        =    1107 or
                     w-tes-cod-top        =    1108 or
                     w-tes-cod-top        =    1109 or
                     w-tes-cod-top        =    1110 or
                     w-tes-cod-top        =    1111 or
                     w-tes-cod-top        =    1127
                     go to pmt-stc-cge-100
           else if   w-tes-cod-top        =    1161
                     go to pmt-stc-cge-120
           else if   w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
      *
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640 or
                     w-tes-cod-top        =    1650
                     go to pmt-stc-cge-200
           else if   w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to pmt-stc-cge-400
           else      go to pmt-stc-cge-999.
       pmt-stc-cge-100.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto serie 11nn   *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "  registrazione scadenza    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-120.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 1161 *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "       cessione scadenza    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-200.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto serie 16nn   *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "         abbuoni passivi    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-400.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto serie 18nn   *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "         abbuoni passivi    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-900.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt relativo                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pmt-stc-001        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pmt-stc-002        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-stc-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Contropartita contabile          *
      *    *-----------------------------------------------------------*
       pmt-ctp-cge-000.
      *              *-------------------------------------------------*
      *              * Test su codice tipo operazione                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
      *
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640 or
                     w-tes-cod-top        =    1650 or
      *
                     w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to pmt-ctp-cge-300
           else      go to pmt-ctp-cge-999.
       pmt-ctp-cge-300.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per contropartite causali   *
      *              * serie 03nn                                      *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-ctp-001          .
           move      "          abbuoni attivi    "
                                          to   w-pmt-ctp-002          .
           move      spaces               to   w-pmt-ctp-003          .
           go to     pmt-ctp-cge-900.
       pmt-ctp-cge-900.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt relativo                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pmt-ctp-001        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pmt-ctp-002        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-pmt-ctp-003        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ctp-cge-999.
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
      *    * Accettazione campo testata : Descrizione tipo operazione  *
      *    *-----------------------------------------------------------*
       acc-des-top-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-des-top-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-tes-des-top (1)    to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-des-top-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-des-top-999.
       acc-des-top-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-des-top (1)      .
       acc-des-top-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-des-top (1)    not  = spaces
                     go to acc-des-top-450.
           if        v-key                =    "UP  "
                     go to acc-des-top-600
           else      go to acc-des-top-100.
       acc-des-top-450.
      *                  *---------------------------------------------*
      *                  * Se valore a non spaces il primo carattere   *
      *                  * non deve essere a spaces                    *
      *                  *---------------------------------------------*
           if        w-tes-des-top (1)
                    (01 : 01)             =    spaces
                     go to acc-des-top-100.
       acc-des-top-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-top (1)    to   w-all-str-alf          .
           move      50                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
       acc-des-top-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-des-top-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-des-top-100.
       acc-des-top-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione operazione    *
      *    *-----------------------------------------------------------*
       vis-des-top-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-des-top (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-top-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Codice mnemonico             *
      *    *-----------------------------------------------------------*
       acc-cod-mne-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
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
           move      04                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-mne-100.
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
           move      04                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-cod-mne (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-mne-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice causale contabile             *
      *    *-----------------------------------------------------------*
       acc-cau-cge-000.
      *              *-------------------------------------------------*
      *              * Pre accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    0001 or
                     w-tes-cod-top        =    0002 or
                     w-tes-cod-top        =    0003 or
                     w-tes-cod-top        =    0007 or
                     w-tes-cod-top        =    0008 or
                     w-tes-cod-top        =    0009 or
                     w-tes-cod-top        =    0011 or
                     w-tes-cod-top        =    0012 or
                     w-tes-cod-top        =    0022 or
                     w-tes-cod-top        =    0033 or
                     w-tes-cod-top        =    0111 or
                     w-tes-cod-top        =    0112 or
                     w-tes-cod-top        =    0222 or
                     w-tes-cod-top        =    0333 or
                     w-tes-cod-top        =    0444 or
                     w-tes-cod-top        =    1101 or
                     w-tes-cod-top        =    1102 or
                     w-tes-cod-top        =    1103 or
                     w-tes-cod-top        =    1104 or
                     w-tes-cod-top        =    1105 or
                     w-tes-cod-top        =    1106 or
                     w-tes-cod-top        =    1107 or
                     w-tes-cod-top        =    1108 or
                     w-tes-cod-top        =    1109 or
                     w-tes-cod-top        =    1110 or
                     w-tes-cod-top        =    1111 or
                     w-tes-cod-top        =    1127 or
                     w-tes-cod-top        =    1161 or
                     w-tes-cod-top        =    1200 or
                     w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640 or
                     w-tes-cod-top        =    1650 or
                     w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to acc-cau-cge-100
           else      go to acc-cau-cge-999.
       acc-cau-cge-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zcc-ope      .
           move      w-tes-cau-cge (1)    to   w-cod-mne-zcc-cod      .
           move      10                   to   w-cod-mne-zcc-lin      .
           move      30                   to   w-cod-mne-zcc-pos      .
           move      10                   to   w-cod-mne-zcc-dln      .
           move      41                   to   w-cod-mne-zcc-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
       acc-cau-cge-110.
           perform   cod-mne-zcc-cll-000  thru cod-mne-zcc-cll-999    .
           if        w-cod-mne-zcc-ope    =    "F+"
                     go to acc-cau-cge-115.
           if        w-cod-mne-zcc-ope    =    "AC"
                     go to acc-cau-cge-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cau-cge-115.
           perform   cod-mne-zcc-foi-000  thru cod-mne-zcc-foi-999    .
           go to     acc-cau-cge-110.
       acc-cau-cge-120.
           move      w-cod-mne-zcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-cau-cge-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-cau-cge-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-cau-cge (1)      .
       acc-cau-cge-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella causali                     *
      *                  *---------------------------------------------*
           move      w-tes-cau-cge (1)    to   w-let-arc-zcc-cod      .
           perform   let-arc-zcc-000      thru let-arc-zcc-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione descrizione causale         *
      *                  *---------------------------------------------*
           move      w-let-arc-zcc-des    to   w-tes-cau-cge-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione causale         *
      *                  *---------------------------------------------*
           perform   vis-des-cau-000      thru vis-des-cau-999        .
      *                  *---------------------------------------------*
      *                  * Se causale non esistente : messaggio di er- *
      *                  * rore e reimpostazione                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zcc-flg    not  = spaces
                     go to acc-cau-cge-100.
      *                  *---------------------------------------------*
      *                  * Se causale a zero : continuazione           *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to acc-cau-cge-600.
       acc-cau-cge-450.
      *                  *---------------------------------------------*
      *                  * Se causale di bilancio : messaggio di erro- *
      *                  * re e reimpostazione                         *
      *                  *---------------------------------------------*
           if        w-let-arc-zcc-snb    =    "N"
                     go to acc-cau-cge-500.
           move      "ME"                 to   v-ope                  .
           move      "La causale contabile e' di rettifica bilancio !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-cau-cge-100.
       acc-cau-cge-500.
      *                  *---------------------------------------------*
      *                  * Se causale iva                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che il tipo opera- *
      *                      * zione sia una registrazione fattura     *
      *                      * fornitore o no                          *
      *                      *-----------------------------------------*
           if        w-tes-cod-top        =    0001 or
                     w-tes-cod-top        =    0002 or
                     w-tes-cod-top        =    0003 or
                     w-tes-cod-top        =    0007 or
                     w-tes-cod-top        =    0008 or
                     w-tes-cod-top        =    0009 or
                     w-tes-cod-top        =    0011 or
                     w-tes-cod-top        =    0012 or
                     w-tes-cod-top        =    0022 or
                     w-tes-cod-top        =    0033 or
                     w-tes-cod-top        =    0111 or
                     w-tes-cod-top        =    0112 or
                     w-tes-cod-top        =    0222 or
                     w-tes-cod-top        =    0333 or
                     w-tes-cod-top        =    0444
                     go to acc-cau-cge-510
           else      go to acc-cau-cge-550.
       acc-cau-cge-510.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione relativo ad una re-  *
      *                      * registrazione fattura fornitore         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo o-  *
      *                          * perazione                           *
      *                          *-------------------------------------*
           if        w-tes-cod-top        =    0001 or
                     w-tes-cod-top        =    0002
                     go to acc-cau-cge-512
           else if   w-tes-cod-top        =    0003
                     go to acc-cau-cge-514
           else if   w-tes-cod-top        =    0007 or
                     w-tes-cod-top        =    0008 or
                     w-tes-cod-top        =    0011 or
                     w-tes-cod-top        =    0012 or
                     w-tes-cod-top        =    0022
                     go to acc-cau-cge-516
           else if   w-tes-cod-top        =    0009 or
                     w-tes-cod-top        =    0033
                     go to acc-cau-cge-518
           else if   w-tes-cod-top        =    0111 or
                     w-tes-cod-top        =    0112 or
                     w-tes-cod-top        =    0222 or
                     w-tes-cod-top        =    0333
                     go to acc-cau-cge-520
           else if   w-tes-cod-top        =    0444
                     go to acc-cau-cge-522.
       acc-cau-cge-512.
      *                          *-------------------------------------*
      *                          * Tipo operazione : registrazione     *
      *                          * fattura o nota addebito Italia      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Il tipo movimento iva deve es-  *
      *                              * sere : '5', cioe' Avere forni-  *
      *                              * tore Italia                     *
      *                              *---------------------------------*
           if        w-let-arc-zcc-tmi    not  = "5"
                     go to acc-cau-cge-540.
           go to     acc-cau-cge-600.
       acc-cau-cge-514.
      *                          *-------------------------------------*
      *                          * Tipo operazione : registrazione     *
      *                          * nota accredito Italia               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Il tipo movimento iva deve es-  *
      *                              * sere : '4', cioe' Dare forni-   *
      *                              * tore Italia                     *
      *                              *---------------------------------*
           if        w-let-arc-zcc-tmi    not  = "4"
                     go to acc-cau-cge-540.
           go to     acc-cau-cge-600.
       acc-cau-cge-516.
      *                          *-------------------------------------*
      *                          * Tipo operazione : registrazione     *
      *                          * fattura o nota addebito CEE o       *
      *                          * Reverse Charge                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Il tipo movimento iva deve es-  *
      *                              * sere : 'E', cioe' Avere forni-  *
      *                              * tore CEE o RC                   *
      *                              *---------------------------------*
           if        w-let-arc-zcc-tmi    not  = "E"
                     go to acc-cau-cge-540.
           go to     acc-cau-cge-600.
       acc-cau-cge-518.
      *                          *-------------------------------------*
      *                          * Tipo operazione : registrazione     *
      *                          * nota accredito CEE                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Il tipo movimento iva deve es-  *
      *                              * sere : 'D', cioe' Dare forni-   *
      *                              * tore CEE o RC                   *
      *                              *---------------------------------*
           if        w-let-arc-zcc-tmi    not  = "D"
                     go to acc-cau-cge-540.
           go to     acc-cau-cge-600.
       acc-cau-cge-520.
      *                          *-------------------------------------*
      *                          * Tipo operazione : registrazione     *
      *                          * documento Estero                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Il tipo movimento iva deve es-  *
      *                              * sere : '0' o a spazi            *
      *                              *---------------------------------*
           if        w-let-arc-zcc-tmi    not  = "0"    and
                     w-let-arc-zcc-tmi    not  = spaces
                     go to acc-cau-cge-590.
           go to     acc-cau-cge-600.
       acc-cau-cge-522.
      *                          *-------------------------------------*
      *                          * Tipo operazione : registrazione     *
      *                          * bolla doganale                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Il tipo movimento iva deve es-  *
      *                              * sere : '6', cioe' Bolla dogana- *
      *                              * le                              *
      *                              *---------------------------------*
           if        w-let-arc-zcc-tmi    not  = "6"
                     go to acc-cau-cge-540.
           go to     acc-cau-cge-600.
       acc-cau-cge-540.
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Causale contabile errata sotto il profilo Iva !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cau-cge-100.
       acc-cau-cge-550.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione non relativo ad una  *
      *                      * registrazione fattura fornitore         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Il tipo movimento iva deve esse-    *
      *                          * re : '0' o a spazi                  *
      *                          *-------------------------------------*
           if        w-let-arc-zcc-tmi    =    "0" or
                     w-let-arc-zcc-tmi    =    spaces
                     go to acc-cau-cge-600.
       acc-cau-cge-590.
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "La causale contabile non puo' essere di tipo Iva !
      -              ""
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-cau-cge-100.
       acc-cau-cge-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cau-cge-625.
      *                  *---------------------------------------------*
      *                  * Se causale a zero si normalizza il sotto-   *
      *                  * conto contabile e la contropartita conta-   *
      *                  * bile se e' il caso                          *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to acc-cau-cge-630
           else      go to acc-cau-cge-800.
       acc-cau-cge-630.
      *                  *---------------------------------------------*
      *                  * Normalizzazione sottoconto                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se tipo operazione che prevede il  *
      *                      * sottoconto contabile                    *
      *                      *-----------------------------------------*
           if        w-tes-cod-top        not  < 1101 and
                     w-tes-cod-top        not  > 1111
                     go to acc-cau-cge-635
           else if   w-tes-cod-top        =    1161
                     go to acc-cau-cge-635
           else if   w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
      *
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640 or
                     w-tes-cod-top        =    1650 or
      *
                     w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to acc-cau-cge-635
           else      go to acc-cau-cge-645.
       acc-cau-cge-635.
      *                      *-----------------------------------------*
      *                      * Test se sottoconto contabile gia' nor-  *
      *                      * malizzato                               *
      *                      *-----------------------------------------*
           if        w-tes-stc-cge (1)    =    zero
                     go to acc-cau-cge-645.
       acc-cau-cge-640.
      *                      *-----------------------------------------*
      *                      * Normalizzazione sottoconto              *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-stc-cge (1)      .
           move      spaces               to   w-tes-stc-cge-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-stc-cge-000      thru vis-stc-cge-999        .
           perform   vis-des-stc-000      thru vis-des-stc-999        .
       acc-cau-cge-645.
      *                  *---------------------------------------------*
      *                  * Normalizzazione contropartita               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se tipo operazione che prevede la  *
      *                      * contropartita contabile intesa come     *
      *                      * secondo sottoconto                      *
      *                      *-----------------------------------------*
           if        w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
      *
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640 or
                     w-tes-cod-top        =    1650 or
      *
                     w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to acc-cau-cge-650
           else      go to acc-cau-cge-660.
       acc-cau-cge-650.
      *                      *-----------------------------------------*
      *                      * Test se contropartita contabile gia'    *
      *                      * normalizzata                            *
      *                      *-----------------------------------------*
           if        w-tes-ctp-cge (1)    =    zero
                     go to acc-cau-cge-660.
       acc-cau-cge-655.
      *                      *-----------------------------------------*
      *                      * Normalizzazione contropartita           *
      *                      *-----------------------------------------*
           move      zero                 to   w-tes-ctp-cge (1)      .
           move      spaces               to   w-tes-ctp-cge-des (1)  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-ctp-cge-000      thru vis-ctp-cge-999        .
           perform   vis-des-ctp-000      thru vis-des-ctp-999        .
       acc-cau-cge-660.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     acc-cau-cge-800.
       acc-cau-cge-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-cau-cge-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-cau-cge-100.
       acc-cau-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice causale contabile          *
      *    *-----------------------------------------------------------*
       vis-cau-cge-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      w-tes-cau-cge (1)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-cau-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione causale contabile     *
      *    *-----------------------------------------------------------*
       vis-des-cau-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-cau-cge-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-des-cau-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Sottoconto contabile         *
      *    *-----------------------------------------------------------*
       acc-stc-cge-000.
      *              *-------------------------------------------------*
      *              * Pre accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to acc-stc-cge-999.
           if        w-tes-cod-top        =    1101 or
                     w-tes-cod-top        =    1102 or
                     w-tes-cod-top        =    1103 or
                     w-tes-cod-top        =    1104 or
                     w-tes-cod-top        =    1105 or
                     w-tes-cod-top        =    1106 or
                     w-tes-cod-top        =    1107 or
                     w-tes-cod-top        =    1108 or
                     w-tes-cod-top        =    1109 or
                     w-tes-cod-top        =    1110 or
                     w-tes-cod-top        =    1111 or
                     w-tes-cod-top        =    1127 or
                     w-tes-cod-top        =    1161 or
                     w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640 or
                     w-tes-cod-top        =    1650 or
                     w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to acc-stc-cge-100
           else      go to acc-stc-cge-999.
       acc-stc-cge-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-stc-cge (1)    to   w-cod-mne-pdc-cod      .
           move      13                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      13                   to   w-cod-mne-pdc-dln      .
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
       acc-stc-cge-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-stc-cge-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-stc-cge-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-stc-cge-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-stc-cge-110.
       acc-stc-cge-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-stc-cge-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-stc-cge-999.
       acc-stc-cge-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-stc-cge (1)      .
       acc-stc-cge-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-stc-cge-425.
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-stc-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-stc-cge-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-des-stc-000      thru vis-des-stc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-stc-cge-100.
       acc-stc-cge-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-stc-cge-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-stc-cge-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-stc-cge-100.
       acc-stc-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Sottoconto contabile      *
      *    *-----------------------------------------------------------*
       vis-stc-cge-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-stc-cge (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-stc-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione sottoconto    *
      *    *-----------------------------------------------------------*
       vis-des-stc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-stc-cge-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-stc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Contropartita contabile      *
      *    *-----------------------------------------------------------*
       acc-ctp-cge-000.
      *              *-------------------------------------------------*
      *              * Pre accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to acc-ctp-cge-999.
           if        w-tes-cod-top        =    1510 or
                     w-tes-cod-top        =    1520 or
                     w-tes-cod-top        =    1530 or
      *
                     w-tes-cod-top        =    1610 or
                     w-tes-cod-top        =    1620 or
                     w-tes-cod-top        =    1630 or
                     w-tes-cod-top        =    1640 or
                     w-tes-cod-top        =    1650 or
      *
                     w-tes-cod-top        =    1830 or
                     w-tes-cod-top        =    1831 or
                     w-tes-cod-top        =    1840 or
                     w-tes-cod-top        =    1850
                     go to acc-ctp-cge-100
           else      go to acc-ctp-cge-999.
       acc-ctp-cge-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
           move      w-prs-liv-pdc        to   w-cod-mne-pdc-liv      .
           move      w-tes-ctp-cge (1)    to   w-cod-mne-pdc-cod      .
           move      16                   to   w-cod-mne-pdc-lin      .
           move      30                   to   w-cod-mne-pdc-pos      .
           move      16                   to   w-cod-mne-pdc-dln      .
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
       acc-ctp-cge-110.
           perform   cod-mne-pdc-cll-000  thru cod-mne-pdc-cll-999    .
           if        w-cod-mne-pdc-ope    =    "F+"
                     go to acc-ctp-cge-115.
           if        w-cod-mne-pdc-ope    =    "AC"
                     go to acc-ctp-cge-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-ctp-cge-115.
           perform   cod-mne-pdc-foi-000  thru cod-mne-pdc-foi-999    .
           go to     acc-ctp-cge-110.
       acc-ctp-cge-120.
           move      w-cod-mne-pdc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-cge-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ctp-cge-999.
       acc-ctp-cge-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-tes-ctp-cge (1)      .
       acc-ctp-cge-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ctp-cge-425.
      *                  *---------------------------------------------*
      *                  * Lettura archivio piano dei conti            *
      *                  *---------------------------------------------*
           move      w-tes-ctp-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-cge-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione sottoconto      *
      *                  *---------------------------------------------*
           perform   vis-des-ctp-000      thru vis-des-ctp-999        .
      *                  *---------------------------------------------*
      *                  * Se codice sottoconto non esistente : reim-  *
      *                  * postazione                                  *
      *                  *---------------------------------------------*
           if        w-let-arc-pdc-flg    not  = spaces
                     go to acc-ctp-cge-100.
       acc-ctp-cge-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ctp-cge-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ctp-cge-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ctp-cge-100.
       acc-ctp-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Contropartita contabile   *
      *    *-----------------------------------------------------------*
       vis-ctp-cge-000.
      *              *-------------------------------------------------*
      *              * Editing con appoggio a sinistra                 *
      *              *-------------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ctp-cge (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ctp-cge-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Descrizione contropartita *
      *    *-----------------------------------------------------------*
       vis-des-ctp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-tes-ctp-cge-des (1)
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
      *              * Test su tipo operazione                         *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
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
       cnt-tdo-nok-050.
      *              *-------------------------------------------------*
      *              * Test su descrizione tipo operazione             *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to cnt-tdo-nok-100.
           if        w-tes-des-top (1)    not  = spaces
                     go to cnt-tdo-nok-100.
           move      "Manca la descrizione del tipo operazione          
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Fine controlli                                  *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-800.
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Se controlli tutti superati                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore                   *
      *              *-------------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con flag di errore                   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-nok-flg      .
       cnt-tdo-nok-999.
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
           move      zero                 to   w-tes-cod-top          .
           move      spaces               to   w-tes-cod-top-des      .
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           move      spaces               to   w-tes-cod-mne (1)      .
           move      spaces               to   w-tes-des-key (1)      .
           move      spaces               to   w-tes-des-top (1)      .
           move      zero                 to   w-tes-cau-cge (1)      .
           move      spaces               to   w-tes-cau-cge-des (1)  .
           move      zero                 to   w-tes-stc-cge (1)      .
           move      spaces               to   w-tes-stc-cge-des (1)  .
           move      zero                 to   w-tes-ctp-cge (1)      .
           move      spaces               to   w-tes-ctp-cge-des (1)  .
           move      zero                 to   w-tes-f01-top (1)      .
           move      zero                 to   w-tes-f02-top (1)      .
           move      zero                 to   w-tes-f03-top (1)      .
           move      zero                 to   w-tes-f04-top (1)      .
           move      zero                 to   w-tes-f05-top (1)      .
           move      spaces               to   w-tes-snx-agg (1)      .
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
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOP    "         to   f-key                  .
           move      w-tes-cod-top        to   rf-yop-cod-top         .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se anagrafica non trovata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-055.
      *                          *-------------------------------------*
      *                          * Flag di uscita                      *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-rou-let-reg      .
      *                          *-------------------------------------*
      *                          * Messaggio                           *
      *                          *-------------------------------------*
           move      "Inserimento non consentito !                      
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * Box di errore                       *
      *                          *-------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     rou-let-reg-900.
       rou-let-reg-055.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
       rou-let-reg-060.
      *                      *-----------------------------------------*
      *                      * Preparazione valori di default          *
      *                      *-----------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       rou-let-reg-065.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    w-tpm-tip-ope-max
                     go to rou-let-reg-070.
           if        w-tpm-tip-ope-cod
                    (w-cix-ctr-001)       not  = w-tes-cod-top
                     go to rou-let-reg-065.
           move      w-tpm-tip-ope-cod
                    (w-cix-ctr-001)       to   w-tes-cod-top          .
           move      w-tpm-tip-ope-des
                    (w-cix-ctr-001)       to   w-tes-des-top (1)      .
      *
           move      w-tpm-tip-ope-des
                    (w-cix-ctr-001)       to   w-all-str-alf          .
           move      50                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-tes-des-key (1)      .
      *
           move      w-tpm-tip-ope-mne
                    (w-cix-ctr-001)       to   w-tes-cod-mne (1)      .
       rou-let-reg-070.
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
      *                          * Valori contenuti direttamente in    *
      *                          * record [yop]                        *
      *                          *-------------------------------------*
           move      rf-yop-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-yop-des-key       to   w-tes-des-key (1)      .
           move      rf-yop-des-top       to   w-tes-des-top (1)      .
           move      rf-yop-cau-cge       to   w-tes-cau-cge (1)      .
           move      rf-yop-stc-cge       to   w-tes-stc-cge (1)      .
           move      rf-yop-ctp-cge       to   w-tes-ctp-cge (1)      .
           move      rf-yop-f01-top       to   w-tes-f01-top (1)      .
           move      rf-yop-f02-top       to   w-tes-f02-top (1)      .
           move      rf-yop-f03-top       to   w-tes-f03-top (1)      .
           move      rf-yop-f04-top       to   w-tes-f04-top (1)      .
           move      rf-yop-f05-top       to   w-tes-f05-top (1)      .
           move      rf-yop-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [yop]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Descrizione causale             *
      *                              *---------------------------------*
           move      w-tes-cau-cge (1)    to   w-let-arc-zcc-cod      .
           perform   let-arc-zcc-000      thru let-arc-zcc-999        .
           move      w-let-arc-zcc-des    to   w-tes-cau-cge-des (1)  .
      *                              *---------------------------------*
      *                              * Descrizione sottoconto          *
      *                              *---------------------------------*
           move      w-tes-stc-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-stc-cge-des (1)  .
      *                              *---------------------------------*
      *                              * Descrizione contropartita       *
      *                              *---------------------------------*
           move      w-tes-ctp-cge (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-cge-des (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
       rou-let-reg-800.
      *              *-------------------------------------------------*
      *              * Test per visualizzazione                        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-vis-sgr    =    "V"
                     move  "V"            to   w-cnt-mfu-tip-fun      .
       rou-let-reg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
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
      *              * Trattamento file [yop]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [yop]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-yop-000      thru wrt-rec-yop-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [yop]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-yop-000      thru rew-rec-yop-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [yop]                             *
      *              *-------------------------------------------------*
           perform   del-rec-yop-000      thru del-rec-yop-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [yop]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-yop-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-top        to   rf-yop-cod-top         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-yop-ide-dat         .
           move      s-ute                to   rf-yop-ide-ute         .
           move      s-fas                to   rf-yop-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-yop-cod-mne         .
           move      w-tes-des-key (1)    to   rf-yop-des-key         .
           move      w-tes-des-top (1)    to   rf-yop-des-top         .
           move      w-tes-cau-cge (1)    to   rf-yop-cau-cge         .
           move      w-tes-stc-cge (1)    to   rf-yop-stc-cge         .
           move      w-tes-ctp-cge (1)    to   rf-yop-ctp-cge         .
           move      w-tes-f01-top (1)    to   rf-yop-f01-top         .
           move      w-tes-f02-top (1)    to   rf-yop-f02-top         .
           move      w-tes-f03-top (1)    to   rf-yop-f03-top         .
           move      w-tes-f04-top (1)    to   rf-yop-f04-top         .
           move      w-tes-f05-top (1)    to   rf-yop-f05-top         .
           move      w-tes-alx-exp (1)    to   rf-yop-alx-exp         .
       cmp-rec-yop-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [yop]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-yop-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yop-000      thru cmp-rec-yop-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
       wrt-rec-yop-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [yop]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-yop-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yop-000      thru cmp-rec-yop-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
       rew-rec-yop-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [yop]                                *
      *    *-----------------------------------------------------------*
       del-rec-yop-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-yop-000      thru cmp-rec-yop-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofyop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yop                 .
       del-rec-yop-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zcc]                             *
      *    *-----------------------------------------------------------*
       let-arc-zcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice causale a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-zcc-cod    =    zero
                     go to let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      w-let-arc-zcc-cod    to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcc-400.
       let-arc-zcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcc-des-cau       to   w-let-arc-zcc-des      .
           move      rf-zcc-tip-moi       to   w-let-arc-zcc-tmi      .
           move      rf-zcc-snx-bil       to   w-let-arc-zcc-snb      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcc-999.
       let-arc-zcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcc-flg      .
           move      all   "."            to   w-let-arc-zcc-des      .
           go to     let-arc-zcc-600.
       let-arc-zcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcc-des      .
       let-arc-zcc-600.
           move      spaces               to   w-let-arc-zcc-tmi      .
           move      spaces               to   w-let-arc-zcc-snb      .
       let-arc-zcc-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice operazione per sca- *
      *    * denze fornitori                                           *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/acmnyop0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della causale contabile    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .
