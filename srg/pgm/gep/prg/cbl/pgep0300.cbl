       Identification Division.
       Program-Id.                                 pgep0300           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    tab                 *
      *                                   Fase:    gep030              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 05/11/91    *
      *                       Ultima revisione:    NdK del 27/09/12    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione archivio Tipi operazione per       *
      *                    gestione portafoglio                        *
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
                     "gep"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "tab"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "gep030"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pgep0300"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "TIPI OPERAZIONE PER GESTIONE PORTAFOGLIO"       .

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
      *        * [zop]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzop"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [gep]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfgep"                          .

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
      *    * Work-area per ridefinizione record file [gep]             *
      *    *-----------------------------------------------------------*
       01  w-fil-gep.
      *        *-------------------------------------------------------*
      *        * Per ridefinizione tipo record 01 per personalizzazio- *
      *        * ni inerenti l'aggiornamento della contabilita' gene-  *
      *        * rale dalla gestione portafoglio                       *
      *        *-------------------------------------------------------*
           05  w-fil-gep-001.
      *            *---------------------------------------------------*
      *            * Si/No aggiornamento, parametro globale            *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-fil-gep-001-agg      pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo operazione         *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzop0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice causale contabile       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/No aggiornamento contabile              *
      *        *-------------------------------------------------------*
           05  w-exp-snx-agg.
               10  w-exp-snx-agg-num      pic  9(02)       value 02   .
               10  w-exp-snx-agg-lun      pic  9(02)       value 02   .
               10  w-exp-snx-agg-tbl.
                   15  filler             pic  x(02) value
                            "Si"                                      .
                   15  filler             pic  x(02) value
                            "No"                                      .
      *        *-------------------------------------------------------*
      *        * Work per : Flag 3                                     *
      *        *-------------------------------------------------------*
           05  w-exp-f03-top.
               10  w-exp-f03-top-num      pic  9(02)       value 02   .
               10  w-exp-f03-top-lun      pic  9(02)       value 02   .
               10  w-exp-f03-top-tbl.
                   15  filler             pic  x(02) value
                            "No"                                      .
                   15  filler             pic  x(02) value
                            "Si"                                      .

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
           05  w-tpm-tip-ope-max          pic  9(03)   value 044      .
      *        *-------------------------------------------------------*
      *        * Tabella dei valori precablati                         *
      *        *-------------------------------------------------------*
           05  w-tpm-tip-ope-val.
      *
               10  filler                 pic  9(04) value       0101 .
               10  filler                 pic  x(50) value
                 "Emissione Rimessa Diretta                         " .
               10  filler                 pic  x(04) value
                 "RD  "                                               .
      *
               10  filler                 pic  9(04) value       0102 .
               10  filler                 pic  x(50) value
                 "Emissione Incasso Elettronico                     " .
               10  filler                 pic  x(04) value
                 "IE  "                                               .
      *
               10  filler                 pic  9(04) value       0103 .
               10  filler                 pic  x(50) value
                 "Emissione Ri.Ba.                                  " .
               10  filler                 pic  x(04) value
                 "RIBA"                                               .
      *
               10  filler                 pic  9(04) value       0104 .
               10  filler                 pic  x(50) value
                 "Emissione C.d.O.                                  " .
               10  filler                 pic  x(04) value
                 "CDO "                                               .
      *
               10  filler                 pic  9(04) value       0105 .
               10  filler                 pic  x(50) value
                 "Emissione M.Av.                                   " .
               10  filler                 pic  x(04) value
                 "MAV "                                               .
      *
               10  filler                 pic  9(04) value       0106 .
               10  filler                 pic  x(50) value
                 "Emissione R.I.D.                                  " .
               10  filler                 pic  x(04) value
                 "RID "                                               .
      *
               10  filler                 pic  9(04) value       0107 .
               10  filler                 pic  x(50) value
                 "Emissione Bonifico Bancario                       " .
               10  filler                 pic  x(04) value
                 "BB  "                                               .
      *
               10  filler                 pic  9(04) value       0108 .
               10  filler                 pic  x(50) value
                 "Emissione C/C Postale                             " .
               10  filler                 pic  x(04) value
                 "CCP "                                               .
      *
               10  filler                 pic  9(04) value       0109 .
               10  filler                 pic  x(50) value
                 "Emissione Ricevuta Bancaria                       " .
               10  filler                 pic  x(04) value
                 "RB  "                                               .
      *
               10  filler                 pic  9(04) value       0110 .
               10  filler                 pic  x(50) value
                 "Emissione Tratta                                  " .
               10  filler                 pic  x(04) value
                 "TR  "                                               .
      *
               10  filler                 pic  9(04) value       0111 .
               10  filler                 pic  x(50) value
                 "Emissione Paghero' Cambiario                      " .
               10  filler                 pic  x(04) value
                 "PC  "                                               .
      *
               10  filler                 pic  9(04) value       0161 .
               10  filler                 pic  x(50) value
                 "Cessione Paghero' Cambiario da Cliente            " .
               10  filler                 pic  x(04) value
                 "CPC "                                               .
      *
               10  filler                 pic  9(04) value       0200 .
               10  filler                 pic  x(50) value
                 "Storno Scadenza                                   " .
               10  filler                 pic  x(04) value
                 "SSC "                                               .
      *
               10  filler                 pic  9(04) value       0301 .
               10  filler                 pic  x(50) value
                 "Riscossione Scadenze per Contanti                 " .
               10  filler                 pic  x(04) value
                 "RCO "                                               .
      *
               10  filler                 pic  9(04) value       0302 .
               10  filler                 pic  x(50) value
                 "Riscossione Scadenze con Assegno                  " .
               10  filler                 pic  x(04) value
                 "RAS "                                               .
      *
               10  filler                 pic  9(04) value       0303 .
               10  filler                 pic  x(50) value
                 "Riscossione Scadenze per Bonifico Bancario        " .
               10  filler                 pic  x(04) value
                 "RBB "                                               .
      *
               10  filler                 pic  9(04) value       0304 .
               10  filler                 pic  x(50) value
                 "Riscossione Scadenze per Bonifico in C/C Postale  " .
               10  filler                 pic  x(04) value
                 "RBP "                                               .
      *
               10  filler                 pic  9(04) value       0321 .
               10  filler                 pic  x(50) value
                 "Pagamento Scadenze per Contanti                   " .
               10  filler                 pic  x(04) value
                 "PCO "                                               .
      *
               10  filler                 pic  9(04) value       0322 .
               10  filler                 pic  x(50) value
                 "Pagamento Scadenze con Assegno                    " .
               10  filler                 pic  x(04) value
                 "PAS "                                               .
      *
               10  filler                 pic  9(04) value       0323 .
               10  filler                 pic  x(50) value
                 "Pagamento Scadenze per Bonifico Bancario          " .
               10  filler                 pic  x(04) value
                 "PBB "                                               .
      *
               10  filler                 pic  9(04) value       0324 .
               10  filler                 pic  x(50) value
                 "Pagamento Scadenze per Bonifico in C/C Postale    " .
               10  filler                 pic  x(04) value
                 "PBP "                                               .
      *
               10  filler                 pic  9(04) value       0345 .
               10  filler                 pic  x(50) value
                 "Lista Clienti con Compensazioni da Effettuare     " .
               10  filler                 pic  x(04) value
                 "LCC "                                               .
      *
               10  filler                 pic  9(04) value       0350 .
               10  filler                 pic  x(50) value
                 "Compensazione Scadenze                            " .
               10  filler                 pic  x(04) value
                 "CMP "                                               .
      *
               10  filler                 pic  9(04) value       0401 .
               10  filler                 pic  x(50) value
                 "Composizione Distinta Incassi Elettronici         " .
               10  filler                 pic  x(04) value
                 "DIE "                                               .
      *
               10  filler                 pic  9(04) value       0402 .
               10  filler                 pic  x(50) value
                 "Composizione Distinta Effetti                     " .
               10  filler                 pic  x(04) value
                 "DEF "                                               .
      *
               10  filler                 pic  9(04) value       0403 .
               10  filler                 pic  x(50) value
                 "Composizione Distinta Paghero' Cambiari           " .
               10  filler                 pic  x(04) value
                 "DPC "                                               .
      *
               10  filler                 pic  9(04) value       0404 .
               10  filler                 pic  x(50) value
                 "Composizione Distinta Cessioni                    " .
               10  filler                 pic  x(04) value
                 "DCE "                                               .
      *
               10  filler                 pic  9(04) value       0415 .
               10  filler                 pic  x(50) value
                 "Stampa Distinta per Controllo                     " .
               10  filler                 pic  x(04) value
                 "SDC "                                               .
      *
               10  filler                 pic  9(04) value       0501 .
               10  filler                 pic  x(50) value
                 "Presentazione Distinta SBF a Maturazione di Valuta" .
               10  filler                 pic  x(04) value
                 "PMV "                                               .
      *
               10  filler                 pic  9(04) value       0502 .
               10  filler                 pic  x(50) value
                 "Presentazione Distinta SBF con Accredito Immediato" .
               10  filler                 pic  x(04) value
                 "PAI "                                               .
      *
               10  filler                 pic  9(04) value       0503 .
               10  filler                 pic  x(50) value
                 "Presentazione Distinta al Dopo Incasso            " .
               10  filler                 pic  x(04) value
                 "PDI "                                               .
      *
               10  filler                 pic  9(04) value       0504 .
               10  filler                 pic  x(50) value
                 "Presentazione Distinta allo Sconto                " .
               10  filler                 pic  x(04) value
                 "PSC "                                               .
      *
               10  filler                 pic  9(04) value       0510 .
               10  filler                 pic  x(50) value
                 "Preparazione Archivio di Supporto per Distinte    " .
               10  filler                 pic  x(04) value
                 "ADS "                                               .
      *
               10  filler                 pic  9(04) value       0515 .
               10  filler                 pic  x(50) value
                 "Stampa Distinta per la Presentazione              " .
               10  filler                 pic  x(04) value
                 "SDP "                                               .
      *
               10  filler                 pic  9(04) value       0516 .
               10  filler                 pic  x(50) value
                 "Stampa Effetti a fronte Distinta                  " .
               10  filler                 pic  x(04) value
                 "SEF "                                               .
      *
               10  filler                 pic  9(04) value       0520 .
               10  filler                 pic  x(50) value
                 "Riscontro Tipo Avviso a Debitori per Incassi Elet." .
               10  filler                 pic  x(04) value
                 "RTA "                                               .
      *
               10  filler                 pic  9(04) value       0540 .
               10  filler                 pic  x(50) value
                 "Accettazione Distinta da parte della Banca        " .
               10  filler                 pic  x(04) value
                 "ACT "                                               .
      *
               10  filler                 pic  9(04) value       0560 .
               10  filler                 pic  x(50) value
                 "Accredito Distinta in C/Corrente Bancario         " .
               10  filler                 pic  x(04) value
                 "ACD "                                               .
      *
               10  filler                 pic  9(04) value       0600 .
               10  filler                 pic  x(50) value
                 "Insoluto su Scadenza presentata in Banca          " .
               10  filler                 pic  x(04) value
                 "INS "                                               .
      *
               10  filler                 pic  9(04) value       0620 .
               10  filler                 pic  x(50) value
                 "Richiamo di Scadenza presentata in Banca          " .
               10  filler                 pic  x(04) value
                 "RIC "                                               .
      *
               10  filler                 pic  9(04) value       0700 .
               10  filler                 pic  x(50) value
                 "Accredito di Scadenza presentata al Dopo Incasso  " .
               10  filler                 pic  x(04) value
                 "ACS "                                               .
      *
               10  filler                 pic  9(04) value       0720 .
               10  filler                 pic  x(50) value
                 "Notizia di buon esito Scadenze SBF o allo Sconto  " .
               10  filler                 pic  x(04) value
                 "NBE "                                               .
      *
               10  filler                 pic  9(04) value       0740 .
               10  filler                 pic  x(50) value
                 "Presunto buon esito di Scadenze SBF o allo Sconto " .
               10  filler                 pic  x(04) value
                 "PBE "                                               .
      *
               10  filler                 pic  9(04) value       0741 .
               10  filler                 pic  x(50) value
                 "Presunto buon esito per Gruppi di Scadenze        " .
               10  filler                 pic  x(04) value
                 "PBG "                                               .
      *        *-------------------------------------------------------*
      *        * Ridefinizione tabella                                 *
      *        *-------------------------------------------------------*
           05  w-tpm-tip-ope-r01 redefines
               w-tpm-tip-ope-val.
      *            *---------------------------------------------------*
      *            * Elementi tabella                                  *
      *            *---------------------------------------------------*
               10  w-tpm-tip-ope-ele occurs 044.
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
      *              * Test se nessun record presente nel file [zop]   *
      *              *-------------------------------------------------*
           perform   ctl-esi-zop-000      thru ctl-esi-zop-999        .
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
      *              * gestione portafoglio                            *
      *              *-------------------------------------------------*
           perform   cod-mne-zop-opn-000  thru cod-mne-zop-opn-999    .
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
      *              * gestione portafoglio                            *
      *              *-------------------------------------------------*
           perform   cod-mne-zop-cls-000  thru cod-mne-zop-cls-999    .
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
      *    * Routine di verifica se record presenti in file [zop]      *
      *    *-----------------------------------------------------------*
       ctl-esi-zop-000.
      *              *-------------------------------------------------*
      *              * Open file [zop]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *              *-------------------------------------------------*
      *              * Start su file [zop]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTOP    "         to   f-key                  .
           move      zero                 to   rf-zop-cod-top         .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *                  *---------------------------------------------*
      *                  * Se Start a buon fine : uscita               *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to ctl-esi-zop-900.
      *                  *---------------------------------------------*
      *                  * Se nessun record presente nel file [zop] :  *
      *                  * caricamento tabella precablata in memoria   *
      *                  * nel file [zop]                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione contatore generico         *
      *                      *-----------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       ctl-esi-zop-100.
      *                      *-----------------------------------------*
      *                      * Ciclo di scrittura file [zop]           *
      *                      *-----------------------------------------*
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    w-tpm-tip-ope-max
                     go to ctl-esi-zop-900.
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
      *                          * Write record [zop]                  *
      *                          *-------------------------------------*
           perform   wrt-rec-zop-000      thru wrt-rec-zop-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     ctl-esi-zop-100.
       ctl-esi-zop-900.
      *              *-------------------------------------------------*
      *              * Chiusura file [zop]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
       ctl-esi-zop-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [zop]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
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
      *              *-------------------------------------------------*
      *              * [gep]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [zop]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
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
      *              *-------------------------------------------------*
      *              * [gep]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
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
           move      "AC"                 to   w-cod-mne-zop-ope      .
           move      w-tes-cod-top        to   w-cod-mne-zop-cod      .
           move      04                   to   w-cod-mne-zop-lin      .
           move      30                   to   w-cod-mne-zop-pos      .
           move      06                   to   w-cod-mne-zop-dln      .
           move      30                   to   w-cod-mne-zop-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zop-cll-000  thru cod-mne-zop-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zop-foi-000  thru cod-mne-zop-foi-999    .
       acc-cod-top-110.
           perform   cod-mne-zop-cll-000  thru cod-mne-zop-cll-999    .
           if        w-cod-mne-zop-ope    =    "F+"
                     go to acc-cod-top-115.
           if        w-cod-mne-zop-ope    =    "AC"
                     go to acc-cod-top-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-top-115.
           perform   cod-mne-zop-foi-000  thru cod-mne-zop-foi-999    .
           go to     acc-cod-top-110.
       acc-cod-top-120.
           move      w-cod-mne-zop-cod    to   v-num                  .
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
      *                  * Test che il valore esista in tabella        *
      *                  *---------------------------------------------*
           move      zero                 to   w-cix-ctr-001          .
       acc-cod-top-420.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    w-tpm-tip-ope-max
                     go to acc-cod-top-500.
           if        w-tpm-tip-ope-cod
                    (w-cix-ctr-001)       =    w-tes-cod-top
                     go to acc-cod-top-500.
           go to     acc-cod-top-420.
       acc-cod-top-500.
           if        w-cix-ctr-001        not  > w-tpm-tip-ope-max
                     move  spaces         to   w-tes-cod-top-des
                     go to acc-cod-top-520.
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
      *                  *---------------------------------------------*
      *                  * Si/No aggiornamento contabile, globale      *
      *                  *---------------------------------------------*
           perform   acc-snx-agg-000      thru acc-snx-agg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
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
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Flag 03                                     *
      *                  *---------------------------------------------*
           perform   acc-f03-top-000      thru acc-f03-top-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
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
      *              * Si/No aggiornamento contabile, globale          *
      *              *-------------------------------------------------*
           perform   vis-snx-agg-000      thru vis-snx-agg-999        .
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
      *              * Flag 03                                         *
      *              *-------------------------------------------------*
           perform   vis-f03-top-000      thru vis-f03-top-999        .
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
      *              * Si/No aggiornamento contabile, globale          *
      *              *-------------------------------------------------*
           perform   pmt-snx-agg-000      thru pmt-snx-agg-999        .
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
      *              * Flag 03                                         *
      *              *-------------------------------------------------*
           perform   pmt-f03-top-000      thru pmt-f03-top-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/No aggiornamento contabile,   *
      *    *                          globale                          *
      *    *-----------------------------------------------------------*
       pmt-snx-agg-000.
      *              *-------------------------------------------------*
      *              * Se pre-accettazioni : no visualizzazione        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to pmt-snx-agg-999.
      *              *-------------------------------------------------*
      *              * Se tipo operazione non-zero : uscita            *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        not  = zero
                     go to pmt-snx-agg-999.
       pmt-snx-agg-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Descrizione                :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "Personalizzazione si/no aggiornamenti contabili "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "per la gestione portafoglio, parametro globale  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Si/No aggiornamenti        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-agg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione tipo operazione      *
      *    *-----------------------------------------------------------*
       pmt-des-top-000.
      *              *-------------------------------------------------*
      *              * Se pre-accettazioni : si visualizzazione        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to pmt-des-top-100.
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-des-top-999.
       pmt-des-top-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Se pre-accettazioni : si visualizzazione        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to pmt-cod-mne-100.
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-cod-mne-999.
       pmt-cod-mne-100.
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
      *              * Se pre-accettazioni : si visualizzazione        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to pmt-cau-cge-005.
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-cau-cge-999.
       pmt-cau-cge-005.
      *              *-------------------------------------------------*
      *              * Test su codice tipo operazione                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-cau-cge-010
           else if   w-tes-cod-top        =    0102 or
                     w-tes-cod-top        =    0103 or
                     w-tes-cod-top        =    0104 or
                     w-tes-cod-top        =    0105 or
                     w-tes-cod-top        =    0106 or
                     w-tes-cod-top        =    0107 or
                     w-tes-cod-top        =    0108 or
                     w-tes-cod-top        =    0109 or
                     w-tes-cod-top        =    0110 or
                     w-tes-cod-top        =    0111
                     go to pmt-cau-cge-100
           else if   w-tes-cod-top        =    0161
                     go to pmt-cau-cge-160
           else if   w-tes-cod-top        =    0200
                     go to pmt-cau-cge-200
           else if   w-tes-cod-top        =    0301 or
                     w-tes-cod-top        =    0302 or
                     w-tes-cod-top        =    0303 or
                     w-tes-cod-top        =    0304
                     go to pmt-cau-cge-300
           else if   w-tes-cod-top        =    0321 or
                     w-tes-cod-top        =    0322 or
                     w-tes-cod-top        =    0323 or
                     w-tes-cod-top        =    0324
                     go to pmt-cau-cge-320
           else if   w-tes-cod-top        =    0345
                     go to pmt-cau-cge-345
           else if   w-tes-cod-top        =    0350
                     go to pmt-cau-cge-350
           else if   w-tes-cod-top        =    0401 or
                     w-tes-cod-top        =    0402 or
                     w-tes-cod-top        =    0403 or
                     w-tes-cod-top        =    0404
                     go to pmt-cau-cge-400
           else if   w-tes-cod-top        =    0415
                     go to pmt-cau-cge-415
           else if   w-tes-cod-top        =    0501 or
                     w-tes-cod-top        =    0502 or
                     w-tes-cod-top        =    0503 or
                     w-tes-cod-top        =    0504
                     go to pmt-cau-cge-500
           else if   w-tes-cod-top        =    0510 or
                     w-tes-cod-top        =    0515 or
                     w-tes-cod-top        =    0516
                     go to pmt-cau-cge-510
           else if   w-tes-cod-top        =    0520
                     go to pmt-cau-cge-520
           else if   w-tes-cod-top        =    0540
                     go to pmt-cau-cge-540
           else if   w-tes-cod-top        =    0560
                     go to pmt-cau-cge-560
           else if   w-tes-cod-top        =    0600
                     go to pmt-cau-cge-600
           else if   w-tes-cod-top        =    0620
                     go to pmt-cau-cge-620
           else if   w-tes-cod-top        =    0700
                     go to pmt-cau-cge-700
           else if   w-tes-cod-top        =    0720
                     go to pmt-cau-cge-720
           else if   w-tes-cod-top        =    0740
                     go to pmt-cau-cge-740
           else      go to pmt-cau-cge-999.
       pmt-cau-cge-010.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale a zero          *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-cau-001          .
           move      spaces               to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-100.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali 0101..0111      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "  per emissione scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-160.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0161            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "   per cessione scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-200.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0200            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "     per storno scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-300.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 030n      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "per riscossione scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-320.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 032n      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "  per pagamento scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-345.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 0345      *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-cau-001          .
           move      spaces               to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-350.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 0350      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "       per compensazione    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-400.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali 0401..0404      *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-cau-001          .
           move      spaces               to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-415.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0415            *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-cau-001          .
           move      spaces               to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-500.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 050n      *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "    per la presentazione    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-510.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causali serie 051n      *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-cau-001          .
           move      spaces               to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-520.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0520            *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-cau-001          .
           move      spaces               to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-540.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0540            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      " per spese e/o interessi    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-560.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0560            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "  per accredito distinta    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-600.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0600            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "          per l'insoluto    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-620.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0620            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "         per il richiamo    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-700.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0700            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "  per accredito scadenza    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-720.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0720            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "       per il buon esito    "
                                          to   w-pmt-cau-002          .
           go to     pmt-cau-cge-900.
       pmt-cau-cge-740.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per causale 0740            *
      *              *-------------------------------------------------*
           move      "Codice causale contabile   :"
                                          to   w-pmt-cau-001          .
           move      "       per il buon esito    "
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
      *              * Se pre-accettazioni : si visualizzazione        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to pmt-stc-cge-005.
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-stc-cge-999.
       pmt-stc-cge-005.
      *              *-------------------------------------------------*
      *              * Test su codice tipo operazione                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-stc-cge-010
           else if   w-tes-cod-top        =    0102 or
                     w-tes-cod-top        =    0103 or
                     w-tes-cod-top        =    0104 or
                     w-tes-cod-top        =    0105 or
                     w-tes-cod-top        =    0106 or
                     w-tes-cod-top        =    0107 or
                     w-tes-cod-top        =    0108 or
                     w-tes-cod-top        =    0109 or
                     w-tes-cod-top        =    0110 or
                     w-tes-cod-top        =    0111
                     go to pmt-stc-cge-100
           else if   w-tes-cod-top        =    0161
                     go to pmt-stc-cge-160
           else if   w-tes-cod-top        =    0200
                     go to pmt-stc-cge-200
           else if   w-tes-cod-top        =    0301 or
                     w-tes-cod-top        =    0302 or
                     w-tes-cod-top        =    0303 or
                     w-tes-cod-top        =    0304
                     go to pmt-stc-cge-300
           else if   w-tes-cod-top        =    0321 or
                     w-tes-cod-top        =    0322 or
                     w-tes-cod-top        =    0323 or
                     w-tes-cod-top        =    0324
                     go to pmt-stc-cge-320
           else if   w-tes-cod-top        =    0345
                     go to pmt-stc-cge-345
           else if   w-tes-cod-top        =    0350
                     go to pmt-stc-cge-350
           else if   w-tes-cod-top        =    0401 or
                     w-tes-cod-top        =    0402 or
                     w-tes-cod-top        =    0403 or
                     w-tes-cod-top        =    0404
                     go to pmt-stc-cge-400
           else if   w-tes-cod-top        =    0415
                     go to pmt-stc-cge-415
           else if   w-tes-cod-top        =    0501 or
                     w-tes-cod-top        =    0502 or
                     w-tes-cod-top        =    0503 or
                     w-tes-cod-top        =    0504
                     go to pmt-stc-cge-500
           else if   w-tes-cod-top        =    0510 or
                     w-tes-cod-top        =    0515 or
                     w-tes-cod-top        =    0516
                     go to pmt-stc-cge-510
           else if   w-tes-cod-top        =    0520
                     go to pmt-stc-cge-520
           else if   w-tes-cod-top        =    0540
                     go to pmt-stc-cge-540
           else if   w-tes-cod-top        =    0560
                     go to pmt-stc-cge-560
           else if   w-tes-cod-top        =    0600
                     go to pmt-stc-cge-600
           else if   w-tes-cod-top        =    0620
                     go to pmt-stc-cge-620
           else if   w-tes-cod-top        =    0700
                     go to pmt-stc-cge-700
           else if   w-tes-cod-top        =    0720
                     go to pmt-stc-cge-720
           else if   w-tes-cod-top        =    0740
                     go to pmt-stc-cge-740
           else      go to pmt-stc-cge-999.
       pmt-stc-cge-010.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per tipo operazione a zero  *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-100.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causali se-  *
      *              *                                      rie 01nn   *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "      emissione scadenza    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-160.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0161 *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "       cessione scadenza    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-200.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0200 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-300.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causali se-  *
      *              *                                      rie 030n   *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "         abbuoni passivi    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-320.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causali se-  *
      *              *                                      rie 032n   *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "         abbuoni passivi    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-345.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto per la cau-  *
      *              *                                      sale 0345  *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-350.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causali se-  *
      *              *                                      rie 0350   *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "         abbuoni passivi    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-400.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causali da   *
      *              *                                   0401 a 0404   *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-415.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto per la cau-  *
      *              *                                      sale 0415  *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-500.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causali se-  *
      *              *                                      rie 050n   *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-510.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causali se-  *
      *              *                                      rie 051n   *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-520.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0520 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-540.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0540 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-560.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0560 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-600.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0600 *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "                insoluti    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-620.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0620 *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-stc-001          .
           move      "      effetti richiamati    "
                                          to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-700.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0700 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-720.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0720 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
           go to     pmt-stc-cge-900.
       pmt-stc-cge-740.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per sottoconto causale 0740 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-stc-001          .
           move      spaces               to   w-pmt-stc-002          .
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
      *              * Se pre-accettazioni : si visualizzazione        *
      *              *-------------------------------------------------*
           if        w-cnt-mfu-tip-imp    =    spaces
                     go to pmt-ctp-cge-005.
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-ctp-cge-999.
       pmt-ctp-cge-005.
      *              *-------------------------------------------------*
      *              * Test su codice tipo operazione                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pmt-ctp-cge-010
           else if   w-tes-cod-top        =    0102 or
                     w-tes-cod-top        =    0103 or
                     w-tes-cod-top        =    0104 or
                     w-tes-cod-top        =    0105 or
                     w-tes-cod-top        =    0106 or
                     w-tes-cod-top        =    0109 or
                     w-tes-cod-top        =    0110 or
                     w-tes-cod-top        =    0111 or
                     w-tes-cod-top        =    0161
                     go to pmt-ctp-cge-100
           else if   w-tes-cod-top        =    0301 or
                     w-tes-cod-top        =    0302 or
                     w-tes-cod-top        =    0303 or
                     w-tes-cod-top        =    0304
                     go to pmt-ctp-cge-300
           else if   w-tes-cod-top        =    0321 or
                     w-tes-cod-top        =    0322 or
                     w-tes-cod-top        =    0323 or
                     w-tes-cod-top        =    0324
                     go to pmt-ctp-cge-320
           else if   w-tes-cod-top        =    0350
                     go to pmt-ctp-cge-350
           else      go to pmt-ctp-cge-800.
       pmt-ctp-cge-010.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per tipo operazione a zero  *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-ctp-001          .
           move      spaces               to   w-pmt-ctp-002          .
           move      spaces               to   w-pmt-ctp-003          .
           go to     pmt-ctp-cge-900.
       pmt-ctp-cge-100.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per contropartite causali   *
      *              * da 0102 a 0161                                  *
      *              *-------------------------------------------------*
           move      "Contropartita contabile    :"
                                          to   w-pmt-ctp-001          .
           move      "   per la presentazione     "
                                          to   w-pmt-ctp-002          .
           move      "         della distinta     "
                                          to   w-pmt-ctp-003          .
           go to     pmt-ctp-cge-900.
       pmt-ctp-cge-300.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per contropartite causali   *
      *              * serie 030n                                      *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-ctp-001          .
           move      "          abbuoni attivi    "
                                          to   w-pmt-ctp-002          .
           move      spaces               to   w-pmt-ctp-003          .
           go to     pmt-ctp-cge-900.
       pmt-ctp-cge-320.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per contropartite causali   *
      *              * serie 032n                                      *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-ctp-001          .
           move      "          abbuoni attivi    "
                                          to   w-pmt-ctp-002          .
           move      spaces               to   w-pmt-ctp-003          .
           go to     pmt-ctp-cge-900.
       pmt-ctp-cge-350.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per contropartite causali   *
      *              * serie 0350                                      *
      *              *-------------------------------------------------*
           move      "Sottoconto contabile per   :"
                                          to   w-pmt-ctp-001          .
           move      "          abbuoni attivi    "
                                          to   w-pmt-ctp-002          .
           move      spaces               to   w-pmt-ctp-003          .
           go to     pmt-ctp-cge-900.
       pmt-ctp-cge-800.
      *              *-------------------------------------------------*
      *              * Preparazione prompt per tutte le contropartite  *
      *              * rimanenti                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-pmt-ctp-001          .
           move      spaces               to   w-pmt-ctp-002          .
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
      *    * Visualizzazione prompt : Flag 03                          *
      *    *-----------------------------------------------------------*
       pmt-f03-top-000.
      *              *-------------------------------------------------*
      *              * Test su codice tipo operazione                  *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        not  = 0600
                     go to pmt-f03-top-999.
       pmt-f03-top-900.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Accettazione descrizione   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       causale contabile    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-f03-top-999.
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
      *    * Accettazione campo testata : Si/No aggiornamento contabi- *
      *    *                              le, parametro globale        *
      *    *-----------------------------------------------------------*
       acc-snx-agg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo operazione non-zero : uscita        *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        not  = zero
                     go to acc-snx-agg-999.
       acc-snx-agg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-agg-lun    to   v-car                  .
           move      w-exp-snx-agg-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-agg-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-tes-snx-agg (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-agg (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-agg-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-agg-999.
       acc-snx-agg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-tes-snx-agg (1)
           else if   v-num                =    02
                     move  "N"            to   w-tes-snx-agg (1)
           else      move  spaces         to   w-tes-snx-agg (1)      .
       acc-snx-agg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a spaces : reimpostazione, a meno *
      *                  * che non sia su tasto Up                     *
      *                  *---------------------------------------------*
           if        w-tes-snx-agg (1)    not  = spaces
                     go to acc-snx-agg-600.
           if        v-key                =    "UP  "
                     go to acc-snx-agg-600
           else      go to acc-snx-agg-100.
       acc-snx-agg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-agg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-agg-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-agg-100.
       acc-snx-agg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Si/No aggiornamento con-  *
      *    *                                 tabile, parametro globale *
      *    *-----------------------------------------------------------*
       vis-snx-agg-000.
      *              *-------------------------------------------------*
      *              * Se tipo operazione non-zero : uscita            *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        not  = zero
                     go to vis-snx-agg-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-agg-lun    to   v-car                  .
           move      w-exp-snx-agg-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-agg-tbl    to   v-txt                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-tes-snx-agg (1)    =    "S"
                     move  01             to   v-num
           else if   w-tes-snx-agg (1)    =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-agg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Descrizione tipo operazione  *
      *    *-----------------------------------------------------------*
       acc-des-top-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo operazione zero : uscita            *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to acc-des-top-999.
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
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-des-top-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Se tipo operazione zero : uscita            *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to acc-cod-mne-999.
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
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-cod-mne-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
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
      *                  * Se tipo operazione zero : uscita            *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to acc-cau-cge-999.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    0101 or
                     w-tes-cod-top        =    0345 or
                     w-tes-cod-top        =    0401 or
                     w-tes-cod-top        =    0402 or
                     w-tes-cod-top        =    0403 or
                     w-tes-cod-top        =    0404 or
                     w-tes-cod-top        =    0415 or
                     w-tes-cod-top        =    0510 or
                     w-tes-cod-top        =    0515 or
                     w-tes-cod-top        =    0516 or
                     w-tes-cod-top        =    0520 or
                     w-tes-cod-top        =    0741
                     go to acc-cau-cge-999.
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
                     go to acc-cau-cge-460.
           move      "ME"                 to   v-ope                  .
           move      "La causale contabile e' di rettifica bilancio !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-cau-cge-100.
       acc-cau-cge-460.
      *                  *---------------------------------------------*
      *                  * Se causale iva : messaggio di errore e re-  *
      *                  * impostazione                                *
      *                  *---------------------------------------------*
           if        w-let-arc-zcc-tmi    =    "0" or
                     w-let-arc-zcc-tmi    =    spaces
                     go to acc-cau-cge-600.
           move      "ME"                 to   v-ope                  .
           move      "La causale contabile e' di tipo iva!"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-cau-cge-100.
       acc-cau-cge-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-cau-cge-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-des-cau-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
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
      *                  * Se tipo operazione zero : uscita            *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to acc-stc-cge-999.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    0101 or
                     w-tes-cod-top        =    0200 or
                     w-tes-cod-top        =    0345 or
                     w-tes-cod-top        =    0401 or
                     w-tes-cod-top        =    0402 or
                     w-tes-cod-top        =    0403 or
                     w-tes-cod-top        =    0404 or
                     w-tes-cod-top        =    0415 or
                     w-tes-cod-top        =    0501 or
                     w-tes-cod-top        =    0502 or
                     w-tes-cod-top        =    0503 or
                     w-tes-cod-top        =    0504 or
                     w-tes-cod-top        =    0510 or
                     w-tes-cod-top        =    0515 or
                     w-tes-cod-top        =    0516 or
                     w-tes-cod-top        =    0520 or
                     w-tes-cod-top        =    0540 or
                     w-tes-cod-top        =    0560 or
                     w-tes-cod-top        =    0700 or
                     w-tes-cod-top        =    0720 or
                     w-tes-cod-top        =    0740 or
                     w-tes-cod-top        =    0741
                     go to acc-stc-cge-999.
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
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-stc-cge-999.
       vis-stc-cge-100.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing con appoggio a sinistra             *
      *                  *---------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-stc-cge (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-des-stc-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
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
      *                  * Se tipo operazione zero : uscita            *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to acc-ctp-cge-999.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        =    0101 or
                     w-tes-cod-top        =    0107 or
                     w-tes-cod-top        =    0108 or
                     w-tes-cod-top        =    0200 or
                     w-tes-cod-top        =    0345 or
                     w-tes-cod-top        =    0401 or
                     w-tes-cod-top        =    0402 or
                     w-tes-cod-top        =    0403 or
                     w-tes-cod-top        =    0404 or
                     w-tes-cod-top        =    0415 or
                     w-tes-cod-top        =    0501 or
                     w-tes-cod-top        =    0502 or
                     w-tes-cod-top        =    0503 or
                     w-tes-cod-top        =    0504 or
                     w-tes-cod-top        =    0510 or
                     w-tes-cod-top        =    0515 or
                     w-tes-cod-top        =    0516 or
                     w-tes-cod-top        =    0520 or
                     w-tes-cod-top        =    0540 or
                     w-tes-cod-top        =    0560 or
                     w-tes-cod-top        =    0600 or
                     w-tes-cod-top        =    0620 or
                     w-tes-cod-top        =    0700 or
                     w-tes-cod-top        =    0720 or
                     w-tes-cod-top        =    0740 or
                     w-tes-cod-top        =    0741
                     go to acc-ctp-cge-999.
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
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-ctp-cge-999.
       vis-ctp-cge-100.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing con appoggio a sinistra             *
      *                  *---------------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-tes-ctp-cge (1)    to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Se tipo operazione zero : uscita                *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to vis-des-ctp-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
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
      *    * Accettazione campo : Flag 03                              *
      *    *-----------------------------------------------------------*
       acc-f03-top-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se tipo operazione : insoluto          *
      *                  *---------------------------------------------*
           if        w-tes-cod-top        not  = 0600
                     go to acc-f03-top-999.
      *                  *---------------------------------------------*
      *                  * Solo se codice causale non a zero           *
      *                  *---------------------------------------------*
           if        w-tes-cau-cge (1)    =    zero
                     go to acc-f03-top-999.
       acc-f03-top-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-f03-top-lun    to   v-car                  .
           move      w-exp-f03-top-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-f03-top-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-tes-f03-top (1)    =    00
                     move  01             to   v-num
           else if   w-tes-f03-top (1)    =    01
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-f03-top-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-f03-top-999.
       acc-f03-top-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  00             to   w-tes-f03-top (1)
           else if   v-num                =    02
                     move  01             to   w-tes-f03-top (1)
           else      move  spaces         to   w-tes-f03-top (1)      .
       acc-f03-top-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-f03-top-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-f03-top-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-f03-top-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-f03-top-100.
       acc-f03-top-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Flag 03                           *
      *    *-----------------------------------------------------------*
       vis-f03-top-000.
      *              *-------------------------------------------------*
      *              * Solo se tipo operazione : insoluto              *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        not  = 0600
                     go to vis-f03-top-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : visualizzazione                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-f03-top-lun    to   v-car                  .
           move      w-exp-f03-top-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-f03-top-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-tes-f03-top (1)    =    00
                     move  01             to   v-num
           else if   w-tes-f03-top (1)    =    01
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-f03-top-999.
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
       cnt-tdo-nok-025.
      *              *-------------------------------------------------*
      *              * Test su si/no aggiornamento                     *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        not  = zero
                     go to cnt-tdo-nok-050.
           if        w-tes-snx-agg (1)    not  = spaces
                     go to cnt-tdo-nok-027.
           move      "Manca la voce Si/No aggiornamento                 
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-027.
           if        w-tes-snx-agg (1)    =    "S" or
                     w-tes-snx-agg (1)    =    "N"
                     go to cnt-tdo-nok-050.
           move      "Voce Si/No aggiornamento errata                   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
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
      *              * Test su sottoconto contabile                    *
      *              *                                                 *
      *              * Nota : Le istruzioni seguenti sono state bypas- *
      *              *        sate, in quanto ora si permette di non   *
      *              *        impostare il sottoconto. In tal caso la  *
      *              *        causale contabile diventa inutile.       *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-150.
       cnt-tdo-nok-101.
           if        w-tes-cod-top        =    zero
                     go to cnt-tdo-nok-150.
           if        w-tes-cod-top        =    0600 or
                     w-tes-cod-top        =    0620
                     go to cnt-tdo-nok-150.
           if        w-tes-cod-top        not  < 0101 and
                     w-tes-cod-top        not  > 0111
                     go to cnt-tdo-nok-105
           else if   w-tes-cod-top        not  < 0301 and
                     w-tes-cod-top        not  > 0304
                     go to cnt-tdo-nok-105
           else if   w-tes-cod-top        =    0161
                     go to cnt-tdo-nok-105
           else      go to cnt-tdo-nok-150.
       cnt-tdo-nok-105.
           if        w-tes-cau-cge (1)    =    zero
                     go to cnt-tdo-nok-150.
           if        w-tes-stc-cge (1)    not  = zero
                     go to cnt-tdo-nok-150.
           if        w-tes-cod-top        not  < 0301 and
                     w-tes-cod-top        not  > 0304
                     go to cnt-tdo-nok-110
           else      go to cnt-tdo-nok-115.
       cnt-tdo-nok-110.
           move      "Manca il sottoconto per gli abbuoni passivi       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-115.
           move      "Manca il sottoconto contabile                     
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-150.
      *              *-------------------------------------------------*
      *              * Test su contropartita contabile                 *
      *              *                                                 *
      *              * Nota : Le istruzioni seguenti sono state bypas- *
      *              *        sate, in quanto ora si permette di non   *
      *              *        impostare il sottoconto. In tal caso la  *
      *              *        causale contabile diventa inutile.       *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-200.
       cnt-tdo-nok-151.
           if        w-tes-cod-top        =    zero
                     go to cnt-tdo-nok-200.
           if        w-tes-cod-top        not  < 0301 and
                     w-tes-cod-top        not  > 0304
                     go to cnt-tdo-nok-155
           else      go to cnt-tdo-nok-200.
       cnt-tdo-nok-155.
           if        w-tes-cau-cge (1)    =    zero
                     go to cnt-tdo-nok-200.
           if        w-tes-ctp-cge (1)    not  = zero
                     go to cnt-tdo-nok-200.
           if        w-tes-cod-top        not  < 0301 and
                     w-tes-cod-top        not  > 0304
                     go to cnt-tdo-nok-160
           else      go to cnt-tdo-nok-165.
       cnt-tdo-nok-160.
           move      "Manca il sottoconto per gli abbuoni attivi        
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-165.
           move      "Manca la contropartita contabile                  
      -              "               "    to   w-err-box-err-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-200.
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
      *              * Se tipo operazione a zero si esegue la routine  *
      *              * specifica in alternativa a quella normale       *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        not  = zero
                     go to rou-let-reg-025.
           perform   rou-let-opz-000      thru rou-let-opz-999        .
           go to     rou-let-reg-999.
       rou-let-reg-025.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOP    "         to   f-key                  .
           move      w-tes-cod-top        to   rf-zop-cod-top         .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
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
      *                          * record [zop]                        *
      *                          *-------------------------------------*
           move      rf-zop-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-zop-des-key       to   w-tes-des-key (1)      .
           move      rf-zop-des-top       to   w-tes-des-top (1)      .
           move      rf-zop-cau-cge       to   w-tes-cau-cge (1)      .
           move      rf-zop-stc-cge       to   w-tes-stc-cge (1)      .
           move      rf-zop-ctp-cge       to   w-tes-ctp-cge (1)      .
           move      rf-zop-f01-top       to   w-tes-f01-top (1)      .
           move      rf-zop-f02-top       to   w-tes-f02-top (1)      .
           move      rf-zop-f03-top       to   w-tes-f03-top (1)      .
           move      rf-zop-f04-top       to   w-tes-f04-top (1)      .
           move      rf-zop-f05-top       to   w-tes-f05-top (1)      .
           move      rf-zop-alx-exp       to   w-tes-alx-exp (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zop]                        *
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
      *    * Lettura registrazione pre-esistente per tipo operazione a *
      *    * zero, ovvero per si/no aggiornamenti contabili            *
      *    *-----------------------------------------------------------*
       rou-let-opz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-let-reg      .
       rou-let-opz-005.
      *              *-------------------------------------------------*
      *              * Ricostruzione prompts di testata                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompts di testata                          *
      *                  *---------------------------------------------*
           perform   pmt-tes-reg-000      thru pmt-tes-reg-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       rou-let-opz-025.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione da file [gep]         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGEP    "         to   f-key                  .
           move      01                   to   rf-gep-tip-gep         .
           move      "CGE       "         to   rf-gep-cod-gep         .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
           if        f-sts                =    e-not-err
                     go to rou-let-opz-100.
       rou-let-opz-050.
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
           go to     rou-let-opz-999.
       rou-let-opz-100.
      *                  *---------------------------------------------*
      *                  * Se anagrafica trovata                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Modifica           *
      *                      *-----------------------------------------*
           move      "M"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Dati record in area per ridefinizione   *
      *                      * delle personalizzazioni portafoglio per *
      *                      * tipo record 01                          *
      *                      *-----------------------------------------*
           move      rf-gep-dat-rec       to   w-fil-gep-001          .
      *                      *-----------------------------------------*
      *                      * Determinazione valori attuali           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori contenuti direttamente in    *
      *                          * record [gep]                        *
      *                          *-------------------------------------*
           move      w-fil-gep-001-agg    to   w-tes-snx-agg (1)      .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
       rou-let-opz-999.
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
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pos-cnf-ins-200
           else      go to pos-cnf-ins-400.
       pos-cnf-ins-200.
      *              *-------------------------------------------------*
      *              * Se tipo operazione : zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura movimento per operazione zero     *
      *                  *---------------------------------------------*
           perform   scr-mov-opz-000      thru scr-mov-opz-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-cnf-ins-999.
       pos-cnf-ins-400.
      *              *-------------------------------------------------*
      *              * Se tipo operazione : non-zero                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura movimento su files                *
      *                  *---------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-cnf-ins-999.
       pos-cnf-ins-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di modifica                         *
      *    *-----------------------------------------------------------*
       pos-cnf-mod-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pos-cnf-mod-200
           else      go to pos-cnf-mod-400.
       pos-cnf-mod-200.
      *              *-------------------------------------------------*
      *              * Se tipo operazione : zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura movimento per operazione zero     *
      *                  *---------------------------------------------*
           perform   scr-mov-opz-000      thru scr-mov-opz-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-cnf-mod-999.
       pos-cnf-mod-400.
      *              *-------------------------------------------------*
      *              * Se tipo operazione : non-zero                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura movimento su files                *
      *                  *---------------------------------------------*
           perform   scr-mov-fil-000      thru scr-mov-fil-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-cnf-mod-999.
       pos-cnf-mod-999.
           exit.

      *    *===========================================================*
      *    * Routine post-conferma di annullamento                     *
      *    *-----------------------------------------------------------*
       pos-cnf-ann-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo operazione        *
      *              *-------------------------------------------------*
           if        w-tes-cod-top        =    zero
                     go to pos-cnf-ann-200
           else      go to pos-cnf-ann-400.
       pos-cnf-ann-200.
      *              *-------------------------------------------------*
      *              * Se tipo operazione : zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete movimento per operazione zero        *
      *                  *---------------------------------------------*
           perform   del-mov-opz-000      thru del-mov-opz-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-cnf-ann-999.
       pos-cnf-ann-400.
      *              *-------------------------------------------------*
      *              * Se tipo operazione : non-zero                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete movimento da files                   *
      *                  *---------------------------------------------*
           perform   del-mov-fil-000      thru del-mov-fil-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pos-cnf-ann-999.
       pos-cnf-ann-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento su file                               *
      *    *-----------------------------------------------------------*
       scr-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [zop]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Write record [zop]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-zop-000      thru wrt-rec-zop-999        .
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [zop]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-zop-000      thru rew-rec-zop-999        .
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
      *              *-------------------------------------------------*
      *              * Delete record [zop]                             *
      *              *-------------------------------------------------*
           perform   del-rec-zop-000      thru del-rec-zop-999        .
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [zop]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-zop-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-tes-cod-top        to   rf-zop-cod-top         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-zop-ide-dat         .
           move      s-ute                to   rf-zop-ide-ute         .
           move      s-fas                to   rf-zop-ide-fas         .
           move      w-tes-cod-mne (1)    to   rf-zop-cod-mne         .
           move      w-tes-des-key (1)    to   rf-zop-des-key         .
           move      w-tes-des-top (1)    to   rf-zop-des-top         .
           move      w-tes-cau-cge (1)    to   rf-zop-cau-cge         .
           move      w-tes-stc-cge (1)    to   rf-zop-stc-cge         .
           move      w-tes-ctp-cge (1)    to   rf-zop-ctp-cge         .
           move      w-tes-f01-top (1)    to   rf-zop-f01-top         .
           move      w-tes-f02-top (1)    to   rf-zop-f02-top         .
           move      w-tes-f03-top (1)    to   rf-zop-f03-top         .
           move      w-tes-f04-top (1)    to   rf-zop-f04-top         .
           move      w-tes-f05-top (1)    to   rf-zop-f05-top         .
           move      w-tes-alx-exp (1)    to   rf-zop-alx-exp         .
       cmp-rec-zop-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [zop]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-zop-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zop-000      thru cmp-rec-zop-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
       wrt-rec-zop-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [zop]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-zop-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zop-000      thru cmp-rec-zop-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
       rew-rec-zop-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [zop]                                *
      *    *-----------------------------------------------------------*
       del-rec-zop-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-zop-000      thru cmp-rec-zop-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
       del-rec-zop-999.
           exit.

      *    *===========================================================*
      *    * Scrittura movimento per operazione zero, ovvero per per-  *
      *    * sonalizzazione si/no aggiornamenti contabili per gestio-  *
      *    * ne portafoglio                                            *
      *    *-----------------------------------------------------------*
       scr-mov-opz-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [gep]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-opz-500.
      *                      *-----------------------------------------*
      *                      * Write record [gep]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-gep-000      thru wrt-rec-gep-999        .
           go to     scr-mov-opz-999.
       scr-mov-opz-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rewrite record [gep]                    *
      *                      *-----------------------------------------*
           perform   rew-rec-gep-000      thru rew-rec-gep-999        .
       scr-mov-opz-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento per operazione zero, ovvero per persona- *
      *    * lizzazione si/no aggiornamenti contabili per gestione     *
      *    * portafoglio                                               *
      *    *-----------------------------------------------------------*
       del-mov-opz-000.
      *              *-------------------------------------------------*
      *              * Delete record [gep]                             *
      *              *-------------------------------------------------*
           perform   del-rec-gep-000      thru del-rec-gep-999        .
       del-mov-opz-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [gep]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-gep-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      01                   to   rf-gep-tip-gep         .
           move      "CGE       "         to   rf-gep-cod-gep         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-gep-ide-dat         .
           move      s-ute                to   rf-gep-ide-ute         .
           move      s-fas                to   rf-gep-ide-fas         .
           move      w-tes-snx-agg (1)    to   w-fil-gep-001-agg      .
           move      w-fil-gep-001        to   rf-gep-dat-rec         .
       cmp-rec-gep-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [gep]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-gep-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-gep-000      thru cmp-rec-gep-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
       wrt-rec-gep-999.
           exit.

      *    *===========================================================*
      *    * Riscrittura record [gep]                                  *
      *    *-----------------------------------------------------------*
       rew-rec-gep-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-gep-000      thru cmp-rec-gep-999        .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           move      "FP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
       rew-rec-gep-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [gep]                                *
      *    *-----------------------------------------------------------*
       del-rec-gep-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-gep-000      thru cmp-rec-gep-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
       del-rec-gep-999.
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
      *    * Subroutines per l'accettazione del codice operazione      *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzop0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione della causale contabile    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottoconto      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acs"                   .
