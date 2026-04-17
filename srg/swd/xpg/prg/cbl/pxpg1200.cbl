       Identification Division.
       Program-Id.                                 pxpg1200           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    lpg                 *
      *                                   Fase:    xpg120              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/03/93    *
      *                       Ultima revisione:    NdK del 04/04/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione personalizzazioni generali         *
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
                     "swd"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "xpg"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "lpg"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "xpg120"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg1200  "                                     .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      PERSONALIZZAZIONI GENERALI       "       .

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
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

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
      *    * Area di comunicazione per moduli                "maucmf"  *
      *    *-----------------------------------------------------------*
       01  j.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  j-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  j-tre                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  j-kre                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  j-dat.
               10  j-chr occurs 2048      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        *-------------------------------------------------------*
           05  j-rsc                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return message                                        *
      *        *-------------------------------------------------------*
           05  j-msg                      pic  x(80)                  .

      *    *===========================================================*
      *    * Work per records di [auc] 'psg'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpsg0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per valori ottenuti dal modulo 'mopsys'         *
      *    *-----------------------------------------------------------*
       01  w-mop-sys.
      *        *-------------------------------------------------------*
      *        * Si/no stampa in spool ammessa per il sistema opera-   *
      *        * tivo ospite                                           *
      *        *  - Spaces : Si                                        *
      *        *  - ##     : No                                        *
      *        *-------------------------------------------------------*
           05  w-mop-sys-snx-ssp              pic  x(02)              .

      *    *===========================================================*
      *    * Work-area per accettazioni                                *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per accettazione template spooler di stampa      *
      *        *-------------------------------------------------------*
           05  w-acc-tem-spl.
      *            *---------------------------------------------------*
      *            * Literal di note operative                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-spl-nop      pic  x(60) value
                     "Digitare: 'lpr' o 'lp' o 'aix' o 'dos' oppure un c
      -              "omando.   "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler lpr                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-spl-lpr      pic  x(60) value
                     "(p=@p; f=@f; cat $f | lpr -P $p -l; rm -f $f) &   
      -              "          "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler lp                          *
      *            *---------------------------------------------------*
               10  w-acc-tem-spl-lpx      pic  x(60) value
                     "(p=@p; f=@f; lp -d$p -s $f) &                     
      -              "          "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler aix                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-spl-aix      pic  x(60) value
                     "(p=@p; f=@f; cat $f | lpr -P $p -h -l -s; rm -f $f
      -              ") &       "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler dos                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-spl-dos      pic  x(60) value
                     ".                                                 
      -              "          "                                     .
      *        *-------------------------------------------------------*
      *        * Work per accettazione template blocco spooler di      *
      *        * stampa                                                *
      *        *-------------------------------------------------------*
           05  w-acc-tem-bss.
      *            *---------------------------------------------------*
      *            * Literal di note operative                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-bss-nop      pic  x(60) value
                     "Digitare: 'lpr' o 'lp' o 'aix' o 'dos' oppure un c
      -              "omando.   "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler lpr                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-bss-lpr      pic  x(60) value
                     "(p=@p; /usr/bin/lprm -P$p > /dev/null) &          
      -              "          "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler lp                          *
      *            *---------------------------------------------------*
               10  w-acc-tem-bss-lpx      pic  x(60) value
                     "(p=@p; /usr/bin/cancel -a $p > /dev/null) &       
      -              "          "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler aix                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-bss-aix      pic  x(60) value
                     "(p=@p; enable $p; cancel $p) &                    
      -              "          "                                     .
      *            *---------------------------------------------------*
      *            * Valore per lo spooler dos                         *
      *            *---------------------------------------------------*
               10  w-acc-tem-bss-dos      pic  x(60) value
                     ".                                                 
      -              "          "                                     .
      *        *-------------------------------------------------------*
      *        * Work per accettazione parametro di controllo per la   *
      *        * visualizzazione archivi stampa                        *
      *        *-------------------------------------------------------*
           05  w-acc-vis-prf.
      *            *---------------------------------------------------*
      *            * 1. e 2. carattere                                 *
      *            *---------------------------------------------------*
               10  w-acc-vis-prf-1e2.
      *                *-----------------------------------------------*
      *                * 1. carattere                                  *
      *                *-----------------------------------------------*
                   15  w-acc-vis-prf-001  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * 2. carattere                                  *
      *                *-----------------------------------------------*
                   15  w-acc-vis-prf-002  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Altri 8 caratteri residui                         *
      *            *---------------------------------------------------*
               10  w-acc-vis-prf-alt      pic  x(08)                  .

      *    *===========================================================*
      *    * Work-area per accettazione campi espansi                  *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per accettazione scelta No/si                    *
      *        *-------------------------------------------------------*
           05  w-exp-sce-nsx.
               10  w-exp-sce-nsx-num      pic  9(02)       value 02   .
               10  w-exp-sce-nsx-lun      pic  9(02)       value 02   .
               10  w-exp-sce-nsx-tbl.
                   15  filler             pic  x(02) value
                            "No"                                      .
                   15  filler             pic  x(02) value
                            "Si"                                      .
      *        *-------------------------------------------------------*
      *        * Work per accettazione visualizzazione codice azienda  *
      *        *-------------------------------------------------------*
           05  w-exp-snx-azi.
               10  w-exp-snx-azi-num      pic  9(02)       value 02   .
               10  w-exp-snx-azi-lun      pic  9(02)       value 17   .
               10  w-exp-snx-azi-tbl.
                   15  filler             pic  x(17) value
                            "Visualizzato     "                       .
                   15  filler             pic  x(17) value
                            "Non visualizzato "                       .
      *        *-------------------------------------------------------*
      *        * Work per accettazione ora legale o solare             *
      *        *-------------------------------------------------------*
           05  w-exp-ora-sol.
               10  w-exp-ora-sol-num      pic  9(02)       value 02   .
               10  w-exp-ora-sol-lun      pic  9(02)       value 10   .
               10  w-exp-ora-sol-tbl.
                   15  filler             pic  x(10) value
                            "ora Solare"                              .
                   15  filler             pic  x(10) value
                            "ora Legale"                              .
      *        *-------------------------------------------------------*
      *        * Work per accettazione visualizzazione archivi stampa  *
      *        *-------------------------------------------------------*
           05  w-exp-vis-prf.
               10  w-exp-vis-prf-num      pic  9(02)       value 04   .
               10  w-exp-vis-prf-lun      pic  9(02)       value 50   .
               10  w-exp-vis-prf-tbl.
                   15  filler             pic  x(50) value
                  "Su 18 linee, sempre su 80 colonne                 ".
                   15  filler             pic  x(50) value
                  "Su 18 linee, quando necessario su 132 colonne     ".
                   15  filler             pic  x(50) value
                  "Su 24 linee, sempre su 80 colonne                 ".
                   15  filler             pic  x(50) value
                  "Su 24 linee, quando necessario su 132 colonne     ".
      *        *-------------------------------------------------------*
      *        * Work per Azione password scaduta                      *
      *        *-------------------------------------------------------*
           05  w-exp-tip-asp.
               10  w-exp-tip-asp-num      pic  9(02)       value 02   .
               10  w-exp-tip-asp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-asp-tbl.
                   15  filler             pic  x(40) value
                            "Nessuna                                 ".
                   15  filler             pic  x(40) value
                            "Blocco dell'utente                      ".
______*            15  filler             pic  x(40) value
______*                     "messaggio all'Amministratore del sistema".
______*            15  filler             pic  x(40) value
______*                     "messaggio all'Utente                    ".
      *        *-------------------------------------------------------*
      *        * Work per Azione password errata                       *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ast.
               10  w-exp-tip-ast-num      pic  9(02)       value 02   .
               10  w-exp-tip-ast-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ast-tbl.
                   15  filler             pic  x(40) value
                            "Nessuna                                 ".
                   15  filler             pic  x(40) value
                            "Blocco dell'utente                      ".
______*            15  filler             pic  x(40) value
______*                     "messaggio all'Amministratore del sistema".
______*            15  filler             pic  x(40) value
______*                     "messaggio all'Utente                    ".
      *        *-------------------------------------------------------*
      *        * Work per Agente di spedizione documenti               *
      *        *-------------------------------------------------------*
           05  w-exp-asd-smp.
               10  w-exp-asd-smp-num      pic  9(02)       value 04   .
               10  w-exp-asd-smp-lun      pic  9(02)       value 40   .
               10  w-exp-asd-smp-tbl.
                   15  filler             pic  x(40) value
                            "Senza autenticazione                    ".
                   15  filler             pic  x(40) value
                            "Con autenticazione MD5                  ".
                   15  filler             pic  x(40) value
                            "Con autenticazione TLS                  ".
                   15  filler             pic  x(40) value
                            "Con autenticazione SSL (per invio PEC)  ".

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cpw"                   .

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
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     main-800.
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
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     main-800.
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
      *              * Tasto di funzione Delt : non abilitato          *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk (19)             .
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
           move      "Conferma operazione di annullamento (S/N) ?"
                                          to   v-not                  .
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
      *              * Open moduli accettazione                        *
      *              *-------------------------------------------------*
           perform   opn-mdl-acc-000      thru opn-mdl-acc-999        .
      *              *-------------------------------------------------*
      *              * Richiesta al modulo di interfaccia con il si-   *
      *              * stema operativo ospite di ammissibilita' o no   *
      *              * di stampa in spool, e memorizzazione del ri-    *
      *              * sultato                                         *
      *              *-------------------------------------------------*
           move      "SS"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-sts                to   w-mop-sys-snx-ssp      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close moduli accettazione                       *
      *              *-------------------------------------------------*
           perform   cls-mdl-acc-000      thru cls-mdl-acc-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di accettazione                               *
      *    *-----------------------------------------------------------*
       opn-mdl-acc-000.
       opn-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mdl-acc-000.
       cls-mdl-acc-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Funzione Open  per modulo              "maucmf" *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "maucmf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
           if        j-rsc                not  = spaces
                     go to rou-cls-fls-999.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "maucmf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/maucmf"                         .
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
       vis-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per campi chiave                  *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
       pmt-key-reg-999.
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
           move      "[Exit] per fine visualizzazione :"
                                          to   v-not                  .
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
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
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
      *              * La testata e' composta di nr. 5 pagine          *
      *              *-------------------------------------------------*
           move      5                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione si/no pagina w-cnt-sts-imp-npt da trattare *
      *    *-----------------------------------------------------------*
       snp-tes-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-snp      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     snp-tes-reg-100
                     snp-tes-reg-200
                     snp-tes-reg-300
                     snp-tes-reg-400
                     snp-tes-reg-500
                     depending            on   w-cnt-sts-imp-npt      .
           go to     snp-tes-reg-999.
       snp-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Test per pagina 1                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Test per pagina 2                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Test per pagina 3                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Test per pagina 4                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     snp-tes-reg-999.
       snp-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Test per pagina 5                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre attiva                               *
      *                  *---------------------------------------------*
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
                     acc-tes-reg-400
                     acc-tes-reg-500
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
      *                  * Si/no password azienda                      *
      *                  *---------------------------------------------*
           perform   acc-pwd-azi-000      thru acc-pwd-azi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-105.
      *                  *---------------------------------------------*
      *                  * Si/no password utente                       *
      *                  *---------------------------------------------*
           perform   acc-pwd-ute-000      thru acc-pwd-ute-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-100.
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Si/no password comando                      *
      *                  *---------------------------------------------*
           perform   acc-pwd-cmd-000      thru acc-pwd-cmd-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Si/no password stampante                    *
      *                  *---------------------------------------------*
           perform   acc-pwd-stp-000      thru acc-pwd-stp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza comando                  *
      *                  *---------------------------------------------*
           perform   acc-rsv-ide-000      thru acc-rsv-ide-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza tipo utente              *
      *                  *---------------------------------------------*
           perform   acc-rsv-tut-000      thru acc-rsv-tut-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza livello protezione       *
      *                  *---------------------------------------------*
           perform   acc-rsv-lip-000      thru acc-rsv-lip-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
       acc-tes-reg-135.
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza log utente               *
      *                  *---------------------------------------------*
           perform   acc-rsv-usl-000      thru acc-rsv-usl-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Si/no codice azienda                        *
      *                  *---------------------------------------------*
           perform   acc-snx-azi-000      thru acc-snx-azi-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-135.
       acc-tes-reg-142.
      *                  *---------------------------------------------*
      *                  * Si/no box grafici                           *
      *                  *---------------------------------------------*
           perform   acc-snx-box-000      thru acc-snx-box-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Ad accettazione ora solare / legale         *
      *                  *---------------------------------------------*
           go to     acc-tes-reg-150.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * System Clock Gap - giorni                   *
      *                  *---------------------------------------------*
           perform   acc-scg-cdt-000      thru acc-scg-cdt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-142.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * System Clock Gap - ora solare o legale      *
      *                  *---------------------------------------------*
           perform   acc-scg-chh-000      thru acc-scg-chh-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
       acc-tes-reg-155.
      *                  *---------------------------------------------*
      *                  * Tipo visualizzazione stampe                 *
      *                  *---------------------------------------------*
           perform   acc-vis-prf-000      thru acc-vis-prf-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Spooler di stampa                           *
      *                  *---------------------------------------------*
           perform   acc-tem-spl-000      thru acc-tem-spl-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-155.
       acc-tes-reg-165.
      *                  *---------------------------------------------*
      *                  * Blocco stampa                               *
      *                  *---------------------------------------------*
           perform   acc-tem-bss-000      thru acc-tem-bss-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-160.
       acc-tes-reg-180.
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
                     go to acc-tes-reg-165.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina 2                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice amministratore principale            *
      *                  *---------------------------------------------*
           perform   acc-ute-amm-000      thru acc-ute-amm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-205.
      *                  *---------------------------------------------*
      *                  * Codice amministratore alternativo           *
      *                  *---------------------------------------------*
           perform   acc-alt-amm-000      thru acc-alt-amm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-200.
       acc-tes-reg-210.
      *                  *---------------------------------------------*
      *                  * Durata password                             *
      *                  *---------------------------------------------*
           perform   acc-ngg-val-000      thru acc-ngg-val-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-205.
       acc-tes-reg-215.
      *                  *---------------------------------------------*
      *                  * Azione password scaduta                     *
      *                  *---------------------------------------------*
           perform   acc-tip-asp-000      thru acc-tip-asp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-210.
       acc-tes-reg-220.
      *                  *---------------------------------------------*
      *                  * Numero massimo tentativi                    *
      *                  *---------------------------------------------*
           perform   acc-max-err-000      thru acc-max-err-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-215.
       acc-tes-reg-225.
      *                  *---------------------------------------------*
      *                  * Azione password errata                      *
      *                  *---------------------------------------------*
           perform   acc-tip-ast-000      thru acc-tip-ast-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-220.
       acc-tes-reg-280.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 2                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-225.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Mail server                                 *
      *                  *---------------------------------------------*
           perform   acc-msa-isp-000      thru acc-msa-isp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-305.
      *                  *---------------------------------------------*
      *                  * Indirizzo Postmaster                        *
      *                  *---------------------------------------------*
           perform   acc-eml-psm-000      thru acc-eml-psm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-300.
       acc-tes-reg-310.
      *                  *---------------------------------------------*
      *                  * Utente Postmaster                           *
      *                  *---------------------------------------------*
           perform   acc-ute-psm-000      thru acc-ute-psm-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-305.
       acc-tes-reg-315.
      *                  *---------------------------------------------*
      *                  * Codice per account SMTP                     *
      *                  *---------------------------------------------*
           perform   acc-uid-smp-000      thru acc-uid-smp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-310.
       acc-tes-reg-320.
      *                  *---------------------------------------------*
      *                  * Password per account SMTP                   *
      *                  *---------------------------------------------*
           perform   acc-pwd-smp-000      thru acc-pwd-smp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-315.
       acc-tes-reg-325.
      *                  *---------------------------------------------*
      *                  * Agente di spedizione SMTP                   *
      *                  *---------------------------------------------*
           perform   acc-asd-smp-000      thru acc-asd-smp-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-320.
       acc-tes-reg-380.
      *                  *---------------------------------------------*
      *                  * Presa visione per pagina 3                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vpg-000      thru acc-pre-vpg-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-325.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina 4                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Mail server PEC                             *
      *                  *---------------------------------------------*
           perform   acc-srv-pec-000      thru acc-srv-pec-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-405.
      *                  *---------------------------------------------*
      *                  * User ID PEC                                 *
      *                  *---------------------------------------------*
           perform   acc-uid-pec-000      thru acc-uid-pec-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-400.
       acc-tes-reg-410.
      *                  *---------------------------------------------*
      *                  * Password PEC                                *
      *                  *---------------------------------------------*
           perform   acc-pwd-pec-000      thru acc-pwd-pec-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-405.
       acc-tes-reg-415.
      *                  *---------------------------------------------*
      *                  * Agente di spedizione PEC                    *
      *                  *---------------------------------------------*
           perform   acc-asd-pec-000      thru acc-asd-pec-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-410.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina 5                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Password MySQL                              *
      *                  *---------------------------------------------*
           perform   acc-pwd-sql-000      thru acc-pwd-sql-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-500.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
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
                     vis-tes-reg-400
                     vis-tes-reg-500
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no password azienda                      *
      *                  *---------------------------------------------*
           perform   vis-pwd-azi-000      thru vis-pwd-azi-999        .
      *                  *---------------------------------------------*
      *                  * Si/no password utente                       *
      *                  *---------------------------------------------*
           perform   vis-pwd-ute-000      thru vis-pwd-ute-999        .
      *                  *---------------------------------------------*
      *                  * Si/no password comando                      *
      *                  *---------------------------------------------*
           perform   vis-pwd-cmd-000      thru vis-pwd-cmd-999        .
      *                  *---------------------------------------------*
      *                  * Si/no password stampante                    *
      *                  *---------------------------------------------*
           perform   vis-pwd-stp-000      thru vis-pwd-stp-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza comando                  *
      *                  *---------------------------------------------*
           perform   vis-rsv-ide-000      thru vis-rsv-ide-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza tipo utente              *
      *                  *---------------------------------------------*
           perform   vis-rsv-tut-000      thru vis-rsv-tut-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza livello protezione       *
      *                  *---------------------------------------------*
           perform   vis-rsv-lip-000      thru vis-rsv-lip-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza log utente               *
      *                  *---------------------------------------------*
           perform   vis-rsv-usl-000      thru vis-rsv-usl-999        .
      *                  *---------------------------------------------*
      *                  * Si/no codice azienda                        *
      *                  *---------------------------------------------*
           perform   vis-snx-azi-000      thru vis-snx-azi-999        .
      *                  *---------------------------------------------*
      *                  * Si/no box grafici                           *
      *                  *---------------------------------------------*
           perform   vis-snx-box-000      thru vis-snx-box-999        .
      *                  *---------------------------------------------*
      *                  * System Clock Gap - giorni                   *
      *                  *---------------------------------------------*
           perform   vis-scg-cdt-000      thru vis-scg-cdt-999        .
      *                  *---------------------------------------------*
      *                  * System Clock Gap - ore                      *
      *                  *---------------------------------------------*
           perform   vis-scg-chh-000      thru vis-scg-chh-999        .
      *                  *---------------------------------------------*
      *                  * Tipo visualizzazione stampe                 *
      *                  *---------------------------------------------*
           perform   vis-vis-prf-000      thru vis-vis-prf-999        .
      *                  *---------------------------------------------*
      *                  * Spooler di stampa                           *
      *                  *---------------------------------------------*
           perform   vis-tem-spl-000      thru vis-tem-spl-999        .
      *                  *---------------------------------------------*
      *                  * Blocco stampa                               *
      *                  *---------------------------------------------*
           perform   vis-tem-bss-000      thru vis-tem-bss-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina 2                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice amministratore principale            *
      *                  *---------------------------------------------*
           perform   vis-ute-amm-000      thru vis-ute-amm-999        .
      *                  *---------------------------------------------*
      *                  * Codice amministratore alternativo           *
      *                  *---------------------------------------------*
           perform   vis-alt-amm-000      thru vis-alt-amm-999        .
      *                  *---------------------------------------------*
      *                  * Durata password                             *
      *                  *---------------------------------------------*
           perform   vis-ngg-val-000      thru vis-ngg-val-999        .
      *                  *---------------------------------------------*
      *                  * Azione password scaduta                     *
      *                  *---------------------------------------------*
           perform   vis-tip-asp-000      thru vis-tip-asp-999        .
      *                  *---------------------------------------------*
      *                  * Numero massimo tentativi                    *
      *                  *---------------------------------------------*
           perform   vis-max-err-000      thru vis-max-err-999        .
      *                  *---------------------------------------------*
      *                  * Azione password errata                      *
      *                  *---------------------------------------------*
           perform   vis-tip-ast-000      thru vis-tip-ast-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Mail server                                 *
      *                  *---------------------------------------------*
           perform   vis-msa-isp-000      thru vis-msa-isp-999        .
      *                  *---------------------------------------------*
      *                  * Indirizzo Postmaster                        *
      *                  *---------------------------------------------*
           perform   vis-eml-psm-000      thru vis-eml-psm-999        .
      *                  *---------------------------------------------*
      *                  * Codice Postmaster                           *
      *                  *---------------------------------------------*
           perform   vis-ute-psm-000      thru vis-ute-psm-999        .
      *                  *---------------------------------------------*
      *                  * Codice per account SMTP                     *
      *                  *---------------------------------------------*
           perform   vis-uid-smp-000      thru vis-uid-smp-999        .
      *                  *---------------------------------------------*
      *                  * Password per account SMTP                   *
      *                  *---------------------------------------------*
           perform   vis-pwd-smp-000      thru vis-pwd-smp-999        .
      *                  *---------------------------------------------*
      *                  * Password per documenti PDF                  *
      *                  *---------------------------------------------*
           perform   vis-pdf-pde-000      thru vis-pdf-pde-999        .
      *                  *---------------------------------------------*
      *                  * Agente di spedizione SMTP                   *
      *                  *---------------------------------------------*
           perform   vis-asd-smp-000      thru vis-asd-smp-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina 4                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Mail server PEC                             *
      *                  *---------------------------------------------*
           perform   vis-srv-pec-000      thru vis-srv-pec-999        .
      *                  *---------------------------------------------*
      *                  * User ID PEC                                 *
      *                  *---------------------------------------------*
           perform   vis-uid-pec-000      thru vis-uid-pec-999        .
      *                  *---------------------------------------------*
      *                  * Password PEC                                *
      *                  *---------------------------------------------*
           perform   vis-pwd-pec-000      thru vis-pwd-pec-999        .
      *                  *---------------------------------------------*
      *                  * Agente di spedizione PEC                    *
      *                  *---------------------------------------------*
           perform   vis-asd-pec-000      thru vis-asd-pec-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     vis-tes-reg-999.
       vis-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina 5                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Password MySQL                              *
      *                  *---------------------------------------------*
           perform   vis-pwd-sql-000      thru vis-pwd-sql-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero pagina        *
      *              *-------------------------------------------------*
           go to     pmt-tes-reg-100
                     pmt-tes-reg-200
                     pmt-tes-reg-300
                     pmt-tes-reg-400
                     pmt-tes-reg-500
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Pagina 1                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fincatura iniziale                          *
      *                  *---------------------------------------------*
           perform   pmt-fin-ini-000      thru pmt-fin-ini-999        .
      *                  *---------------------------------------------*
      *                  * Si/no password azienda                      *
      *                  *---------------------------------------------*
           perform   pmt-pwd-azi-000      thru pmt-pwd-azi-999        .
      *                  *---------------------------------------------*
      *                  * Si/no password utente                       *
      *                  *---------------------------------------------*
           perform   pmt-pwd-ute-000      thru pmt-pwd-ute-999        .
      *                  *---------------------------------------------*
      *                  * Si/no password comando                      *
      *                  *---------------------------------------------*
           perform   pmt-pwd-cmd-000      thru pmt-pwd-cmd-999        .
      *                  *---------------------------------------------*
      *                  * Si/no password stampante                    *
      *                  *---------------------------------------------*
           perform   pmt-pwd-stp-000      thru pmt-pwd-stp-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza comando                  *
      *                  *---------------------------------------------*
           perform   pmt-rsv-ide-000      thru pmt-rsv-ide-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza tipo utente              *
      *                  *---------------------------------------------*
           perform   pmt-rsv-tut-000      thru pmt-rsv-tut-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza livello protezione       *
      *                  *---------------------------------------------*
           perform   pmt-rsv-lip-000      thru pmt-rsv-lip-999        .
      *                  *---------------------------------------------*
      *                  * Si/no riservatezza log utente               *
      *                  *---------------------------------------------*
           perform   pmt-rsv-usl-000      thru pmt-rsv-usl-999        .
      *                  *---------------------------------------------*
      *                  * Fincatura centrale                          *
      *                  *---------------------------------------------*
           perform   pmt-fin-cen-000      thru pmt-fin-cen-999        .
      *                  *---------------------------------------------*
      *                  * Si/no codice azienda                        *
      *                  *---------------------------------------------*
           perform   pmt-snx-azi-000      thru pmt-snx-azi-999        .
      *                  *---------------------------------------------*
      *                  * Si/no box grafici                           *
      *                  *---------------------------------------------*
           perform   pmt-snx-box-000      thru pmt-snx-box-999        .
      *                  *---------------------------------------------*
      *                  * System Clock Gap - giorni                   *
      *                  *---------------------------------------------*
           perform   pmt-scg-cdt-000      thru pmt-scg-cdt-999        .
      *                  *---------------------------------------------*
      *                  * Tipo visualizzazione stampe                 *
      *                  *---------------------------------------------*
           perform   pmt-vis-prf-000      thru pmt-vis-prf-999        .
      *                  *---------------------------------------------*
      *                  * Spooler di stampa                           *
      *                  *---------------------------------------------*
           perform   pmt-tem-spl-000      thru pmt-tem-spl-999        .
      *                  *---------------------------------------------*
      *                  * Blocco stampa                               *
      *                  *---------------------------------------------*
           perform   pmt-tem-bss-000      thru pmt-tem-bss-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Pagina 2                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fincatura iniziale                          *
      *                  *---------------------------------------------*
           perform   pmt-fin-in2-000      thru pmt-fin-in2-999        .
      *                  *---------------------------------------------*
      *                  * Codice amministratore principale            *
      *                  *---------------------------------------------*
           perform   pmt-ute-amm-000      thru pmt-ute-amm-999        .
      *                  *---------------------------------------------*
      *                  * Codice amministratore alternativo           *
      *                  *---------------------------------------------*
           perform   pmt-alt-amm-000      thru pmt-alt-amm-999        .
      *                  *---------------------------------------------*
      *                  * Durata password                             *
      *                  *---------------------------------------------*
           perform   pmt-ngg-val-000      thru pmt-ngg-val-999        .
      *                  *---------------------------------------------*
      *                  * Azione password scaduta                     *
      *                  *---------------------------------------------*
           perform   pmt-tip-asp-000      thru pmt-tip-asp-999        .
      *                  *---------------------------------------------*
      *                  * Numero massimo tentativi                    *
      *                  *---------------------------------------------*
           perform   pmt-max-err-000      thru pmt-max-err-999        .
      *                  *---------------------------------------------*
      *                  * Azione password errata                      *
      *                  *---------------------------------------------*
           perform   pmt-tip-ast-000      thru pmt-tip-ast-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-300.
      *              *-------------------------------------------------*
      *              * Pagina 3                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fincatura iniziale                          *
      *                  *---------------------------------------------*
           perform   pmt-fin-in3-000      thru pmt-fin-in3-999        .
      *                  *---------------------------------------------*
      *                  * Mail server                                 *
      *                  *---------------------------------------------*
           perform   pmt-msa-isp-000      thru pmt-msa-isp-999        .
      *                  *---------------------------------------------*
      *                  * Indirizzo Postmaster                        *
      *                  *---------------------------------------------*
           perform   pmt-eml-psm-000      thru pmt-eml-psm-999        .
      *                  *---------------------------------------------*
      *                  * Codice Postmaster                           *
      *                  *---------------------------------------------*
           perform   pmt-ute-psm-000      thru pmt-ute-psm-999        .
      *                  *---------------------------------------------*
      *                  * Codice per account SMTP                     *
      *                  *---------------------------------------------*
           perform   pmt-uid-smp-000      thru pmt-uid-smp-999        .
      *                  *---------------------------------------------*
      *                  * Password per account SMTP                   *
      *                  *---------------------------------------------*
           perform   pmt-pwd-smp-000      thru pmt-pwd-smp-999        .
      *                  *---------------------------------------------*
      *                  * Password per documenti PDF                  *
      *                  *---------------------------------------------*
           perform   pmt-pdf-pde-000      thru pmt-pdf-pde-999        .
      *                  *---------------------------------------------*
      *                  * Agente di spedizione SMTP                   *
      *                  *---------------------------------------------*
           perform   pmt-asd-smp-000      thru pmt-asd-smp-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-400.
      *              *-------------------------------------------------*
      *              * Pagina 4                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fincatura iniziale                          *
      *                  *---------------------------------------------*
           perform   pmt-fin-in4-000      thru pmt-fin-in4-999        .
      *                  *---------------------------------------------*
      *                  * Mail server PEC                             *
      *                  *---------------------------------------------*
           perform   pmt-srv-pec-000      thru pmt-srv-pec-999        .
      *                  *---------------------------------------------*
      *                  * User ID PEC                                 *
      *                  *---------------------------------------------*
           perform   pmt-uid-pec-000      thru pmt-uid-pec-999        .
      *                  *---------------------------------------------*
      *                  * Password PEC                                *
      *                  *---------------------------------------------*
           perform   pmt-pwd-pec-000      thru pmt-pwd-pec-999        .
      *                  *---------------------------------------------*
      *                  * Agente di spedizione PEC                    *
      *                  *---------------------------------------------*
           perform   pmt-asd-pec-000      thru pmt-asd-pec-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-500.
      *              *-------------------------------------------------*
      *              * Pagina 5                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fincatura iniziale                          *
      *                  *---------------------------------------------*
           perform   pmt-fin-in5-000      thru pmt-fin-in5-999        .
      *                  *---------------------------------------------*
      *                  * Password MySQL                              *
      *                  *---------------------------------------------*
           perform   pmt-pwd-sql-000      thru pmt-pwd-sql-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Fincatura iniziale               *
      *    *-----------------------------------------------------------*
       pmt-fin-ini-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      " Sicurezza  "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      "------------"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "      Controllo passwords     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "    Controllo riservatezza    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fin-ini-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no password azienda           *
      *    *-----------------------------------------------------------*
       pmt-pwd-azi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "- Su codici azienda ........ :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-azi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no password utente            *
      *    *-----------------------------------------------------------*
       pmt-pwd-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "- Su codici utente ......... :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no password comando           *
      *    *-----------------------------------------------------------*
       pmt-pwd-cmd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "- Su codici comando ........ :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-cmd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no password stampante         *
      *    *-----------------------------------------------------------*
       pmt-pwd-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "- Su codici stampante ...... :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no riservatezza comando       *
      *    *-----------------------------------------------------------*
       pmt-rsv-ide-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "- Su gerarchie comandi ..... :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rsv-ide-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no riservatezza tipo utente   *
      *    *-----------------------------------------------------------*
       pmt-rsv-tut-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "- Su tipo utente ........... :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rsv-tut-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no riservatezza livello prot. *
      *    *-----------------------------------------------------------*
       pmt-rsv-lip-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "- Su livello di protezione . :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rsv-lip-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no riservatezza log utente    *
      *    *-----------------------------------------------------------*
       pmt-rsv-usl-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "- Log di attivita' utente ...:  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-rsv-usl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Fincatura centrale               *
      *    *-----------------------------------------------------------*
       pmt-fin-cen-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      " Parametri generali "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      "--------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fin-cen-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no codice azienda             *
      *    *-----------------------------------------------------------*
       pmt-snx-azi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice azienda    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-azi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Si/no box grafici                *
      *    *-----------------------------------------------------------*
       pmt-snx-box-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Box grafici :"      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-snx-box-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : System Clock - giorni            *
      *    *-----------------------------------------------------------*
       pmt-scg-cdt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "System clock (+/-): gg.       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-scg-cdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo visualizzazione stampe      *
      *    *-----------------------------------------------------------*
       pmt-vis-prf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Archivi stampa    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-vis-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Spooler di stampa                *
      *    *-----------------------------------------------------------*
       pmt-tem-spl-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Spooler stampa    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tem-spl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Blocco stampa                    *
      *    *-----------------------------------------------------------*
       pmt-tem-bss-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Blocco spooler    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tem-bss-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Fincatura iniziale 2             *
      *    *-----------------------------------------------------------*
       pmt-fin-in2-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "Parametri per la gestione delle password utente "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fin-in2-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice amministratore            *
      *    *-----------------------------------------------------------*
       pmt-ute-amm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Amministratori del sistema :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ute-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Amministratore alternativo       *
      *    *-----------------------------------------------------------*
       pmt-alt-amm-000.
       pmt-alt-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Durata password                  *
      *    *-----------------------------------------------------------*
       pmt-ngg-val-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Durata massima in giorni   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     per password utente    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ngg-val-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Azione password scaduta          *
      *    *-----------------------------------------------------------*
       pmt-tip-asp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Azione da intraprendere    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "allo scadere della password "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-asp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Numero massimo tentativi         *
      *    *-----------------------------------------------------------*
       pmt-max-err-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero massimo tentativi   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       errati di accesso    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-max-err-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Azione password errata           *
      *    *-----------------------------------------------------------*
       pmt-tip-ast-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Azione da intraprendere    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "   all'ultimo tentativo     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ast-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Fincatura iniziale 3             *
      *    *-----------------------------------------------------------*
       pmt-fin-in3-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      49                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      "Parametri per la gestione della posta elettronica"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      49                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fin-in3-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Mail server                      *
      *    *-----------------------------------------------------------*
       pmt-msa-isp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mail server 'SMTP'         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-msa-isp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Indirizzo Postmaster             *
      *    *-----------------------------------------------------------*
       pmt-eml-psm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Indirizzo 'Postmaster'     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-eml-psm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice Postmaster                *
      *    *-----------------------------------------------------------*
       pmt-ute-psm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Utente    'Postmaster'     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ute-psm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice per server 'SMTP'         *
      *    *-----------------------------------------------------------*
       pmt-uid-smp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "User ID account 'SMTP'     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-uid-smp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Password per server 'SMTP'       *
      *    *-----------------------------------------------------------*
       pmt-pwd-smp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Password account 'SMTP'    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-smp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Password per documenti PDF       *
      *    *-----------------------------------------------------------*
       pmt-pdf-pde-000.
           go to pmt-pdf-pde-999.
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Password per documenti     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pdf-pde-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Agente di spedizione SMTP        *
      *    *-----------------------------------------------------------*
       pmt-asd-smp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di account 'SMTP'     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-asd-smp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Fincatura iniziale 4             *
      *    *-----------------------------------------------------------*
       pmt-fin-in4-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      Parametri per la gestione della posta elettr
      -              "onica certificata (PEC)       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      --------------------------------------------
      -              "-----------------------       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fin-in4-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Mail server PEC                  *
      *    *-----------------------------------------------------------*
       pmt-srv-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Mail server 'PEC'          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-srv-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : User ID PEC                      *
      *    *-----------------------------------------------------------*
       pmt-uid-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "User ID account  'PEC'     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-uid-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Password PEC                     *
      *    *-----------------------------------------------------------*
       pmt-pwd-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Password account 'PEC'     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Agente di spedizione PEC         *
      *    *-----------------------------------------------------------*
       pmt-asd-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di account  'PEC'     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-asd-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Fincatura iniziale 5             *
      *    *-----------------------------------------------------------*
       pmt-fin-in5-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                  Parametri per la gestione del Da
      -              "tabase MySQL                  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                  --------------------------------
      -              "------------                  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fin-in5-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Password MySQL                   *
      *    *-----------------------------------------------------------*
       pmt-pwd-sql-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Password server MySQL      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-pwd-sql-999.
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
      *    * Accettazione campo : Si/no password azienda               *
      *    *-----------------------------------------------------------*
       acc-pwd-azi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pwd-azi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-azi        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-azi        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-azi-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-azi-999.
       acc-pwd-azi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-pwd-azi
           else if   v-num                =    02
                     move  "S"            to   w-psg-pwd-azi
           else      move  spaces         to   w-psg-pwd-azi          .
       acc-pwd-azi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pwd-azi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pwd-azi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pwd-azi-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pwd-azi-100.
       acc-pwd-azi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no password azienda            *
      *    *-----------------------------------------------------------*
       vis-pwd-azi-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-azi        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-azi        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-azi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no password utente                *
      *    *-----------------------------------------------------------*
       acc-pwd-ute-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pwd-ute-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-ute        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-ute        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-ute-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-ute-999.
       acc-pwd-ute-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-pwd-ute
           else if   v-num                =    02
                     move  "S"            to   w-psg-pwd-ute
           else      move  spaces         to   w-psg-pwd-ute          .
       acc-pwd-ute-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pwd-ute-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pwd-ute-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pwd-ute-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pwd-ute-100.
       acc-pwd-ute-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no password utente             *
      *    *-----------------------------------------------------------*
       vis-pwd-ute-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-ute        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-ute        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-ute-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no password comando               *
      *    *-----------------------------------------------------------*
       acc-pwd-cmd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pwd-cmd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-cmd        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-cmd        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-cmd-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-cmd-999.
       acc-pwd-cmd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-pwd-cmd
           else if   v-num                =    02
                     move  "S"            to   w-psg-pwd-cmd
           else      move  spaces         to   w-psg-pwd-cmd          .
       acc-pwd-cmd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pwd-cmd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pwd-cmd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pwd-cmd-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pwd-cmd-100.
       acc-pwd-cmd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no password comando            *
      *    *-----------------------------------------------------------*
       vis-pwd-cmd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-cmd        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-cmd        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-cmd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no password stampante             *
      *    *-----------------------------------------------------------*
       acc-pwd-stp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pwd-stp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-stp        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-stp        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-stp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-stp-999.
       acc-pwd-stp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-pwd-stp
           else if   v-num                =    02
                     move  "S"            to   w-psg-pwd-stp
           else      move  spaces         to   w-psg-pwd-stp          .
       acc-pwd-stp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pwd-stp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pwd-stp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pwd-stp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pwd-stp-100.
       acc-pwd-stp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no password stampante          *
      *    *-----------------------------------------------------------*
       vis-pwd-stp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-pwd-stp        =    "N"
                     move  01             to   v-num
           else if   w-psg-pwd-stp        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-stp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no riservatezza comando           *
      *    *-----------------------------------------------------------*
       acc-rsv-ide-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rsv-ide-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-ide        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-ide        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-ide-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-ide-999.
       acc-rsv-ide-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-rsv-ide
           else if   v-num                =    02
                     move  "S"            to   w-psg-rsv-ide
           else      move  spaces         to   w-psg-rsv-ide          .
       acc-rsv-ide-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rsv-ide-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rsv-ide-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-rsv-ide-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-rsv-ide-100.
       acc-rsv-ide-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no riservatezza comando        *
      *    *-----------------------------------------------------------*
       vis-rsv-ide-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      09                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-ide        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-ide        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rsv-ide-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no riservatezza tipo utente       *
      *    *-----------------------------------------------------------*
       acc-rsv-tut-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rsv-tut-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-tut        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-tut        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-tut-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-tut-999.
       acc-rsv-tut-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-rsv-tut
           else if   v-num                =    02
                     move  "S"            to   w-psg-rsv-tut
           else      move  spaces         to   w-psg-rsv-tut          .
       acc-rsv-tut-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rsv-tut-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rsv-tut-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-rsv-tut-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-rsv-tut-100.
       acc-rsv-tut-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no riservatezza tipo utente    *
      *    *-----------------------------------------------------------*
       vis-rsv-tut-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-tut        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-tut        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rsv-tut-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no riservatezza livello protez.   *
      *    *-----------------------------------------------------------*
       acc-rsv-lip-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rsv-lip-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-lip        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-lip        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-lip-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-lip-999.
       acc-rsv-lip-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-rsv-lip
           else if   v-num                =    02
                     move  "S"            to   w-psg-rsv-lip
           else      move  spaces         to   w-psg-rsv-lip          .
       acc-rsv-lip-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rsv-lip-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rsv-lip-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-rsv-lip-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-rsv-lip-100.
       acc-rsv-lip-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no riservatezza livello protez.*
      *    *-----------------------------------------------------------*
       vis-rsv-lip-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      11                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-lip        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-lip        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rsv-lip-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no riservatezza log utente        *
      *    *-----------------------------------------------------------*
       acc-rsv-usl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-rsv-usl-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Personalizzabile per ciascun utente in gestione sc
      -              "heda"               to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-usl        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-usl        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-usl-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-rsv-usl-999.
       acc-rsv-usl-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-rsv-usl
           else if   v-num                =    02
                     move  "S"            to   w-psg-rsv-usl
           else      move  spaces         to   w-psg-rsv-usl          .
       acc-rsv-usl-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-rsv-usl-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-rsv-usl-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-rsv-usl-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-rsv-usl-100.
       acc-rsv-usl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no riservatezza log utente     *
      *    *-----------------------------------------------------------*
       vis-rsv-usl-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-rsv-usl        =    "N"
                     move  01             to   v-num
           else if   w-psg-rsv-usl        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rsv-usl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no codice azienda                 *
      *    *-----------------------------------------------------------*
       acc-snx-azi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-azi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-azi-lun    to   v-car                  .
           move      w-exp-snx-azi-num    to   v-ldt                  .
           move      "VN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-snx-azi-tbl    to   v-txt                  .
      *
           if        w-psg-snx-azi        =    "S"
                     move  01             to   v-num
           else if   w-psg-snx-azi        =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-azi-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-azi-999.
       acc-snx-azi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   w-psg-snx-azi
           else if   v-num                =    02
                     move  "N"            to   w-psg-snx-azi
           else      move  spaces         to   w-psg-snx-azi          .
       acc-snx-azi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-azi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-azi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-azi-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-azi-100.
       acc-snx-azi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no codice azienda              *
      *    *-----------------------------------------------------------*
       vis-snx-azi-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-azi-lun    to   v-car                  .
           move      w-exp-snx-azi-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-exp-snx-azi-tbl    to   v-txt                  .
      *
           if        w-psg-snx-azi        =    "S"
                     move  01             to   v-num
           else if   w-psg-snx-azi        =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-azi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Si/no box grafici                    *
      *    *-----------------------------------------------------------*
       acc-snx-box-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-snx-box-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      "NS#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-snx-box        =    "N"
                     move  01             to   v-num
           else if   w-psg-snx-box        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-snx-box-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-snx-box-999.
       acc-snx-box-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "N"            to   w-psg-snx-box
           else if   v-num                =    02
                     move  "S"            to   w-psg-snx-box
           else      move  spaces         to   w-psg-snx-box          .
       acc-snx-box-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-snx-box-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-snx-box-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-snx-box-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-snx-box-100.
       acc-snx-box-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Si/no box grafici                 *
      *    *-----------------------------------------------------------*
       vis-snx-box-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sce-nsx-lun    to   v-car                  .
           move      w-exp-sce-nsx-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      w-exp-sce-nsx-tbl    to   v-txt                  .
      *
           if        w-psg-snx-box        =    "N"
                     move  01             to   v-num
           else if   w-psg-snx-box        =    "S"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-snx-box-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : System Clock Gap - giorni            *
      *    *-----------------------------------------------------------*
       acc-scg-cdt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-scg-cdt-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "ATTENZIONE: non modificare questo parametro di sis
      -              "tema!"              to   v-nt1                  .
           move      "(Il Gap scatta sempre dopo le ore 2:00 del mattino
      -              ")    "              to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-scg-cdt        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-scg-cdt-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-scg-cdt-999.
       acc-scg-cdt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-psg-scg-cdt          .
       acc-scg-cdt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-scg-cdt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-scg-cdt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-scg-cdt-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-scg-cdt-100.
       acc-scg-cdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : System Clock Gap - giorni         *
      *    *-----------------------------------------------------------*
       vis-scg-cdt-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-psg-scg-cdt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-scg-cdt-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : System Clock Gap - ore               *
      *    *-----------------------------------------------------------*
       acc-scg-chh-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-scg-chh-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ora-sol-lun    to   v-car                  .
           move      w-exp-ora-sol-num    to   v-ldt                  .
           move      "SL#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "UP"                 to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-ora-sol-tbl    to   v-txt                  .
      *
           if        w-psg-scg-chh        =    1
                     move  01             to   v-num
           else if   w-psg-scg-chh        =    2
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-scg-chh-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-scg-chh-999.
       acc-scg-chh-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  1              to   w-psg-scg-chh
           else if   v-num                =    02
                     move  2              to   w-psg-scg-chh
           else      move  spaces         to   w-psg-scg-chh          .
       acc-scg-chh-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-scg-chh-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-scg-chh-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-scg-chh-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-scg-chh-100.
       acc-scg-chh-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : System Clock Gap - ore            *
      *    *-----------------------------------------------------------*
       vis-scg-chh-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ora-sol-lun    to   v-car                  .
           move      w-exp-ora-sol-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      17                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      w-exp-ora-sol-tbl    to   v-txt                  .
      *
           if        w-psg-scg-chh        =    1
                     move  01             to   v-num
           else if   w-psg-scg-chh        =    2
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-scg-chh-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo visualizzazione stampe          *
      *    *-----------------------------------------------------------*
       acc-vis-prf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione campo di accettazione          *
      *                  *---------------------------------------------*
           move      w-psg-cnt-vpf        to   w-acc-vis-prf          .
      *
           if        w-acc-vis-prf-001    not  = "T" and
                     w-acc-vis-prf-001    not  = "I"
                     move  "I"            to   w-acc-vis-prf-001      .
           if        w-acc-vis-prf-002    not  = "S" and
                     w-acc-vis-prf-002    not  = "N"
                     move  "N"            to   w-acc-vis-prf-001      .
       acc-vis-prf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-vis-prf-lun    to   v-car                  .
           move      w-exp-vis-prf-num    to   v-ldt                  .
           move      "1234#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-vis-prf-tbl    to   v-txt                  .
      *
           if        w-acc-vis-prf-001    =    "I" and
                     w-acc-vis-prf-002    =    "N"
                     move  01             to   v-num
           else if   w-acc-vis-prf-001    =    "I" and
                     w-acc-vis-prf-002    =    "S"
                     move  02             to   v-num
           else if   w-acc-vis-prf-001    =    "T" and
                     w-acc-vis-prf-002    =    "N"
                     move  03             to   v-num
           else if   w-acc-vis-prf-001    =    "T" and
                     w-acc-vis-prf-002    =    "S"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-vis-prf-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-vis-prf-999.
       acc-vis-prf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "I"            to   w-acc-vis-prf-001
                     move  "N"            to   w-acc-vis-prf-002
           else if   v-num                =    02
                     move  "I"            to   w-acc-vis-prf-001
                     move  "S"            to   w-acc-vis-prf-002
           else if   v-num                =    03
                     move  "T"            to   w-acc-vis-prf-001
                     move  "N"            to   w-acc-vis-prf-002
           else if   v-num                =    04
                     move  "T"            to   w-acc-vis-prf-001
                     move  "S"            to   w-acc-vis-prf-002
           else      move  spaces         to   w-acc-vis-prf-001
                     move  spaces         to   w-acc-vis-prf-002      .
      *
           move      w-acc-vis-prf        to   w-psg-cnt-vpf          .
       acc-vis-prf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-vis-prf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-vis-prf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-vis-prf-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-vis-prf-100.
       acc-vis-prf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo visualizzazione stampe       *
      *    *-----------------------------------------------------------*
       vis-vis-prf-000.
      *              *-------------------------------------------------*
      *              * Pre-visualizzazione                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione campo di visualizzazione       *
      *                  *---------------------------------------------*
           move      w-psg-cnt-vpf        to   w-acc-vis-prf          .
      *
           if        w-acc-vis-prf-001    not  = "T" and
                     w-acc-vis-prf-001    not  = "I"
                     move  "I"            to   w-acc-vis-prf-001      .
           if        w-acc-vis-prf-002    not  = "S" and
                     w-acc-vis-prf-002    not  = "N"
                     move  "N"            to   w-acc-vis-prf-001      .
       vis-vis-prf-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-vis-prf-lun    to   v-car                  .
           move      w-exp-vis-prf-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-exp-vis-prf-tbl    to   v-txt                  .
      *
           if        w-acc-vis-prf-001    =    "I" and
                     w-acc-vis-prf-002    =    "N"
                     move  01             to   v-num
           else if   w-acc-vis-prf-001    =    "I" and
                     w-acc-vis-prf-002    =    "S"
                     move  02             to   v-num
           else if   w-acc-vis-prf-001    =    "T" and
                     w-acc-vis-prf-002    =    "N"
                     move  03             to   v-num
           else if   w-acc-vis-prf-001    =    "T" and
                     w-acc-vis-prf-002    =    "S"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-vis-prf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Spool di stampa                      *
      *    *-----------------------------------------------------------*
       acc-tem-spl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il sistema operativo ospite non am-  *
      *                      * mette la stampa in spool : uscita       *
      *                      *-----------------------------------------*
           if        w-mop-sys-snx-ssp    not  = spaces
                     go to acc-tem-spl-999.
       acc-tem-spl-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        w-psg-tem-spl        =    spaces
                     move  w-acc-tem-spl-nop
                                          to   v-alf
           else      move  w-psg-tem-spl  to   v-alf                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tem-spl-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tem-spl-999.
       acc-tem-spl-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-tem-spl          .
       acc-tem-spl-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-psg-tem-spl        =    "lpr"
                     move  w-acc-tem-spl-lpr
                                          to   w-psg-tem-spl
                     go to acc-tem-spl-600.
           if        w-psg-tem-spl        =    "lp "
                     move  w-acc-tem-spl-lpx
                                          to   w-psg-tem-spl
                     go to acc-tem-spl-600.
           if        w-psg-tem-spl        =    "aix"
                     move  w-acc-tem-spl-aix
                                          to   w-psg-tem-spl
                     go to acc-tem-spl-600.
           if        w-psg-tem-spl        =    "dos"
                     move  w-acc-tem-spl-dos
                                          to   w-psg-tem-spl
                     go to acc-tem-spl-600.
       acc-tem-spl-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tem-spl-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tem-spl-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tem-spl-100.
       acc-tem-spl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Spool di stampa                   *
      *    *-----------------------------------------------------------*
       vis-tem-spl-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-psg-tem-spl        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tem-spl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Blocco stampa                        *
      *    *-----------------------------------------------------------*
       acc-tem-bss-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il sistema operativo ospite non am-  *
      *                      * mette la stampa in spool : uscita       *
      *                      *-----------------------------------------*
           if        w-mop-sys-snx-ssp    not  = spaces
                     go to acc-tem-bss-999.
       acc-tem-bss-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
      *
           if        w-psg-tem-bss        =    spaces
                     move  w-acc-tem-bss-nop
                                          to   v-alf
           else      move  w-psg-tem-bss  to   v-alf                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tem-bss-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tem-bss-999.
       acc-tem-bss-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-tem-bss          .
       acc-tem-bss-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        w-psg-tem-bss        =    "lpr"
                     move  w-acc-tem-bss-lpr
                                          to   w-psg-tem-bss
                     go to acc-tem-bss-600.
           if        w-psg-tem-bss        =    "lp "
                     move  w-acc-tem-bss-lpx
                                          to   w-psg-tem-bss
                     go to acc-tem-bss-600.
           if        w-psg-tem-bss        =    "aix"
                     move  w-acc-tem-bss-aix
                                          to   w-psg-tem-bss
                     go to acc-tem-bss-600.
           if        w-psg-tem-bss        =    "dos"
                     move  w-acc-tem-bss-dos
                                          to   w-psg-tem-bss
                     go to acc-tem-bss-600.
       acc-tem-bss-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tem-bss-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tem-bss-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tem-bss-100.
       acc-tem-bss-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Blocco stampa                     *
      *    *-----------------------------------------------------------*
       vis-tem-bss-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-psg-tem-bss        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tem-bss-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Utente amministratore principale     *
      *    *-----------------------------------------------------------*
       acc-ute-amm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ute-amm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-ute-amm        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ute-amm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ute-amm-999.
       acc-ute-amm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-ute-amm          .
       acc-ute-amm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ute-amm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ute-amm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ute-amm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ute-amm-100.
       acc-ute-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Utente amministratore principale  *
      *    *-----------------------------------------------------------*
       vis-ute-amm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-ute-amm        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ute-amm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Utente amministratore alternativo    *
      *    *-----------------------------------------------------------*
       acc-alt-amm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-alt-amm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-alt-amm        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-alt-amm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-alt-amm-999.
       acc-alt-amm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-alt-amm          .
       acc-alt-amm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-alt-amm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-alt-amm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-alt-amm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-alt-amm-100.
       acc-alt-amm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Utente amministratore alternativo *
      *    *-----------------------------------------------------------*
       vis-alt-amm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-psg-alt-amm        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-alt-amm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Durata password                      *
      *    *-----------------------------------------------------------*
       acc-ngg-val-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ngg-val-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-ngg-val        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ngg-val-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ngg-val-999.
       acc-ngg-val-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-psg-ngg-val          .
       acc-ngg-val-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ngg-val-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione azione            *
      *                  *---------------------------------------------*
           if        w-psg-ngg-val        not  = zero
                     go to acc-ngg-val-800.
           if        w-psg-tip-asp        not  = 02
                     go to acc-ngg-val-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione azione                      *
      *                  *---------------------------------------------*
           move      01                   to   w-psg-tip-asp          .
           perform   vis-tip-asp-000      thru vis-tip-asp-999        .
       acc-ngg-val-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ngg-val-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ngg-val-100.
       acc-ngg-val-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Durata password                   *
      *    *-----------------------------------------------------------*
       vis-ngg-val-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-ngg-val        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ngg-val-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Azione password scaduta              *
      *    *-----------------------------------------------------------*
       acc-tip-asp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-asp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-asp-lun    to   v-car                  .
           move      w-exp-tip-asp-num    to   v-ldt                  .
           move      "NB#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-tip-asp-tbl    to   v-txt                  .
           move      w-psg-tip-asp        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-asp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-asp-999.
       acc-tip-asp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-psg-tip-asp          .
       acc-tip-asp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-asp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-asp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-asp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-asp-100.
       acc-tip-asp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Azione password scaduta           *
      *    *-----------------------------------------------------------*
       vis-tip-asp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-asp-lun    to   v-car                  .
           move      w-exp-tip-asp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-asp-tbl    to   v-txt                  .
           move      w-psg-tip-asp        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-asp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero massimo tentativi             *
      *    *-----------------------------------------------------------*
       acc-max-err-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-max-err-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-max-err        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-max-err-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-max-err-999.
       acc-max-err-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-psg-max-err          .
       acc-max-err-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-max-err-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione azione            *
      *                  *---------------------------------------------*
           if        w-psg-max-err        not  = zero
                     go to acc-max-err-800.
           if        w-psg-tip-ast        not  = 02
                     go to acc-max-err-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione azione                      *
      *                  *---------------------------------------------*
           move      01                   to   w-psg-tip-ast          .
           perform   vis-tip-ast-000      thru vis-tip-ast-999        .
       acc-max-err-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-max-err-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-max-err-100.
       acc-max-err-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero massimo tentativi          *
      *    *-----------------------------------------------------------*
       vis-max-err-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-max-err        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-max-err-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Azione password errata               *
      *    *-----------------------------------------------------------*
       acc-tip-ast-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ast-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ast-lun    to   v-car                  .
           move      w-exp-tip-ast-num    to   v-ldt                  .
           move      "NB#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-tip-ast-tbl    to   v-txt                  .
           move      w-psg-tip-ast        to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tip-ast-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tip-ast-999.
       acc-tip-ast-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-psg-tip-ast          .
       acc-tip-ast-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-ast-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ast-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-tip-ast-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-tip-ast-100.
       acc-tip-ast-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Azione password errata            *
      *    *-----------------------------------------------------------*
       vis-tip-ast-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ast-lun    to   v-car                  .
           move      w-exp-tip-ast-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-ast-tbl    to   v-txt                  .
           move      w-psg-tip-ast        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ast-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Mail server                          *
      *    *-----------------------------------------------------------*
       acc-msa-isp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-msa-isp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-msa-isp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-msa-isp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-msa-isp-999.
       acc-msa-isp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-msa-isp          .
       acc-msa-isp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-msa-isp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-msa-isp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-msa-isp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-msa-isp-100.
       acc-msa-isp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Mail server                       *
      *    *-----------------------------------------------------------*
       vis-msa-isp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-msa-isp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-msa-isp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Indirizzo Postmaster                 *
      *    *-----------------------------------------------------------*
       acc-eml-psm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-eml-psm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-eml-psm        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-eml-psm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-eml-psm-999.
       acc-eml-psm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-eml-psm          .
       acc-eml-psm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-eml-psm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-eml-psm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-eml-psm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-eml-psm-100.
       acc-eml-psm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Indirizzo Postmaster              *
      *    *-----------------------------------------------------------*
       vis-eml-psm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-eml-psm        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-eml-psm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Utente Postmaster                    *
      *    *-----------------------------------------------------------*
       acc-ute-psm-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-ute-psm-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-ute-psm        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-ute-psm-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-ute-psm-999.
       acc-ute-psm-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-ute-psm          .
       acc-ute-psm-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-ute-psm-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-ute-psm-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-ute-psm-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-ute-psm-100.
       acc-ute-psm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Utente Postmaster                 *
      *    *-----------------------------------------------------------*
       vis-ute-psm-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-ute-psm        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-ute-psm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice per account SMTP              *
      *    *-----------------------------------------------------------*
       acc-uid-smp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-uid-smp-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Solo se necessario per l'autenticazione in uscita"
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-uid-smp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-uid-smp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-uid-smp-999.
       acc-uid-smp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-uid-smp          .
       acc-uid-smp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-uid-smp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-uid-smp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-uid-smp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-uid-smp-100.
       acc-uid-smp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice per account SMTP           *
      *    *-----------------------------------------------------------*
       vis-uid-smp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-uid-smp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-uid-smp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Password per account SMTP            *
      *    *-----------------------------------------------------------*
       acc-pwd-smp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pwd-smp-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Solo se necessario per l'autenticazione in uscita"
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-pwd-smp        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-smp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-smp-999.
       acc-pwd-smp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-pwd-smp          .
       acc-pwd-smp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pwd-smp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
           perform   vis-pwd-smp-000      thru vis-pwd-smp-999        .
       acc-pwd-smp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pwd-smp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pwd-smp-100.
       acc-pwd-smp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Password per account SMTP         *
      *    *-----------------------------------------------------------*
       vis-pwd-smp-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione mascherata                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "W"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-pwd-smp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-smp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Password per documenti PDF           *
      *    *-----------------------------------------------------------*
       acc-pdf-pde-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campo attualmente non gestito               *
      *                  *---------------------------------------------*
           go to     acc-pdf-pde-999.
       acc-pdf-pde-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "E' necessario che sia installata l'apposita utilit
      -              "a' nel server gestionale"
                                          to   v-nt1                  .
           move      "per poter utilizzare la protezione tramite passwor
      -              "d!           "      to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-pdf-pde        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pdf-pde-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pdf-pde-999.
       acc-pdf-pde-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-pdf-pde          .
       acc-pdf-pde-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pdf-pde-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pdf-pde-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pdf-pde-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pdf-pde-100.
       acc-pdf-pde-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Password per documenti PDF        *
      *    *-----------------------------------------------------------*
       vis-pdf-pde-000.
      *              *-------------------------------------------------*
      *              * Campo attualmente non gestito                   *
      *              *-------------------------------------------------*
           go to     vis-pdf-pde-999.
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-pdf-pde        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pdf-pde-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Agente di spedizione SMTP            *
      *    *-----------------------------------------------------------*
       acc-asd-smp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-asd-smp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-asd-smp-lun    to   v-car                  .
           move      w-exp-asd-smp-num    to   v-ldt                  .
           move      "NMTS#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-asd-smp-tbl    to   v-txt                  .
      *
           if        w-psg-asd-smp        =    spaces or
                     w-psg-asd-smp        =    "t_sender            "
                     move  01             to   v-num
           else if   w-psg-asd-smp        =    "t_sender_aut        "
                     move  02             to   v-num
           else if   w-psg-asd-smp        =    "t_sender_tls        "
                     move  03             to   v-num
           else if   w-psg-asd-smp        =    "t_sender_ssl        "
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-asd-smp-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-asd-smp-999.
       acc-asd-smp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "t_sender            "
                                          to   w-psg-asd-smp
           else if   v-num                =    02
                     move  "t_sender_aut        "
                                          to   w-psg-asd-smp
           else if   v-num                =    03
                     move  "t_sender_tls        "
                                          to   w-psg-asd-smp
           else if   v-num                =    04
                     move  "t_sender_ssl        "
                                          to   w-psg-asd-smp
           else      move  spaces         to   w-psg-asd-smp          .
       acc-asd-smp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-asd-smp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-asd-smp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-asd-smp-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-asd-smp-100.
       acc-asd-smp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Agente di spedizione SMTP         *
      *    *-----------------------------------------------------------*
       vis-asd-smp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-asd-smp-lun    to   v-car                  .
           move      w-exp-asd-smp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-asd-smp-tbl    to   v-txt                  .
      *
           if        w-psg-asd-smp        =    spaces or
                     w-psg-asd-smp        =    "t_sender            "
                     move  01             to   v-num
           else if   w-psg-asd-smp        =    "t_sender_aut        "
                     move  02             to   v-num
           else if   w-psg-asd-smp        =    "t_sender_tls        "
                     move  03             to   v-num
           else if   w-psg-asd-smp        =    "t_sender_ssl        "
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-asd-smp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Server PEC                           *
      *    *-----------------------------------------------------------*
       acc-srv-pec-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-srv-pec-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-srv-pec        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-srv-pec-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-srv-pec-999.
       acc-srv-pec-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-srv-pec          .
       acc-srv-pec-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-srv-pec-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-srv-pec-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-srv-pec-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-srv-pec-100.
       acc-srv-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Server PEC                        *
      *    *-----------------------------------------------------------*
       vis-srv-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-srv-pec        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-srv-pec-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice per account PEC               *
      *    *-----------------------------------------------------------*
       acc-uid-pec-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-uid-pec-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Solo se necessario per l'autenticazione in uscita"
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-uid-pec        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-uid-pec-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-uid-pec-999.
       acc-uid-pec-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-uid-pec          .
       acc-uid-pec-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-uid-pec-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-uid-pec-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-uid-pec-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-uid-pec-100.
       acc-uid-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Codice per account PEC            *
      *    *-----------------------------------------------------------*
       vis-uid-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-uid-pec        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-uid-pec-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Password per account PEC             *
      *    *-----------------------------------------------------------*
       acc-pwd-pec-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pwd-pec-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Solo se necessario per l'autenticazione in uscita"
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-pwd-pec        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-pec-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-pec-999.
       acc-pwd-pec-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-pwd-pec          .
       acc-pwd-pec-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pwd-pec-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
           perform   vis-pwd-pec-000      thru vis-pwd-pec-999        .
       acc-pwd-pec-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pwd-pec-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pwd-pec-100.
       acc-pwd-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Password per account PEC          *
      *    *-----------------------------------------------------------*
       vis-pwd-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "W"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-pwd-pec        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-pec-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Agente di spedizione PEC             *
      *    *-----------------------------------------------------------*
       acc-asd-pec-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-asd-pec-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-asd-smp-lun    to   v-car                  .
           move      w-exp-asd-smp-num    to   v-ldt                  .
           move      "NMTS#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-exp-asd-smp-tbl    to   v-txt                  .
      *
           if        w-psg-asd-pec        =    spaces or
                     w-psg-asd-pec        =    "t_sender            "
                     move  01             to   v-num
           else if   w-psg-asd-pec        =    "t_sender_aut        "
                     move  02             to   v-num
           else if   w-psg-asd-pec        =    "t_sender_tls        "
                     move  03             to   v-num
           else if   w-psg-asd-pec        =    "t_sender_ssl        "
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-asd-pec-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-asd-pec-999.
       acc-asd-pec-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "t_sender            "
                                          to   w-psg-asd-pec
           else if   v-num                =    02
                     move  "t_sender_aut        "
                                          to   w-psg-asd-pec
           else if   v-num                =    03
                     move  "t_sender_tls        "
                                          to   w-psg-asd-pec
           else if   v-num                =    04
                     move  "t_sender_ssl        "
                                          to   w-psg-asd-pec
           else      move  spaces         to   w-psg-asd-pec          .
       acc-asd-pec-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-asd-pec-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-asd-pec-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-asd-pec-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-asd-pec-100.
       acc-asd-pec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Agente di spedizione PEC          *
      *    *-----------------------------------------------------------*
       vis-asd-pec-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-asd-smp-lun    to   v-car                  .
           move      w-exp-asd-smp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      14                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-asd-smp-tbl    to   v-txt                  .
      *
           if        w-psg-asd-pec        =    spaces or
                     w-psg-asd-pec        =    "t_sender            "
                     move  01             to   v-num
           else if   w-psg-asd-pec        =    "t_sender_aut        "
                     move  02             to   v-num
           else if   w-psg-asd-pec        =    "t_sender_tls        "
                     move  03             to   v-num
           else if   w-psg-asd-pec        =    "t_sender_ssl        "
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-asd-pec-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Password per MySQL                   *
      *    *-----------------------------------------------------------*
       acc-pwd-sql-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pwd-sql-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-psg-pwd-sql        to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-sql-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-pwd-sql-999.
       acc-pwd-sql-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-psg-pwd-sql          .
       acc-pwd-sql-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pwd-sql-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
           perform   vis-pwd-sql-000      thru vis-pwd-sql-999        .
       acc-pwd-sql-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-pwd-sql-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-pwd-sql-100.
       acc-pwd-sql-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Password per account SMTP         *
      *    *-----------------------------------------------------------*
       vis-pwd-sql-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione mascherata                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "W"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-psg-pwd-sql        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pwd-sql-999.
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
      *              * Controllo su passwords                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Su aziende                                  *
      *                  *---------------------------------------------*
           if        w-psg-pwd-azi        not  = "S" and
                     w-psg-pwd-azi        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
      *                  *---------------------------------------------*
      *                  * Su utenti                                   *
      *                  *---------------------------------------------*
           if        w-psg-pwd-ute        not  = "S" and
                     w-psg-pwd-ute        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
      *                  *---------------------------------------------*
      *                  * Su comandi                                  *
      *                  *---------------------------------------------*
           if        w-psg-pwd-cmd        not  = "S" and
                     w-psg-pwd-cmd        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
      *                  *---------------------------------------------*
      *                  * Su stampanti                                *
      *                  *---------------------------------------------*
           if        w-psg-pwd-stp        not  = "S" and
                     w-psg-pwd-stp        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
       cnt-tdo-nok-200.
      *              *-------------------------------------------------*
      *              * Controllo su riservatezza                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Su gerarchie comandi                        *
      *                  *---------------------------------------------*
           if        w-psg-rsv-ide        not  = "S" and
                     w-psg-rsv-ide        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
      *                  *---------------------------------------------*
      *                  * Su tipo utente                              *
      *                  *---------------------------------------------*
           if        w-psg-rsv-tut        not  = "S" and
                     w-psg-rsv-tut        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
      *                  *---------------------------------------------*
      *                  * Su livello di protezione                    *
      *                  *---------------------------------------------*
           if        w-psg-rsv-lip        not  = "S" and
                     w-psg-rsv-lip        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
       cnt-tdo-nok-300.
      *              *-------------------------------------------------*
      *              * Controllo su parametri generali                 *
      *              *-------------------------------------------------*
       cnt-tdo-nok-310.
      *                  *---------------------------------------------*
      *                  * Su visualizzazione codice azienda           *
      *                  *---------------------------------------------*
           if        w-psg-snx-azi        not  = "S" and
                     w-psg-snx-azi        not  = "N"
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
       cnt-tdo-nok-320.
      *                  *---------------------------------------------*
      *                  * Su template per lo spooler                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il sistema operativo ospite non am-  *
      *                      * mette la stampa in spool si passa al    *
      *                      * controllo del campo successivo          *
      *                      *-----------------------------------------*
           if        w-mop-sys-snx-ssp    not  = spaces
                     go to cnt-tdo-nok-330.
      *                      *-----------------------------------------*
      *                      * Altrimenti si controlla                 *
      *                      *-----------------------------------------*
           if        w-psg-tem-spl        =    w-acc-tem-spl-nop or
                     w-psg-tem-spl        =    spaces
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
       cnt-tdo-nok-330.
      *                  *---------------------------------------------*
      *                  * Su template per blocco spooler              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il sistema operativo ospite non am-  *
      *                      * mette la stampa in spool si passa al    *
      *                      * controllo del campo successivo          *
      *                      *-----------------------------------------*
           if        w-mop-sys-snx-ssp    not  = spaces
                     go to cnt-tdo-nok-340.
      *                      *-----------------------------------------*
      *                      * Altrimenti si controlla                 *
      *                      *-----------------------------------------*
           if        w-psg-tem-bss        =    w-acc-tem-bss-nop
                     move  "Impostazioni errate !"
                                          to   w-err-box-err-msg
                     go to cnt-tdo-nok-900.
       cnt-tdo-nok-340.
      *                  *---------------------------------------------*
      *                  * A normalizzazioni finali                    *
      *                  *---------------------------------------------*
       cnt-tdo-nok-600.
      *              *-------------------------------------------------*
      *              * Normalizzazione se controlli superati           *
      *              *-------------------------------------------------*
       cnt-tdo-nok-610.
      *              *-------------------------------------------------*
      *              * Controllo su passwords                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Su terminali                                *
      *                  *---------------------------------------------*
           move      "N"                  to   w-psg-pwd-ter          .
       cnt-tdo-nok-620.
      *              *-------------------------------------------------*
      *              * Controllo su riservatezza                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Su tasks                                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-psg-rsv-tsk          .
      *                  *---------------------------------------------*
      *                  * Su Menu'                                    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-psg-rsv-mnu          .
      *                  *---------------------------------------------*
      *                  * Su Procedure                                *
      *                  *---------------------------------------------*
           move      "N"                  to   w-psg-rsv-prc          .
       cnt-tdo-nok-630.
      *              *-------------------------------------------------*
      *              * Parametri generali                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento o decremento per Left o Rght in  *
      *                  * visualizzazione archivi-stampa              *
      *                  *---------------------------------------------*
           move      60                   to   w-psg-iod-lor          .
      *                  *---------------------------------------------*
      *                  * Se il sistema operativo ospite non ammette  *
      *                  * la stampa in spool si normalizzano i valori *
      *                  * dei campi ad essa relativi, ponendo il va-  *
      *                  * lore convenzionale per 'operazione non con- *
      *                  * sentita'                                    *
      *                  *---------------------------------------------*
           if        w-mop-sys-snx-ssp    not  = spaces
                     move  "."            to   w-psg-tem-spl
                     move  "."            to   w-psg-tem-bss          .
      *                  *---------------------------------------------*
      *                  * Parametro di controllo per la visualizza-   *
      *                  * zione archivi-stampa                        *
      *                  *---------------------------------------------*
           move      w-psg-cnt-vpf        to   w-acc-vis-prf          .
           if        w-acc-vis-prf-001    not  = "T" and
                     w-acc-vis-prf-001    not  = "I"
                     move  "I"            to   w-acc-vis-prf-001      .
           if        w-acc-vis-prf-002    not  = "S" and
                     w-acc-vis-prf-002    not  = "N"
                     move  "N"            to   w-acc-vis-prf-001      .
           move      w-acc-vis-prf        to   w-psg-cnt-vpf          .
       cnt-tdo-nok-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli tutti superati             *
      *              *-------------------------------------------------*
           go to     cnt-tdo-nok-999.
       cnt-tdo-nok-900.
      *              *-------------------------------------------------*
      *              * Uscita per controlli non superati               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
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
       nor-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave testata                   *
      *    *-----------------------------------------------------------*
       nor-nok-tes-000.
           perform   nor-psg-auc-000      thru nor-psg-auc-999        .
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
      *              * Lettura record personalizzazioni generali       *
      *              *-------------------------------------------------*
           move      "RD"                 to   j-ope                  .
           move      "PSG "               to   j-tre                  .
           move      spaces               to   j-kre                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        j-rsc                =    spaces
                     go to rou-let-reg-100.
       rou-let-reg-050.
      *                  *---------------------------------------------*
      *                  * Se movimento non trovato                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se inserimento consentito          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to rou-let-reg-060.
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
       rou-let-reg-060.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : Inserimento        *
      *                      *-----------------------------------------*
           move      "I"                  to   w-cnt-mfu-tip-fun      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      "NO"                 to   j-ope                  .
           move      "PSG "               to   j-tre                  .
           move      spaces               to   j-kre                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *                      *-----------------------------------------*
      *                      * Record in area di lavoro                *
      *                      *-----------------------------------------*
           move      j-dat                to   w-psg                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazioni                         *
      *                      *-----------------------------------------*
           if        w-psg-scg-cdt        not  numeric
                     move  zero           to   w-psg-scg-cdt          .
           if        w-psg-scg-chh        not  numeric
                     move  zero           to   w-psg-scg-chh          .
           if        w-psg-ngg-val        not  numeric
                     move  zero           to   w-psg-ngg-val          .
           if        w-psg-tip-asp        not  numeric
                     move  zero           to   w-psg-tip-asp          .
           if        w-psg-max-err        not  numeric
                     move  zero           to   w-psg-max-err          .
           if        w-psg-tip-ast        not  numeric
                     move  zero           to   w-psg-tip-ast          .
      *                      *-----------------------------------------*
      *                      * Forzature                               *
      *                      *-----------------------------------------*
           move      "N"                  to   w-psg-rsv-usl          .
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
      *                      * Record in area di lavoro                *
      *                      *-----------------------------------------*
           move      j-dat                to   w-psg                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazioni                         *
      *                      *-----------------------------------------*
           if        w-psg-scg-cdt        not  numeric
                     move  zero           to   w-psg-scg-cdt          .
           if        w-psg-scg-chh        not  numeric
                     move  zero           to   w-psg-scg-chh          .
           if        w-psg-ngg-val        not  numeric
                     move  zero           to   w-psg-ngg-val          .
           if        w-psg-tip-asp        not  numeric
                     move  zero           to   w-psg-tip-asp          .
           if        w-psg-max-err        not  numeric
                     move  zero           to   w-psg-max-err          .
           if        w-psg-tip-ast        not  numeric
                     move  zero           to   w-psg-tip-ast          .
           if        w-psg-snx-box        =    spaces
                     move  "N"            to   w-psg-snx-box          .
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
      *              *-------------------------------------------------*
      *              * Avvertimento                                    *
      *              *-------------------------------------------------*
           move      "Attenzione : eventuali modifiche apportate, avrann
      -              "o effetto a    "    to   w-err-box-err-msg      .
           move      "             partire dalla prossima sessione di Ta
      -              "ngram !        "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
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
      *              * Trattamento file [PSG]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se inserimento                              *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-tip-fun    not  = "I"
                     go to scr-mov-fil-500.
      *                      *-----------------------------------------*
      *                      * Scrittura record                        *
      *                      *-----------------------------------------*
           move      "PT"                 to   j-ope                  .
           move      "PSG "               to   j-tre                  .
           move      spaces               to   j-kre                  .
           move      w-psg                to   j-dat                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-500.
      *                  *---------------------------------------------*
      *                  * Se modifica                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Update record                           *
      *                      *-----------------------------------------*
           move      "UP"                 to   j-ope                  .
           move      "PSG "               to   j-tre                  .
           move      spaces               to   j-kre                  .
           move      w-psg                to   j-dat                  .
           call      "swd/mod/prg/obj/maucmf"
                                        using  j                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     scr-mov-fil-999.
       scr-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento da file                                  *
      *    *-----------------------------------------------------------*
       del-mov-fil-000.
       del-mov-fil-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/werrbox0.cps"                   .

      *    *===========================================================*
      *    * Work per normalizzazione record di [auc] 'psg'            *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpsg0.cps"                   .

