       Identification Division.
       Program-Id.                                 pmtv0010           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mtv                 *
      *                                Settore:    prs                 *
      *                                   Fase:    mtv001              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 03/11/92    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione filtri per ordinamento e selezione *
      *                    su archivio [mtv]                           *
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
                     "mtv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "prs"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "mtv001"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pmtv0010"                                       .
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
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [zum]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzum"                          .
      *        *-------------------------------------------------------*
      *        * [zv1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv1"                          .
      *        *-------------------------------------------------------*
      *        * [zv2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv2"                          .
      *        *-------------------------------------------------------*
      *        * [zv3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv3"                          .
      *        *-------------------------------------------------------*
      *        * [zvs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzvs"                          .

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
               10  w-tes-des-flt          pic  x(40)                  .
               10  w-tes-dat-mtv.
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
                   15  w-tes-tip-mtv      pic  9(02)                  .
                   15  w-tes-umi-gst      pic  x(03)                  .
                   15  w-tes-cod-s01      pic  9(05)                  .
                   15  w-tes-cod-s02      pic  9(05)                  .
                   15  w-tes-cod-s03      pic  9(05)                  .
               10  w-tes-des-dat.
                   15  w-tes-umi-gst-des  pic  x(20)                  .
                   15  w-tes-cod-s01-des  pic  x(20)                  .
                   15  w-tes-cod-s02-des  pic  x(20)                  .
                   15  w-tes-cod-s03-des  pic  x(20)                  .

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
      *    * filtro 'mtv '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/wzosmtv0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zum]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zum.
               10  w-let-arc-zum-flg      pic  x(01)                  .
               10  w-let-arc-zum-cod      pic  x(03)                  .
               10  w-let-arc-zum-des      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvs.
               10  w-let-arc-zvs-flg      pic  x(01)                  .
               10  w-let-arc-zvs-tip      pic  9(02)                  .
               10  w-let-arc-zvs-cod      pic  9(05)                  .
               10  w-let-arc-zvs-des      pic  x(20)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento, selezione                *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 4    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(40) value
                            "Classe, gruppo, sottogr., cod. mat. v.  ".
                   15  filler             pic  x(40) value
                            "Classe, gruppo, sottogr., desc. mat. v. ".
                   15  filler             pic  x(40) value
                            "Codice materiale vario                  ".
                   15  filler             pic  x(40) value
                            "Descrizione materiale vario             ".

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [mtv]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/azosmtv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice materia varia 'mtv'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe materia varia    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv10.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice gruppo materia varia    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv20.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice sottogruppo mat. varia  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv30.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione unita' di misura               *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 1            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs1.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 2            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs2.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 3            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs3.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err su controllo tasto Do non chiave         *
      *        *-------------------------------------------------------*
           05  w-err-tdo-nok.
               10  w-err-tdo-nok-msg      pic  x(48)                  .

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
       exe-acc-cmp-800.
      *              *-------------------------------------------------*
      *              * Flag globale di avvenuta almeno una modifica    *
      *              *-------------------------------------------------*
           if        w-cnt-acc-sav-mod    not  = spaces
                     move  "#"            to   w-cnt-acc-flg-aum      .
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
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      66                   to   v-pos                  .
           if        w-cnt-mfu-tip-fun    =    "I"
                     move  "    Inserimento"
                                          to   v-alf
           else if   w-cnt-mfu-tip-fun    =    "M"
                     move  "       Modifica"
                                          to   v-alf
           else if   w-cnt-mfu-tip-fun    =    "V"
                     move  "Visualizzazione"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
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
      *              * Open modulo accettazione codice filtro          *
      *              *-------------------------------------------------*
           perform   cod-zos-mtv-opn-000  thru cod-zos-mtv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice mat. v. 'mtv'   *
      *              *-------------------------------------------------*
           perform   cod-cod-mtv-opn-000  thru cod-cod-mtv-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice classe          *
      *              *-------------------------------------------------*
           perform   cod-mne-zv1-opn-000  thru cod-mne-zv1-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice gruppo          *
      *              *-------------------------------------------------*
           perform   cod-mne-zv2-opn-000  thru cod-mne-zv2-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice sottogruppo     *
      *              *-------------------------------------------------*
           perform   cod-mne-zv3-opn-000  thru cod-mne-zv3-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione unita' di misura       *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-opn-000  thru cod-cod-zum-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 1    *
      *              *-------------------------------------------------*
           perform   cmn-zvs-001-opn-000  thru cmn-zvs-001-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 2    *
      *              *-------------------------------------------------*
           perform   cmn-zvs-002-opn-000  thru cmn-zvs-002-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice statistico 3    *
      *              *-------------------------------------------------*
           perform   cmn-zvs-003-opn-000  thru cmn-zvs-003-opn-999    .
      *              *-------------------------------------------------*
      *              * Preparazione tipo record con : 'mtv'            *
      *              *-------------------------------------------------*
           move      "mtv "               to   w-tes-tip-rec          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di mnemonico a spaces in   *
      *              * ultimo record in Inserimento                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-usc-ult-mne-spc      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice filtro         *
      *              *-------------------------------------------------*
           perform   cod-zos-mtv-cls-000  thru cod-zos-mtv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice mat. v. 'mtv'  *
      *              *-------------------------------------------------*
           perform   cod-cod-mtv-cls-000  thru cod-cod-mtv-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice classe         *
      *              *-------------------------------------------------*
           perform   cod-mne-zv1-cls-000  thru cod-mne-zv1-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice gruppo         *
      *              *-------------------------------------------------*
           perform   cod-mne-zv2-cls-000  thru cod-mne-zv2-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice sottogruppo    *
      *              *-------------------------------------------------*
           perform   cod-mne-zv3-cls-000  thru cod-mne-zv3-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione unita' di misura      *
      *              *-------------------------------------------------*
           perform   cod-cod-zum-cls-000  thru cod-cod-zum-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 1   *
      *              *-------------------------------------------------*
           perform   cmn-zvs-001-cls-000  thru cmn-zvs-001-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 2   *
      *              *-------------------------------------------------*
           perform   cmn-zvs-002-cls-000  thru cmn-zvs-002-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice statistico 3   *
      *              *-------------------------------------------------*
           perform   cmn-zvs-003-cls-000  thru cmn-zvs-003-cls-999    .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *              *-------------------------------------------------*
      *              * [zv1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *              *-------------------------------------------------*
      *              * [zv2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *              *-------------------------------------------------*
      *              * [zv3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
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
      *              * [zvs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvs                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [mtv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *              *-------------------------------------------------*
      *              * [zv1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *              *-------------------------------------------------*
      *              * [zv2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv2                 .
      *              *-------------------------------------------------*
      *              * [zv3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv3                 .
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
      *              * [zvs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvs                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave della registrazione             *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *
      *              *-------------------------------------------------*
      *              * Se flag di mnemonico a spaces in ultimo Inseri- *
      *              * mento : uscita come per Exit                    *
      *              *-------------------------------------------------*
      *
           if        w-usc-ult-mne-spc    not  = spaces
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-key-reg-999.
      *
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
       acc-cod-flt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-mtv-ope      .
           move      w-tes-cod-flt        to   w-cod-zos-mtv-cod      .
           move      04                   to   w-cod-zos-mtv-lin      .
           move      30                   to   w-cod-zos-mtv-pos      .
           move      06                   to   w-cod-zos-mtv-dln      .
           move      30                   to   w-cod-zos-mtv-dps      .
           move      "<B"                 to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-zos-mtv-cll-000  thru cod-zos-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-zos-mtv-foi-000  thru cod-zos-mtv-foi-999    .
       acc-cod-flt-110.
           perform   cod-zos-mtv-cll-000  thru cod-zos-mtv-cll-999    .
           if        w-cod-zos-mtv-ope    =    "F+"
                     go to acc-cod-flt-115.
           if        w-cod-zos-mtv-ope    =    "AC"
                     go to acc-cod-flt-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-flt-115.
           perform   cod-zos-mtv-foi-000  thru cod-zos-mtv-foi-999    .
           go to     acc-cod-flt-110.
       acc-cod-flt-120.
           move      w-cod-zos-mtv-cod    to   v-num                  .
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
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attuale    *
      *                          *-------------------------------------*
           subtract  1                    from w-cnt-sts-imp-npt      .
      *                          *-------------------------------------*
      *                          * Riciclo ad impostazione testata     *
      *                          *-------------------------------------*
           go to     acc-nok-reg-200.
       acc-nok-reg-400.
      *                      *-----------------------------------------*
      *                      * Se spostamento a pagina successiva      *
      *                      *-----------------------------------------*
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
                     go to acc-nok-reg-800.
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
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attuale    *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-sts-imp-npt      .
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
      *              * La testata e' composta di nr. 2 pagine          *
      *              *-------------------------------------------------*
           move      2                    to   w-cnt-sts-imp-mpt      .
       dmp-tes-reg-999.
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     acc-tes-reg-999.
       acc-tes-reg-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Descrizione filtro                          *
      *                  *---------------------------------------------*
           perform   acc-des-flt-000      thru acc-des-flt-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
       acc-tes-reg-105.
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
       acc-tes-reg-110.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento, selezione                 *
      *                  *---------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-105.
       acc-tes-reg-115.
      *                  *---------------------------------------------*
      *                  * Classe min                                  *
      *                  *---------------------------------------------*
           perform   acc-cla-min-000      thru acc-cla-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-110.
       acc-tes-reg-120.
      *                  *---------------------------------------------*
      *                  * Classe max                                  *
      *                  *---------------------------------------------*
           perform   acc-cla-max-000      thru acc-cla-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-115.
       acc-tes-reg-125.
      *                  *---------------------------------------------*
      *                  * Gruppo min                                  *
      *                  *---------------------------------------------*
           perform   acc-gru-min-000      thru acc-gru-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-120.
       acc-tes-reg-130.
      *                  *---------------------------------------------*
      *                  * Gruppo max                                  *
      *                  *---------------------------------------------*
           perform   acc-gru-max-000      thru acc-gru-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-125.
       acc-tes-reg-135.
      *                  *---------------------------------------------*
      *                  * Sottogruppo min                             *
      *                  *---------------------------------------------*
           perform   acc-sgr-min-000      thru acc-sgr-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-130.
       acc-tes-reg-140.
      *                  *---------------------------------------------*
      *                  * Sottogruppo max                             *
      *                  *---------------------------------------------*
           perform   acc-sgr-max-000      thru acc-sgr-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-135.
       acc-tes-reg-145.
      *                  *---------------------------------------------*
      *                  * Codice min                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-min-000      thru acc-cod-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-140.
       acc-tes-reg-150.
      *                  *---------------------------------------------*
      *                  * Codice max                                  *
      *                  *---------------------------------------------*
           perform   acc-cod-max-000      thru acc-cod-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-145.
       acc-tes-reg-155.
      *                  *---------------------------------------------*
      *                  * Descrizione min                             *
      *                  *---------------------------------------------*
           perform   acc-des-min-000      thru acc-des-min-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-150.
       acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Descrizione max                             *
      *                  *---------------------------------------------*
           perform   acc-des-max-000      thru acc-des-max-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     go to acc-tes-reg-155.
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
                     go to acc-tes-reg-160.
      *                  *---------------------------------------------*
      *                  * Fine Pagina                                 *
      *                  *---------------------------------------------*
           move      "+"                  to   w-cnt-tus-acc-tes      .
           go to     acc-tes-reg-999.
       acc-tes-reg-200.
      *                  *---------------------------------------------*
      *                  * Tipo materia varia                          *
      *                  *---------------------------------------------*
           perform   acc-tip-mtv-000      thru acc-tip-mtv-999        .
           if        v-key                =    "NXSC"
                     move  "+"            to   w-cnt-tus-acc-tes      .
           if        v-key                =    "PRSC"
                     move  "-"            to   w-cnt-tus-acc-tes      .
           if        w-cnt-tus-acc-tes    not  = spaces
                     go to acc-tes-reg-999.
           if        v-key                =    "UP  "
                     move  "-"            to   w-cnt-tus-acc-tes
                     go to acc-tes-reg-999.
       acc-tes-reg-210.
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
           perform   acc-umi-gst-000      thru acc-umi-gst-999        .
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
                     go to acc-tes-reg-210.
       acc-tes-reg-230.
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
                     go to acc-tes-reg-220.
       acc-tes-reg-240.
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
                     go to acc-tes-reg-230.
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     vis-tes-reg-999.
       vis-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Descrizione filtro                              *
      *              *-------------------------------------------------*
           perform   vis-des-flt-000      thru vis-des-flt-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   vis-cod-mne-000      thru vis-cod-mne-999        .
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
           go to     vis-tes-reg-999.
       vis-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Tipo materia varia                              *
      *              *-------------------------------------------------*
           perform   vis-tip-mtv-000      thru vis-tip-mtv-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   vis-umi-gst-000      thru vis-umi-gst-999        .
           perform   vis-des-umi-000      thru vis-des-umi-999        .
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
                     depending            on   w-cnt-sts-imp-npt      .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-100.
      *              *-------------------------------------------------*
      *              * Descrizione filtro                              *
      *              *-------------------------------------------------*
           perform   pmt-des-flt-000      thru pmt-des-flt-999        .
      *              *-------------------------------------------------*
      *              * Codice mnemonico                                *
      *              *-------------------------------------------------*
           perform   pmt-cod-mne-000      thru pmt-cod-mne-999        .
      *              *-------------------------------------------------*
      *              * Tipo ordinamento, selezione                     *
      *              *-------------------------------------------------*
           perform   pmt-tip-ord-000      thru pmt-tip-ord-999        .
           go to     pmt-tes-reg-999.
       pmt-tes-reg-200.
      *              *-------------------------------------------------*
      *              * Tipo materia varia                              *
      *              *-------------------------------------------------*
           perform   pmt-tip-mtv-000      thru pmt-tip-mtv-999        .
      *              *-------------------------------------------------*
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           perform   pmt-umi-gst-000      thru pmt-umi-gst-999        .
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
           go to     pmt-tes-reg-999.
       pmt-tes-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Descrizione                      *
      *    *-----------------------------------------------------------*
       pmt-des-flt-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
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
      *    * Visualizzazione prompt : Tipo ordinamento, selezione      *
      *    *-----------------------------------------------------------*
       pmt-tip-ord-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
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
           move      12                   to   v-lin                  .
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
           move      "  - Codice materiale       :"
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
           move      "  - Descrizione materiale  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Tipo materia varia               *
      *    *-----------------------------------------------------------*
       pmt-tip-mtv-000.
       pmt-tip-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Unita' di misura                 *
      *    *-----------------------------------------------------------*
       pmt-umi-gst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Unita' di misura           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-umi-gst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Codice statistico 1              *
      *    *-----------------------------------------------------------*
       pmt-cod-s01-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice statistico 3        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-s03-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Presa visione per pagina     *
      *    *-----------------------------------------------------------*
       acc-pre-vpg-000.
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
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
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
                     go to acc-des-flt-600.
           if        w-tes-cod-flt        not  = 9999999
                     go to acc-des-flt-600.
           if        v-key                =    "UP  "
                     go to acc-des-flt-600
           else      go to acc-des-flt-100.
       acc-des-flt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      w-tes-des-flt (1)    to   w-all-str-alf          .
           move      40                   to   w-all-str-alf          .
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
           move      06                   to   v-lin                  .
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
      *    * Accettazione campo testata : Tipo ordinamento, selezione  *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
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
           move      10                   to   v-lin                  .
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
       acc-cla-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv1-ope      .
           move      w-tes-cla-min (1)    to   w-cod-mne-zv1-cla      .
           move      15                   to   w-cod-mne-zv1-lin      .
           move      30                   to   w-cod-mne-zv1-pos      .
           move      zero                 to   w-cod-mne-zv1-dln      .
           move      zero                 to   w-cod-mne-zv1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
       acc-cla-min-110.
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           if        w-cod-mne-zv1-ope    =    "F+"
                     go to acc-cla-min-115.
           if        w-cod-mne-zv1-ope    =    "AC"
                     go to acc-cla-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cla-min-115.
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
           go to     acc-cla-min-110.
       acc-cla-min-120.
           move      w-cod-mne-zv1-cla    to   v-num                  .
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
       acc-cla-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv1-ope      .
           move      w-tes-cla-max (1)    to   w-cod-mne-zv1-cla      .
           move      15                   to   w-cod-mne-zv1-lin      .
           move      54                   to   w-cod-mne-zv1-pos      .
           move      zero                 to   w-cod-mne-zv1-dln      .
           move      zero                 to   w-cod-mne-zv1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
       acc-cla-max-110.
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           if        w-cod-mne-zv1-ope    =    "F+"
                     go to acc-cla-max-115.
           if        w-cod-mne-zv1-ope    =    "AC"
                     go to acc-cla-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cla-max-115.
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
           go to     acc-cla-max-110.
       acc-cla-max-120.
           move      w-cod-mne-zv1-cla    to   v-num                  .
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
       acc-gru-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv2-ope      .
           move      w-tes-cla-min (1)    to   w-cod-mne-zv2-cla      .
           move      w-tes-gru-min (1)    to   w-cod-mne-zv2-gru      .
           move      16                   to   w-cod-mne-zv2-lin      .
           move      30                   to   w-cod-mne-zv2-pos      .
           move      zero                 to   w-cod-mne-zv2-dln      .
           move      zero                 to   w-cod-mne-zv2-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv2-cll-000  thru cod-mne-zv2-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv2-foi-000  thru cod-mne-zv2-foi-999    .
       acc-gru-min-110.
           perform   cod-mne-zv2-cll-000  thru cod-mne-zv2-cll-999    .
           if        w-cod-mne-zv2-ope    =    "F+"
                     go to acc-gru-min-115.
           if        w-cod-mne-zv2-ope    =    "AC"
                     go to acc-gru-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-gru-min-115.
           perform   cod-mne-zv2-foi-000  thru cod-mne-zv2-foi-999    .
           go to     acc-gru-min-110.
       acc-gru-min-120.
           move      w-cod-mne-zv2-gru    to   v-num                  .
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
       acc-gru-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv2-ope      .
           move      w-tes-cla-max (1)    to   w-cod-mne-zv2-cla      .
           move      w-tes-gru-max (1)    to   w-cod-mne-zv2-gru      .
           move      16                   to   w-cod-mne-zv2-lin      .
           move      54                   to   w-cod-mne-zv2-pos      .
           move      zero                 to   w-cod-mne-zv2-dln      .
           move      zero                 to   w-cod-mne-zv2-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv2-cll-000  thru cod-mne-zv2-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv2-foi-000  thru cod-mne-zv2-foi-999    .
       acc-gru-max-110.
           perform   cod-mne-zv2-cll-000  thru cod-mne-zv2-cll-999    .
           if        w-cod-mne-zv2-ope    =    "F+"
                     go to acc-gru-max-115.
           if        w-cod-mne-zv2-ope    =    "AC"
                     go to acc-gru-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-gru-max-115.
           perform   cod-mne-zv2-foi-000  thru cod-mne-zv2-foi-999    .
           go to     acc-gru-max-110.
       acc-gru-max-120.
           move      w-cod-mne-zv2-gru    to   v-num                  .
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
       acc-sgr-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv3-ope      .
           move      w-tes-cla-min (1)    to   w-cod-mne-zv3-cla      .
           move      w-tes-gru-min (1)    to   w-cod-mne-zv3-gru      .
           move      w-tes-sgr-min (1)    to   w-cod-mne-zv3-sgr      .
           move      17                   to   w-cod-mne-zv3-lin      .
           move      30                   to   w-cod-mne-zv3-pos      .
           move      zero                 to   w-cod-mne-zv3-dln      .
           move      zero                 to   w-cod-mne-zv3-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv3-cll-000  thru cod-mne-zv3-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv3-foi-000  thru cod-mne-zv3-foi-999    .
       acc-sgr-min-110.
           perform   cod-mne-zv3-cll-000  thru cod-mne-zv3-cll-999    .
           if        w-cod-mne-zv3-ope    =    "F+"
                     go to acc-sgr-min-115.
           if        w-cod-mne-zv3-ope    =    "AC"
                     go to acc-sgr-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgr-min-115.
           perform   cod-mne-zv3-foi-000  thru cod-mne-zv3-foi-999    .
           go to     acc-sgr-min-110.
       acc-sgr-min-120.
           move      w-cod-mne-zv3-sgr    to   v-num                  .
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
       acc-sgr-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv3-ope      .
           move      w-tes-cla-max (1)    to   w-cod-mne-zv3-cla      .
           move      w-tes-gru-max (1)    to   w-cod-mne-zv3-gru      .
           move      w-tes-sgr-max (1)    to   w-cod-mne-zv3-sgr      .
           move      17                   to   w-cod-mne-zv3-lin      .
           move      54                   to   w-cod-mne-zv3-pos      .
           move      zero                 to   w-cod-mne-zv3-dln      .
           move      zero                 to   w-cod-mne-zv3-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-zv3-cll-000  thru cod-mne-zv3-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-zv3-foi-000  thru cod-mne-zv3-foi-999    .
       acc-sgr-max-110.
           perform   cod-mne-zv3-cll-000  thru cod-mne-zv3-cll-999    .
           if        w-cod-mne-zv3-ope    =    "F+"
                     go to acc-sgr-max-115.
           if        w-cod-mne-zv3-ope    =    "AC"
                     go to acc-sgr-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-sgr-max-115.
           perform   cod-mne-zv3-foi-000  thru cod-mne-zv3-foi-999    .
           go to     acc-sgr-max-110.
       acc-sgr-max-120.
           move      w-cod-mne-zv3-sgr    to   v-num                  .
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
       acc-cod-min-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-mtv-ope      .
           move      "L"                  to   w-cod-cod-mtv-tac      .
           move      zero                 to   w-cod-cod-mtv-num      .
           move      w-tes-cod-min (1)    to   w-cod-cod-mtv-alf      .
           move      19                   to   w-cod-cod-mtv-lin      .
           move      30                   to   w-cod-cod-mtv-pos      .
           move      zero                 to   w-cod-cod-mtv-dln      .
           move      zero                 to   w-cod-cod-mtv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
       acc-cod-min-110.
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           if        w-cod-cod-mtv-ope    =    "F+"
                     go to acc-cod-min-115.
           if        w-cod-cod-mtv-ope    =    "AC"
                     go to acc-cod-min-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-min-115.
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
           go to     acc-cod-min-110.
       acc-cod-min-120.
           move      w-cod-cod-mtv-alf    to   v-alf                  .
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
       acc-cod-max-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-mtv-ope      .
           move      "L"                  to   w-cod-cod-mtv-tac      .
           move      zero                 to   w-cod-cod-mtv-num      .
           move      w-tes-cod-max (1)    to   w-cod-cod-mtv-alf      .
           move      19                   to   w-cod-cod-mtv-lin      .
           move      54                   to   w-cod-cod-mtv-pos      .
           move      zero                 to   w-cod-cod-mtv-dln      .
           move      zero                 to   w-cod-cod-mtv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
       acc-cod-max-110.
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           if        w-cod-cod-mtv-ope    =    "F+"
                     go to acc-cod-max-115.
           if        w-cod-cod-mtv-ope    =    "AC"
                     go to acc-cod-max-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-max-115.
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
           go to     acc-cod-max-110.
       acc-cod-max-120.
           move      w-cod-cod-mtv-alf    to   v-alf                  .
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
      *    * Accettazione campo selezione : Descrizione massima        *
      *    *-----------------------------------------------------------*
       acc-des-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
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
           move      "DO  "               to   v-pfk (03)             .
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
      *    * Accettazione campo selezione : Descrizione massima        *
      *    *-----------------------------------------------------------*
       acc-des-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
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
           move      "DO  "               to   v-pfk (03)             .
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
      *    * Accettazione campo testata : Tipo materia varia           *
      *    *-----------------------------------------------------------*
       acc-tip-mtv-000.
       acc-tip-mtv-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo testata : Tipo materia varia        *
      *    *-----------------------------------------------------------*
       vis-tip-mtv-000.
       vis-tip-mtv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo testata : Unita' di misura             *
      *    *-----------------------------------------------------------*
       acc-umi-gst-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-umi-gst-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-umi-ope      .
           move      w-tes-umi-gst (1)    to   w-cod-cod-umi-cod      .
           move      09                   to   w-cod-cod-umi-lin      .
           move      30                   to   w-cod-cod-umi-pos      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "NXSC"               to   v-pfk (08)             .
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
       acc-umi-gst-110.
           perform   cod-cod-zum-cll-000  thru cod-cod-zum-cll-999    .
           if        w-cod-cod-umi-ope    =    "F+"
                     go to acc-umi-gst-115.
           if        w-cod-cod-umi-ope    =    "AC"
                     go to acc-umi-gst-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-umi-gst-115.
           perform   cod-cod-zum-foi-000  thru cod-cod-zum-foi-999    .
           go to     acc-umi-gst-110.
       acc-umi-gst-120.
           move      w-cod-cod-umi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-umi-gst-999.
      *              *-------------------------------------------------*
      *              * Se Delt                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-umi-gst-999.
       acc-umi-gst-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-tes-umi-gst (1)      .
       acc-umi-gst-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura tabella unita' di misura            *
      *                  *---------------------------------------------*
           move      w-tes-umi-gst (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati associati al codice     *
      *                  * unita' di misura                            *
      *                  *---------------------------------------------*
           move      w-let-arc-zum-des    to   w-tes-umi-gst-des (1)  .
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
                     go to acc-umi-gst-100.
       acc-umi-gst-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-umi-gst-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform cnt-tdo-nok-000
                                          thru cnt-tdo-nok-999
                     if      w-cnt-tdo-nok-flg
                                          =    spaces
                             move  "S"    to   w-cnt-tus-acc-tes
                             go to acc-umi-gst-999
                     else    move  spaces to   w-cnt-tdo-nok-flg
                             go to acc-umi-gst-100.
       acc-umi-gst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Unita' di misura                  *
      *    *-----------------------------------------------------------*
       vis-umi-gst-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-umi-gst (1)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-umi-gst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione unita' di misura      *
      *    *-----------------------------------------------------------*
       vis-des-umi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-tes-umi-gst-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-umi-999.
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
           move      "AC"                 to   w-cmn-zvs-001-ope      .
           move      w-tes-cod-s01 (1)    to   w-cmn-zvs-001-cod      .
           move      11                   to   w-cmn-zvs-001-lin      .
           move      30                   to   w-cmn-zvs-001-pos      .
           move      11                   to   w-cmn-zvs-001-dln      .
           move      37                   to   w-cmn-zvs-001-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zvs-001-cll-000  thru cmn-zvs-001-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zvs-001-foi-000  thru cmn-zvs-001-foi-999    .
       acc-cod-s01-110.
           perform   cmn-zvs-001-cll-000  thru cmn-zvs-001-cll-999    .
           if        w-cmn-zvs-001-ope    =    "F+"
                     go to acc-cod-s01-115.
           if        w-cmn-zvs-001-ope    =    "AC"
                     go to acc-cod-s01-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s01-115.
           perform   cmn-zvs-001-foi-000  thru cmn-zvs-001-foi-999    .
           go to     acc-cod-s01-110.
       acc-cod-s01-120.
           move      w-cmn-zvs-001-cod    to   v-num                  .
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
           move      01                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zvs-des    to   w-tes-cod-s01-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs1-000      thru vis-des-cs1-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvs-flg    not  = spaces
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
           move      11                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      37                   to   v-pos                  .
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
           move      "AC"                 to   w-cmn-zvs-002-ope      .
           move      w-tes-cod-s02 (1)    to   w-cmn-zvs-002-cod      .
           move      13                   to   w-cmn-zvs-002-lin      .
           move      30                   to   w-cmn-zvs-002-pos      .
           move      13                   to   w-cmn-zvs-002-dln      .
           move      37                   to   w-cmn-zvs-002-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zvs-002-cll-000  thru cmn-zvs-002-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zvs-002-foi-000  thru cmn-zvs-002-foi-999    .
       acc-cod-s02-110.
           perform   cmn-zvs-002-cll-000  thru cmn-zvs-002-cll-999    .
           if        w-cmn-zvs-002-ope    =    "F+"
                     go to acc-cod-s02-115.
           if        w-cmn-zvs-002-ope    =    "AC"
                     go to acc-cod-s02-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s02-115.
           perform   cmn-zvs-002-foi-000  thru cmn-zvs-002-foi-999    .
           go to     acc-cod-s02-110.
       acc-cod-s02-120.
           move      w-cmn-zvs-002-cod    to   v-num                  .
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
           move      02                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zvs-des    to   w-tes-cod-s02-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs2-000      thru vis-des-cs2-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvs-flg    not  = spaces
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
           move      13                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
           move      37                   to   v-pos                  .
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
           move      "AC"                 to   w-cmn-zvs-003-ope      .
           move      w-tes-cod-s03 (1)    to   w-cmn-zvs-003-cod      .
           move      15                   to   w-cmn-zvs-003-lin      .
           move      30                   to   w-cmn-zvs-003-pos      .
           move      15                   to   w-cmn-zvs-003-dln      .
           move      37                   to   w-cmn-zvs-003-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           perform   cmn-zvs-003-cll-000  thru cmn-zvs-003-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cmn-zvs-003-foi-000  thru cmn-zvs-003-foi-999    .
       acc-cod-s03-110.
           perform   cmn-zvs-003-cll-000  thru cmn-zvs-003-cll-999    .
           if        w-cmn-zvs-003-ope    =    "F+"
                     go to acc-cod-s03-115.
           if        w-cmn-zvs-003-ope    =    "AC"
                     go to acc-cod-s03-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-s03-115.
           perform   cmn-zvs-003-foi-000  thru cmn-zvs-003-foi-999    .
           go to     acc-cod-s03-110.
       acc-cod-s03-120.
           move      w-cmn-zvs-003-cod    to   v-num                  .
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
           move      03                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-zvs-des    to   w-tes-cod-s03-des (1)  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-cs3-000      thru vis-des-cs3-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zvs-flg    not  = spaces
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
           move      15                   to   v-lin                  .
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
           move      15                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-tes-cod-s03-des (1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cs3-999.
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
       cnt-tdo-nok-100.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione                        *
      *              *-------------------------------------------------*
           if        w-tes-des-flt (1)    not  = spaces
                     go to cnt-tdo-nok-200.
           if        w-tes-cod-flt        not  = 9999999
                     go to cnt-tdo-nok-200.
           move      "Manca la descrizione per il filtro"
                                          to   w-err-tdo-nok-msg      .
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
                                          to   w-err-tdo-nok-msg      .
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
                                          to   w-err-tdo-nok-msg      .
           go to     cnt-tdo-nok-900.
       cnt-tdo-nok-400.
      *              *-------------------------------------------------*
      *              * Controllo su descrizione minima e massima       *
      *              *-------------------------------------------------*
           if        w-tes-des-max (1)    =    spaces
                     go to cnt-tdo-nok-800.
           if        w-tes-des-max (1)    not  < w-tes-des-min (1)
                     go to cnt-tdo-nok-800.
           move      "Errore tra i valori minimo e massimo della descriz
      -              "ione"
                                          to   w-err-tdo-nok-msg      .
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
           move      zero                 to   w-tes-tip-mtv (1)      .
           move      spaces               to   w-tes-umi-gst (1)      .
           move      zero                 to   w-tes-cod-s01 (1)      .
           move      zero                 to   w-tes-cod-s02 (1)      .
           move      zero                 to   w-tes-cod-s03 (1)      .
           move      spaces               to   w-tes-umi-gst-des (1)  .
           move      spaces               to   w-tes-cod-s01-des (1)  .
           move      spaces               to   w-tes-cod-s02-des (1)  .
           move      spaces               to   w-tes-cod-s03-des (1)  .
      *              *-------------------------------------------------*
      *              * Normalizzazione dati per area specifica 'mtv'   *
      *              *-------------------------------------------------*
           move      zero                 to   w-zos-mtv-tip-ord      .
           move      zero                 to   w-zos-mtv-cla-min      .
           move      zero                 to   w-zos-mtv-cla-max      .
           move      zero                 to   w-zos-mtv-gru-min      .
           move      zero                 to   w-zos-mtv-gru-max      .
           move      zero                 to   w-zos-mtv-sgr-min      .
           move      zero                 to   w-zos-mtv-sgr-max      .
           move      spaces               to   w-zos-mtv-alf-min      .
           move      spaces               to   w-zos-mtv-alf-max      .
           move      spaces               to   w-zos-mtv-des-min      .
           move      spaces               to   w-zos-mtv-des-max      .
           move      zero                 to   w-zos-mtv-tip-mtv      .
           move      spaces               to   w-zos-mtv-umi-gst      .
           move      zero                 to   w-zos-mtv-cod-s01      .
           move      zero                 to   w-zos-mtv-cod-s02      .
           move      zero                 to   w-zos-mtv-cod-s03      .
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
           move      rf-zos-ide-dat       to   w-tes-ide-dat (1)      .
           move      rf-zos-ide-ute       to   w-tes-ide-ute (1)      .
           move      rf-zos-ide-fas       to   w-tes-ide-fas (1)      .
           move      rf-zos-cod-mne       to   w-tes-cod-mne (1)      .
           move      rf-zos-des-key       to   w-tes-des-key (1)      .
           move      rf-zos-des-flt       to   w-tes-des-flt (1)      .
      *                          *-------------------------------------*
      *                          * Area dati in work di trattamento    *
      *                          *-------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-mtv              .
           move      w-zos-mtv-tip-ord    to   w-tes-tip-ord (1)      .
           move      w-zos-mtv-cla-min    to   w-tes-cla-min (1)      .
           move      w-zos-mtv-cla-max    to   w-tes-cla-max (1)      .
           move      w-zos-mtv-gru-min    to   w-tes-gru-min (1)      .
           move      w-zos-mtv-gru-max    to   w-tes-gru-max (1)      .
           move      w-zos-mtv-sgr-min    to   w-tes-sgr-min (1)      .
           move      w-zos-mtv-sgr-max    to   w-tes-sgr-max (1)      .
           move      w-zos-mtv-alf-min    to   w-tes-cod-min (1)      .
           move      w-zos-mtv-alf-max    to   w-tes-cod-max (1)      .
           move      w-zos-mtv-des-min    to   w-tes-des-min (1)      .
           move      w-zos-mtv-des-max    to   w-tes-des-max (1)      .
           move      w-zos-mtv-tip-mtv    to   w-tes-tip-mtv (1)      .
           if        w-tes-tip-mtv (1)    =    00
                     move  99             to   w-tes-tip-mtv (1)      .
           move      w-zos-mtv-umi-gst    to   w-tes-umi-gst (1)      .
           move      w-zos-mtv-cod-s01    to   w-tes-cod-s01 (1)      .
           move      w-zos-mtv-cod-s02    to   w-tes-cod-s02 (1)      .
           move      w-zos-mtv-cod-s03    to   w-tes-cod-s03 (1)      .
      *                          *-------------------------------------*
      *                          * Valori contenuti indirettamente in  *
      *                          * record [zos]                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [zum]          *
      *                              *---------------------------------*
           move      w-tes-umi-gst (1)    to   w-let-arc-zum-cod      .
           perform   let-arc-zum-000      thru let-arc-zum-999        .
           move      w-let-arc-zum-des    to   w-tes-umi-gst-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zvs]          *
      *                              *---------------------------------*
           move      01                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s01 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
           move      w-let-arc-zvs-des    to   w-tes-cod-s01-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zvs]          *
      *                              *---------------------------------*
           move      02                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s02 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
           move      w-let-arc-zvs-des    to   w-tes-cod-s02-des (1)  .
      *                              *---------------------------------*
      *                              * Lettura archivio [zvs]          *
      *                              *---------------------------------*
           move      03                   to   w-let-arc-zvs-tip      .
           move      w-tes-cod-s03 (1)    to   w-let-arc-zvs-cod      .
           perform   let-arc-zvs-000      thru let-arc-zvs-999        .
           move      w-let-arc-zvs-des    to   w-tes-cod-s03-des (1)  .
      *                      *-----------------------------------------*
      *                      * Valori precedenti anagrafica            *
      *                      *-----------------------------------------*
           move      w-tes-val-aep (1)    to   w-tes-val-aep (2)      .
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
           if        w-tes-cod-mne (1)    not  = spaces
                     go to pos-cnf-ins-999.
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
           move      spaces               to   w-zos-mtv              .
           move      w-tes-tip-ord (1)    to   w-zos-mtv-tip-ord      .
           move      w-tes-cla-min (1)    to   w-zos-mtv-cla-min      .
           move      w-tes-cla-max (1)    to   w-zos-mtv-cla-max      .
           move      w-tes-gru-min (1)    to   w-zos-mtv-gru-min      .
           move      w-tes-gru-max (1)    to   w-zos-mtv-gru-max      .
           move      w-tes-sgr-min (1)    to   w-zos-mtv-sgr-min      .
           move      w-tes-sgr-max (1)    to   w-zos-mtv-sgr-max      .
           move      w-tes-cod-min (1)    to   w-zos-mtv-alf-min      .
           move      w-tes-cod-max (1)    to   w-zos-mtv-alf-max      .
           move      w-tes-des-min (1)    to   w-zos-mtv-des-min      .
           move      w-tes-des-max (1)    to   w-zos-mtv-des-max      .
           move      w-tes-tip-mtv (1)    to   w-zos-mtv-tip-mtv      .
           if        w-zos-mtv-tip-mtv    =    99
                     move  00             to   w-zos-mtv-tip-mtv      .
           move      w-tes-umi-gst (1)    to   w-zos-mtv-umi-gst      .
           move      w-tes-cod-s01 (1)    to   w-zos-mtv-cod-s01      .
           move      w-tes-cod-s02 (1)    to   w-zos-mtv-cod-s02      .
           move      w-tes-cod-s03 (1)    to   w-zos-mtv-cod-s03      .
           move      w-zos-mtv            to   rf-zos-dat-flt         .
       cmp-rec-zos-999.
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
           move      13                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      68                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      48                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-err-tdo-nok-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      64                   to   v-pos                  .
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
           move      65                   to   v-pos                  .
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
      *    * Routine di lettura archivio [zvs]                         *
      *    *-----------------------------------------------------------*
       let-arc-zvs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice statistico a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-zvs-cod    =    zero
                     go to let-arc-zvs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLS    "         to   f-key                  .
           move      w-let-arc-zvs-tip    to   rf-zvs-tip-cls         .
           move      w-let-arc-zvs-cod    to   rf-zvs-cod-cls         .
           move      "pgm/mtv/fls/ioc/obj/iofzvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zvs-400.
       let-arc-zvs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zvs-des-cls       to   w-let-arc-zvs-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zvs-999.
       let-arc-zvs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zvs-flg      .
           move      all   "."            to   w-let-arc-zvs-des      .
           go to     let-arc-zvs-999.
       let-arc-zvs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvs-des      .
       let-arc-zvs-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice filtro ordina-  *
      *    * mento e selezione per file [mtv]                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/azosmtv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice materia varia         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv10.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice gruppo          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv20.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice sottogruppo     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv30.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione unita' di misura           *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acodzum0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 1    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs1.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 2    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs2.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice statistico 3    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzvs3.acs"                   .

