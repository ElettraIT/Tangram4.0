       Identification Division.
       Program-Id.                                 cnvthsc            .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    tmi                 *
      *                        Area gestionale:    tmi                 *
      *                                Settore:                        *
      *                                   Fase:    cnvthsc             *
      *                    ------------------------------------------- *
      *                       Versione attuale:    001 del 08/07/92    *
      *                    ------------------------------------------- *
      *                                 Autore:    Alter s.r.l. - PD   *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione archivio [hsc]                  *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     Alter-Srl-PD .
       Object-Computer.     Alter-Srl-PD .

       Special-Names.       Decimal-Point is comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [old]                                        *
      *    *-----------------------------------------------------------*
           select  optional  old   assign to disk           o-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is old-k01
                   alternate record key   is old-k02
                   alternate record key   is old-k03
                             file status  is                o-sts     .

      *    *===========================================================*
      *    * File Control [new]                                        *
      *    *-----------------------------------------------------------*
           select  optional  new   assign to disk           n-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is new-k01
                   alternate record key   is new-k02
                   alternate record key   is new-k03
                   alternate record key   is new-k04
                   alternate record key   is new-k05
                             file status  is                n-sts     .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [old]                                    *
      *    *-----------------------------------------------------------*
       fd  old       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  old-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  old-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CLIPRO                         *
      *            *---------------------------------------------------*
               10  old-k01.
                   15  old-cod-cli        pic  9(07)       comp-3     .
                   15  old-dpz-cli        pic  x(04)                  .
                   15  old-num-prg        pic  9(05)       comp-3     .
                   15  old-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  old-k02.
                   15  old-ide-dat        pic  9(07)       comp-3     .
                   15  old-cod-cli-2      pic  9(07)       comp-3     .
                   15  old-dpz-cli-2      pic  x(04)                  .
                   15  old-num-prg-2      pic  9(05)       comp-3     .
                   15  old-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PROCLI                         *
      *            *---------------------------------------------------*
               10  old-k03.
                   15  old-num-pro-3      pic  9(07)       comp-3     .
                   15  old-num-prg-3      pic  9(05)       comp-3     .
                   15  old-cod-cli-3      pic  9(07)       comp-3     .
                   15  old-dpz-cli-3      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  old-dat.
               10  old-ide-ute            pic  x(08)                  .
               10  old-ide-fas            pic  x(06)                  .
               10  old-qta-pro            pic  9(05)       comp-3     .
               10  old-sgl-vlt            pic  x(03)                  .
               10  old-dec-vlt            pic  9(01)                  .
               10  old-cdc-vlt            pic  9(06)v9(05) comp-3     .
               10  old-tdc-vlt            pic  x(01)                  .
               10  old-prz-ven            pic  9(09)       comp-3     .
               10  old-dat-doc            pic  9(07)       comp-3     .
               10  old-num-doc            pic  x(10)                  .
               10  old-key-acc.
                   15  old-key-num occurs 03
                                          pic  x(12)                  .
               10  old-com-gsc            pic  x(20)                  .
               10  old-flg-pul            pic  x(01)                  .
               10  old-cod-old            pic  9(05)       comp-3     .
               10  old-alx-exp.
                   15  filler occurs  20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [new]                                    *
      *    *-----------------------------------------------------------*
       fd  new       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  new-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  new-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CLIPRO                         *
      *            *---------------------------------------------------*
               10  new-k01.
                   15  new-cod-cli        pic  9(07)       comp-3     .
                   15  new-dpz-cli        pic  x(04)                  .
                   15  new-num-prg        pic  9(05)       comp-3     .
                   15  new-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  new-k02.
                   15  new-ide-dat        pic  9(07)       comp-3     .
                   15  new-cod-cli-2      pic  9(07)       comp-3     .
                   15  new-dpz-cli-2      pic  x(04)                  .
                   15  new-num-prg-2      pic  9(05)       comp-3     .
                   15  new-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PROCLI                         *
      *            *---------------------------------------------------*
               10  new-k03.
                   15  new-num-pro-3      pic  9(07)       comp-3     .
                   15  new-num-prg-3      pic  9(05)       comp-3     .
                   15  new-cod-cli-3      pic  9(07)       comp-3     .
                   15  new-dpz-cli-3      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : FATCPR                         *
      *            *---------------------------------------------------*
               10  new-k04.
                   15  new-prt-fat        pic  9(09)       comp-3     .
                   15  new-cod-cli-4      pic  9(07)       comp-3     .
                   15  new-dpz-cli-4      pic  x(04)                  .
                   15  new-num-prg-4      pic  9(05)       comp-3     .
                   15  new-num-pro-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DDSCPR                         *
      *            *---------------------------------------------------*
               10  new-k05.
                   15  new-prt-dds        pic  9(09)       comp-3     .
                   15  new-cod-cli-5      pic  9(07)       comp-3     .
                   15  new-dpz-cli-5      pic  x(04)                  .
                   15  new-num-prg-5      pic  9(05)       comp-3     .
                   15  new-num-pro-5      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  new-dat.
               10  new-ide-ute            pic  x(08)                  .
               10  new-ide-fas            pic  x(06)                  .
               10  new-qta-pro            pic  9(05)       comp-3     .
               10  new-sgl-vlt            pic  x(03)                  .
               10  new-dec-vlt            pic  9(01)                  .
               10  new-cdc-vlt            pic  9(06)v9(05) comp-3     .
               10  new-tdc-vlt            pic  x(01)                  .
               10  new-prz-ven            pic  9(09)       comp-3     .
               10  new-dat-doc            pic  9(07)       comp-3     .
               10  new-num-doc            pic  x(10)                  .
               10  new-key-acc.
                   15  new-key-num occurs 03
                                          pic  x(12)                  .
               10  new-com-gsc            pic  x(20)                  .
               10  new-flg-pul            pic  x(01)                  .
               10  new-cod-old            pic  9(05)       comp-3     .
               10  new-alx-exp.
                   15  filler occurs  20  pic  x(01)                  .

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
                     "tmi"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "tmi"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "   "                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cnvhsc"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnvthsc "                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "         CONVERSIONE FILE [hsc]         "       .

      *    *===========================================================*
      *    * File area per [old]                                       *
      *    *-----------------------------------------------------------*
       01  o.
           05  o-nam                      pic  x(04)                  .
           05  o-pat                      pic  x(40)                  .
           05  o-sts                      pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [new]                                       *
      *    *-----------------------------------------------------------*
       01  n.
           05  n-nam                      pic  x(04)                  .
           05  n-pat                      pic  x(40)                  .
           05  n-sts                      pic  x(02)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
           05  w-cnt-flg.
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
               10  w-cnt-ric-cnf-exe      pic  x(01)                  .
               10  w-cnt-exe-cnv-fil      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area contatori                                      *
      *    *-----------------------------------------------------------*
       01  w-ctr-rec.
           05  w-ctr-rec-old              pic  9(09)                  .
           05  w-ctr-rec-new              pic  9(09)                  .

      *    *===========================================================*
      *    * Work-area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
           05  w-wrk-cod-mne.
               10  w-wrk-mne-003          pic  x(03)                  .
               10  w-wrk-mne-007          pic  x(07)                  .
           05  w-wrk-new-mne              pic  x(10)                  .
           05  w-wrk-des-cfl              pic  x(30)                  .

      *    *===========================================================*
      *    * Work-area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione ragione sociale in chiave          *
      *        *-------------------------------------------------------*
           05  w-det-rag-key.
      *            *---------------------------------------------------*
      *            * Stringa originale                                 *
      *            *---------------------------------------------------*
               10  w-det-rag-key-str.
                   15  w-det-rag-key-chr  occurs 76
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Stringa in uscita                                 *
      *            *---------------------------------------------------*
               10  w-det-rag-key-nst.
                   15  w-det-rag-key-nch  occurs 76
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * contatori                                         *
      *            *---------------------------------------------------*
               10  w-det-rag-key-ct1      pic  9(02)                  .
               10  w-det-rag-key-ct2      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .

      *    *===========================================================*
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Richiesta conferma esecuzione                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-ric-cnf-exe      .
           perform   ric-cnf-exe-000      thru ric-cnf-exe-999        .
           if        w-cnt-ric-cnf-exe    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine di conversione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-exe-cnv-fil      .
           perform   exe-cnv-fil-000      thru exe-cnv-fil-999        .
           if        w-cnt-exe-cnv-fil    not  = spaces
                     go to main-900.
       main-900.
      *              *-------------------------------------------------*
      *              * Box di conferma dati letti e scritti            *
      *              *-------------------------------------------------*
           perform   box-end-prg-000      thru box-end-prg-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
           move      "Q+"                 to   s-ope                  .
           move      i-ide-sap            to   s-sap                  .
           move      i-ide-arg            to   s-arg                  .
           move      i-ide-set            to   s-set                  .
           move      i-ide-fas            to   s-fas                  .
           move      i-ide-pro            to   s-pro                  .
           move      "S"                  to   s-svv                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
           move      "P-"                 to   s-ope                  .
           move      i-ide-pro            to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
      *    * Richiesta conferma esecuzione                             *
      *    *-----------------------------------------------------------*
       ric-cnf-exe-000.
      *              *-------------------------------------------------*
      *              * Maschera per pathnames                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pathname del file in input  (old) :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pathname del file in output (new) :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione pathnames                       *
      *              *-------------------------------------------------*
           move      spaces               to   o-pat                  .
           move      spaces               to   n-pat                  .
       ric-cnf-exe-200.
      *              *-------------------------------------------------*
      *              * Accettazione pathnames                          *
      *              *-------------------------------------------------*
       ric-cnf-exe-300.
      *                  *---------------------------------------------*
      *                  * Pathname file in input  (old)               *
      *                  *---------------------------------------------*
       ric-cnf-exe-310.
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      o-pat                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   o-pat                  .
       ric-cnf-exe-320.
           if        v-key                =    "EXIT"
                     move  "#"            to   w-cnt-ric-cnf-exe
                     go to ric-cnf-exe-999.
       ric-cnf-exe-330.
           if        v-key                not  = "DO  "
                     go to ric-cnf-exe-340.
           if        o-pat                =    spaces or
                     n-pat                =    spaces
                     go to ric-cnf-exe-310.
           if        o-pat                =    n-pat
                     go to ric-cnf-exe-310.
           go to     ric-cnf-exe-900.
       ric-cnf-exe-340.
           if        o-pat                =    spaces
                     go to ric-cnf-exe-310.
           go to     ric-cnf-exe-400.
       ric-cnf-exe-400.
      *                  *---------------------------------------------*
      *                  * Pathname file in output (new)               *
      *                  *---------------------------------------------*
           if        n-pat                =    spaces
                     move  o-pat          to   n-pat                  .
       ric-cnf-exe-410.
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      n-pat                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   n-pat                  .
       ric-cnf-exe-420.
           if        v-key                =    "EXIT"
                     move  "#"            to   w-cnt-ric-cnf-exe
                     go to ric-cnf-exe-999.
       ric-cnf-exe-430.
           if        v-key                not  = "DO  "
                     go to ric-cnf-exe-440.
           if        o-pat                =    spaces or
                     n-pat                =    spaces
                     go to ric-cnf-exe-410.
           if        o-pat                =    n-pat
                     go to ric-cnf-exe-410.
           go to     ric-cnf-exe-900.
       ric-cnf-exe-440.
           if        v-key                =    "UP  "
                     go to ric-cnf-exe-300.
           if        n-pat                =    spaces
                     go to ric-cnf-exe-410.
           if        n-pat                =    o-pat
                     go to ric-cnf-exe-410.
           go to     ric-cnf-exe-800.
       ric-cnf-exe-800.
      *              *-------------------------------------------------*
      *              * Conferma finale                                 *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma esecuzione (S/N/E) ?"
                                          to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to ric-cnf-exe-840.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       ric-cnf-exe-840.
      *              *-------------------------------------------------*
      *              * Test su risposta dell'utente                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     go to ric-cnf-exe-900
           else if   v-key                =    "EXIT"
                     move  "#"            to   w-cnt-ric-cnf-exe
                     go to ric-cnf-exe-999
           else if   v-key                =    "UP  "
                     go to ric-cnf-exe-200
           else      go to ric-cnf-exe-800.
       ric-cnf-exe-900.
      *              *-------------------------------------------------*
      *              * Messaggio programma in esecuzione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Records letti da file in input    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Records scritti su file in output :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       ric-cnf-exe-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione routine di conversione                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fil-000.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura file [gxc]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Apertura files old e new                    *
      *                  *---------------------------------------------*
           open      i-o    old                                       .
           open      i-o    new                                       .
      *              *-------------------------------------------------*
      *              * Azzeramento contatori records                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr-rec-old          .
           move      zero                 to   w-ctr-rec-new          .
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   old-k01                .
           start     old    key not less
                            old-k01
                            invalid key
                            go to exe-cnv-fil-900.
       exe-cnv-fil-200.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      old    next
                            with no lock
                            at end
                            go to exe-cnv-fil-900.
      *              *-------------------------------------------------*
      *              * Incremento contatore records in input           *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr-rec-old          .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      14                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-ctr-rec-old        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-cnv-fil-300.
      *              *-------------------------------------------------*
      *              * Spaces in tutto il record di output             *
      *              *-------------------------------------------------*
           move      spaces               to   new-rec                .
       exe-cnv-fil-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      old-cod-cli          to   new-cod-cli            .
           move      old-dpz-cli          to   new-dpz-cli            .
           move      old-num-prg          to   new-num-prg            .
           move      old-num-pro          to   new-num-pro            .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      old-ide-dat          to   new-ide-dat            .
           move      old-cod-cli          to   new-cod-cli-2          .
           move      old-dpz-cli          to   new-dpz-cli-2          .
           move      old-num-prg          to   new-num-prg-2          .
           move      old-num-pro          to   new-num-pro-2          .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      old-num-pro          to   new-num-pro-3          .
           move      old-num-prg          to   new-num-prg-3          .
           move      old-cod-cli          to   new-cod-cli-3          .
           move      old-dpz-cli          to   new-dpz-cli-3          .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      zero                 to   new-prt-fat            .
           move      old-cod-cli          to   new-cod-cli-4          .
           move      old-dpz-cli          to   new-dpz-cli-4          .
           move      old-num-prg          to   new-num-prg-4          .
           move      old-num-pro          to   new-num-pro-4          .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      zero                 to   new-prt-dds            .
           move      old-cod-cli          to   new-cod-cli-5          .
           move      old-dpz-cli          to   new-dpz-cli-5          .
           move      old-num-prg          to   new-num-prg-5          .
           move      old-num-pro          to   new-num-pro-5          .
      *              *-------------------------------------------------*
      *              * Composizione dati                               *
      *              *-------------------------------------------------*
           move      old-ide-ute          to   new-ide-ute            .
           move      old-ide-fas          to   new-ide-fas            .
           move      old-qta-pro          to   new-qta-pro            .
           move      old-sgl-vlt          to   new-sgl-vlt            .
           move      old-dec-vlt          to   new-dec-vlt            .
           move      old-cdc-vlt          to   new-cdc-vlt            .
           move      old-tdc-vlt          to   new-tdc-vlt            .
           move      old-prz-ven          to   new-prz-ven            .
           move      old-dat-doc          to   new-dat-doc            .
           move      old-num-doc          to   new-num-doc            .
           move      old-key-acc          to   new-key-acc            .
           move      old-com-gsc          to   new-com-gsc            .
           move      old-flg-pul          to   new-flg-pul            .
           move      old-cod-old          to   new-cod-old            .
           move      old-alx-exp          to   new-alx-exp            .
       exe-cnv-fil-600.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           write     new-rec invalid key
                             go to exe-cnv-fil-800.
       exe-cnv-fil-700.
      *              *-------------------------------------------------*
      *              * Incremento contatore records in output          *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr-rec-new          .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-ctr-rec-new        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-cnv-fil-800.
      *              *-------------------------------------------------*
      *              * Riciclo su input                                *
      *              *-------------------------------------------------*
           go to     exe-cnv-fil-200.
       exe-cnv-fil-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [gxc]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Close files old e new                       *
      *                  *---------------------------------------------*
           close     old                                              .
           close     new                                              .
       exe-cnv-fil-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di fine esecuzione programma            *
      *    *-----------------------------------------------------------*
       box-end-prg-000.
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
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      23                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Numero record letti   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      11                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-ctr-rec-old        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      23                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Numero record scritti :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-ctr-rec-new        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal per presa visione                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione 'OK' di presa visione              *
      *              *-------------------------------------------------*
           move      spaces               to   v-alf                  .
       box-end-prg-100.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(20)              .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT"
                     go to box-end-prg-900.
           if        v-alf                not  = "OK"
                     go to box-end-prg-100.
       box-end-prg-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-end-prg-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione ragione sociale in chiave       *
      *    *-----------------------------------------------------------*
      *    * In input       : w-det-rag-key-str = Stringa originale    *
      *    *                                                           *
      *    * In output      : w-det-rag-key-nst = Stringa in uscita    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-rag-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-rag-key-nst      .
      *              *-------------------------------------------------*
      *              * Test iniziali                                   *
      *              *-------------------------------------------------*
           if        w-det-rag-key-str    =    spaces
                     go to det-rag-key-999.
      *              *-------------------------------------------------*
      *              * Inizio ciclo di scansione                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rag-key-ct1      .
           move      zero                 to   w-det-rag-key-ct2      .
       det-rag-key-100.
           add       1                    to   w-det-rag-key-ct1      .
      *                  *---------------------------------------------*
      *                  * Test sul massimo contatore 1                *
      *                  *---------------------------------------------*
           if        w-det-rag-key-ct1    >    76
                     go to det-rag-key-999.
      *                  *---------------------------------------------*
      *                  * Test di confronto con caratteri             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Traduzione di '&' commerciale           *
      *                      *-----------------------------------------*
           if        w-det-rag-key-chr
                    (w-det-rag-key-ct1)   =    "&"
                     move  "E"            to   w-det-rag-key-chr
                                              (w-det-rag-key-ct1)
                     go to det-rag-key-300.
      *                      *-----------------------------------------*
      *                      * Test su caratteri alfabetici            *
      *                      *-----------------------------------------*
           if        w-det-rag-key-chr
                    (w-det-rag-key-ct1)   <    "A"  or
                     w-det-rag-key-chr
                    (w-det-rag-key-ct1)   >    "Z"
                     go to det-rag-key-200.
           go to     det-rag-key-300.
       det-rag-key-200.
      *                      *-----------------------------------------*
      *                      * Test su caratteri numerici              *
      *                      *-----------------------------------------*
           if        w-det-rag-key-chr
                    (w-det-rag-key-ct1)   <    "0"  or
                     w-det-rag-key-chr
                    (w-det-rag-key-ct1)   >    "9"
                     go to det-rag-key-100.
           go to     det-rag-key-300.
       det-rag-key-300.
      *                  *---------------------------------------------*
      *                  * Composizione della nuova stringa            *
      *                  *---------------------------------------------*
           add       1                    to   w-det-rag-key-ct2      .
      *                  *---------------------------------------------*
      *                  * Test sul massimo contatore 2                *
      *                  *---------------------------------------------*
           if        w-det-rag-key-ct2    >    76
                     go to det-rag-key-999.
           move      w-det-rag-key-chr
                    (w-det-rag-key-ct1)   to   w-det-rag-key-nch
                                              (w-det-rag-key-ct2)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-rag-key-100.
       det-rag-key-999.
           exit.

