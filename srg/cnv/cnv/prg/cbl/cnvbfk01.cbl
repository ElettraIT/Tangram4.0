       Identification Division.
       Program-Id.                                 cnvbfk01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    arc                 *
      *                                   Fase:    cnvbfk              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/04/23    *
      *                       Ultima revisione:    NdK del 30/04/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione archivio [bfk]                  *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [old] bfk                                    *
      *    *-----------------------------------------------------------*
           select  optional  old   assign to disk           o-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is old-k01
                   alternate record key   is old-k02
                             file status  is                o-sts     .

      *    *===========================================================*
      *    * File Control [new] bfk                                    *
      *    *-----------------------------------------------------------*
           select  optional  new   assign to disk           n-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is new-k01
                   alternate record key   is new-k02
                   alternate record key   is new-k03
                   alternate record key   is new-k04
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
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  old-k01.
                   15  old-num-prt        pic  9(11)       comp-3     .
                   15  old-num-prg        pic  9(05)       comp-3     .
                   15  old-num-prc        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRTPRC                         *
      *            *---------------------------------------------------*
               10  old-k02.
                   15  old-num-prt-2      pic  9(11)       comp-3     .
                   15  old-num-prc-2      pic  9(05)       comp-3     .
                   15  old-num-prg-2      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  old-dat.
               10  old-cts-prc            pic  x(20)                  .
               10  old-tip-prc            pic  9(05)       comp-3     .
               10  old-qta-prc            pic  9(08)v9(03) comp-3     .
               10  old-lrd-prc            pic  9(08)v9(03) comp-3     .
               10  old-tar-prc            pic  9(08)v9(03) comp-3     .
               10  old-vol-prc            pic  9(08)v9(03) comp-3     .
               10  old-dim-lar            pic  9(06)v9(03) comp-3     .
               10  old-dim-alt            pic  9(06)v9(03) comp-3     .
               10  old-dim-prf            pic  9(06)v9(03) comp-3     .
               10  old-dat-prc            pic  9(07)                  .
               10  old-not-prc            pic  x(40)                  .
               10  old-alx-exp.
                   15  filler occurs 200  pic  9(01)                  .

      *    *===========================================================*
      *    * File Description [new]                                    *
      *    *-----------------------------------------------------------*
       fd  new       label record standard                            .
       01  new-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  new-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  new-k01.
                   15  new-num-prt        pic  9(11)       comp-3     .
                   15  new-num-prg        pic  9(05)       comp-3     .
                   15  new-num-prc        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRTPRC                         *
      *            *---------------------------------------------------*
               10  new-k02.
                   15  new-num-prt-2      pic  9(11)       comp-3     .
                   15  new-num-prc-2      pic  9(05)       comp-3     .
                   15  new-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CTSPRC                         *
      *            *---------------------------------------------------*
               10  new-k03.
                   15  new-cts-prc        pic  x(20)                  .
                   15  new-num-prt-3      pic  9(11)       comp-3     .
                   15  new-num-prg-3      pic  9(05)       comp-3     .
                   15  new-num-prc-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PROCTS                         *
      *            *---------------------------------------------------*
               10  new-k04.
                   15  new-num-mag        pic  9(07)       comp-3     .
                   15  new-cts-prc-4      pic  x(20)                  .
                   15  new-num-prt-4      pic  9(11)       comp-3     .
                   15  new-num-prg-4      pic  9(05)       comp-3     .
                   15  new-num-prc-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  new-dat.
               10  new-qta-prc            pic  9(08)v9(03) comp-3     .
               10  new-alx-exp.
                   15  filler occurs 80   pic  9(01)                  .

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
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "arc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cnvbfk"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnvbfk01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "       Conversione archivio [bfk]       "       .

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
      *    * Area di comunicazione per modulo                "mopsys"  *
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
      *    * Records logici                                            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .

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
           move      "/abd/azi/ele/bfk.old"
                                          to   o-pat                  .
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
      *    * Esecuzione routine di conversione                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fil-000.
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
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
      *              * Normalizzazione [bfr]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Lettura record [bfr]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      old-num-prt          to   rf-bfr-num-prt         .
           move      old-num-prg          to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
           move      old-num-prt          to   new-num-prt            .
           move      old-num-prg          to   new-num-prg            .
           move      old-num-prc          to   new-num-prc            .
           move      old-cts-prc          to   new-cts-prc            .
           move      old-qta-prc          to   new-qta-prc            .
           move      rf-bfr-num-mag       to   new-num-mag            .
           move      old-alx-exp          to   new-alx-exp            .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      old-num-prt          to   new-num-prt            .
           move      old-num-prg          to   new-num-prg            .
           move      old-num-prc          to   new-num-prc            .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      old-num-prt          to   new-num-prt-2          .
           move      old-num-prc          to   new-num-prc-2          .
           move      old-num-prg          to   new-num-prg-2          .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      old-cts-prc          to   new-cts-prc            .
           move      old-num-prt          to   new-num-prt-3          .
           move      old-num-prg          to   new-num-prg-3          .
           move      old-num-prc          to   new-num-prc-3          .
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      rf-bfr-num-mag       to   new-num-mag            .
           move      old-cts-prc          to   new-cts-prc-4          .
           move      old-num-prt          to   new-num-prt-4          .
           move      old-num-prg          to   new-num-prg-4          .
           move      old-num-prc          to   new-num-prc-4          .
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
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           close     old                                              .
           close     new                                              .
      *              *-------------------------------------------------*
      *              * Messaggio di fine programma in esecuzione       *
      *              *-------------------------------------------------*
           move      "FE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-cnv-fil-999.
           exit.
