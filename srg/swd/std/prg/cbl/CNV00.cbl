       Identification Division.
       Program-Id.                                 cnv___             .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    arc                 *
      *                                   Fase:    cnv___              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del __/__/__    *
      *                       Ultima revisione:    NdK del __/__/__    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione archivio [___]                  *
      *                                                                *
      *                    N.B.: i nomi file devono essere completati  *
      *                          con il percorso dell'azienda !        *
      *                                                                *
      *                          Esempio: /abd/azi/prv/xxx.old         *
      *                                   /abd/azi/prv/xxx.new         *
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
      *    * File Control [old] ___                                    *
      *    *-----------------------------------------------------------*
           select  optional  old   assign to disk           o-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is old-k01
                   alternate record key   is old-k02
                        with duplicates
                   alternate record key   is old-k03
                        with duplicates
                   alternate record key   is old-k04
                        with duplicates
                             file status  is                o-sts     .

      *    *===========================================================*
      *    * File Control [new] ___                                    *
      *    *-----------------------------------------------------------*
           select  optional  new   assign to disk           n-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is new-k01
                   alternate record key   is new-k02
                   alternate record key   is new-k03
                   alternate record key   is new-k04
                   alternate record key   is new-k05
                   alternate record key   is new-k06
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
       01  old-rec.
           05  old-key.
               10  old-k01.
                   15  old-cod-cli        pic  9(07)       comp-3     .
               10  old-k02.
                   15  old-rag-key        pic  x(20)                  .
               10  old-k03.
                   15  old-cod-mne        pic  x(10)                  .
               10  old-k04.
                   15  old-prt-iva        pic  9(11)       comp-3     .
           05  old-dat.
               10  old-rag-soc            pic  x(40)                  .
               10  old-via-cli            pic  x(30)                  .
               10  old-loc-cli            pic  x(30)                  .
               10  old-snx-a13            pic  x(01)                  .
               10  old-cod-cge            pic  9(07)       comp-3     .

      *    *===========================================================*
      *    * File Description [new]                                    *
      *    *-----------------------------------------------------------*
       fd  new       label record standard                            .
       01  new-rec.
           05  new-key.
               10  new-k01.
                   15  new-cod-cli        pic  9(07)       comp-3     .
               10  new-k02.
                   15  new-ide-dat        pic  9(07)       comp-3     .
                   15  new-cod-cli-2      pic  9(07)       comp-3     .
               10  new-k03.
                   15  new-rag-key        pic  x(40)                  .
                   15  new-cod-cli-3      pic  9(07)       comp-3     .
               10  new-k04.
                   15  new-cod-mne        pic  x(10)                  .
                   15  new-cod-cli-4      pic  9(07)       comp-3     .
               10  new-k05.
                   15  new-prt-iva        pic  9(11)       comp-3     .
                   15  new-cod-cli-5      pic  9(07)       comp-3     .
               10  new-k06.
                   15  new-cod-fis        pic  x(16)                  .
                   15  new-cod-cli-6      pic  9(07)       comp-3     .
           05  new-dat.
               10  new-ide-ute            pic  x(08)                  .
               10  new-ide-fas            pic  x(06)                  .
               10  new-rag-soc            pic  x(40)                  .
               10  new-via-cli            pic  x(40)                  .
               10  new-loc-cli            pic  x(40)                  .
               10  new-cod-naz            pic  x(03)                  .
               10  new-cap-uff            pic  9(05)       comp-3     .
               10  new-cap-est            pic  9(03)       comp-3     .
               10  new-cap-loc            pic  9(03)       comp-3     .
               10  new-num-tel            pic  x(20)                  .
               10  new-num-fax            pic  x(20)                  .
               10  new-num-tlx            pic  x(20)                  .
               10  new-nom-int            pic  x(30)                  .
               10  new-cod-iva            pic  9(05)       comp-3     .
               10  new-snx-a13            pic  x(01)                  .
               10  new-cod-cge            pic  9(07)       comp-3     .
               10  new-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

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
                     "___"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "___"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "___"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "______"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "p______n"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "________________________________________"       .

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
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*


___________________________


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
           close     old                                              .
           close     new                                              .
       exe-cnv-fil-999.
           exit.
