       Identification Division.
       Program-Id.                                 elebfoe0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    elebfo              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 22/03/23    *
      *                       Ultima revisione:    NdK del 12/04/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Visualizzazione castelletto pacchi          *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      *                    ___ OBSOLETO ___                            *
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bfk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfk"                          .
      
      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-saa-bfo              pic  x(03)                  .
           05  w-exe-num-bfo              pic  x(06)                  .
           05  w-exe-prg-bfo              pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-v01              pic  x(20)                  .
           05  w-exe-prm-v02              pic  x(20)                  .
           05  w-exe-prm-v03              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi generici                                       *
      *        *-------------------------------------------------------*
           05  w-exe-qta-edt              pic  9(11)                  .
           05  w-exe-snx-pcr              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione tag generico               *
      *        *-------------------------------------------------------*
           05  w-exe-tag.
               10  w-exe-tag-tag          pic  x(20)                  .
               10  w-exe-tag-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione tag tabella                *
      *        *-------------------------------------------------------*
           05  w-exe-ttd.
               10  w-exe-ttd-snh          pic  x(01)                  .
               10  w-exe-ttd-tg1          pic  x(20)                  .
               10  w-exe-ttd-tg2          pic  x(30)                  .
               10  w-exe-ttd-txt          pic  x(120)                 .
               10  w-exe-ttd-col          pic  9(02)                  .
               10  w-exe-ttd-cow          pic  x(20)                  .
               10  w-exe-ttd-all          pic  x(01)                  .
               10  w-exe-ttd-alw          pic  x(20)                  .
               10  w-exe-ttd-wdt          pic  x(01)                  .
               10  w-exe-ttd-wdw          pic  x(20)                  .
               10  w-exe-ttd-stl          pic  x(01)                  .
               10  w-exe-ttd-st1          pic  x(25)                  .
               10  w-exe-ttd-st2          pic  x(20)                  .
               10  w-exe-ttd-cli          pic  x(07)                  .
               10  w-exe-ttd-prg          pic  x(05)                  .
               10  w-exe-ttd-prr          pic  x(03)                  .
               10  w-exe-ttd-ub1          pic  x(07)                  .
               10  w-exe-ttd-ub2          pic  x(07)                  .
               10  w-exe-ttd-ub3          pic  x(07)                  .
               10  w-exe-ttd-ub4          pic  x(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-bft.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-nds      pic  9(11)                  .
               10  w-slc-num-bft-nds-r redefines
                   w-slc-num-bft-nds.
                   15  w-slc-num-bft-nsa  pic  9(03)                  .
                   15  w-slc-num-bft-ndp  pic  9(02)                  .
                   15  w-slc-num-bft-npg  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-num      pic  9(11)                  .
               10  w-slc-num-bft-prg      pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      ******************************************************************
       Procedure Division                                             .
      ******************************************************************

      *================================================================*
      * Main                                                           *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           perform   ext-prm-000          thru ext-prm-999            .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   opn-fls-000          thru opn-fls-999            .
       main-100.
      *              *-------------------------------------------------*
      *              * Ciclo di lettura e preparazione html            *
      *              *-------------------------------------------------*
           perform   exe-cph-000          thru exe-cph-999            .
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   cls-fls-000          thru cls-fls-999            .
       main-999.
           exit      program.

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *-----------------------------------------------------------*
       ext-prm-000.
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
      *              * Data di sistema                                 *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * CORRETTIVO PROVVISORIO                          *
      *              *-------------------------------------------------*
           if        w-exe-dat-exe        <    999999
                     add  1000000         to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-prm-v01          .
           move      spaces               to   w-exe-prm-v02          .
           move      spaces               to   w-exe-prm-v03          .
           move      spaces               to   w-exe-saa-bfo          .
           move      spaces               to   w-exe-num-bfo          .
           move      spaces               to   w-exe-prg-bfo          .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione di 3 parametri                       *
      *              *-------------------------------------------------*
           move      o-pst                to   w-all-str-alf          .
           move      "&"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *
           move      w-all-str-cat (1)    to   w-exe-prm-v01          .
           move      w-all-str-cat (2)    to   w-exe-prm-v02          .
           move      w-all-str-cat (3)    to   w-exe-prm-v03          .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Secolo/anno documento                       *
      *                  *---------------------------------------------*
           move      w-exe-prm-v01        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-saa-bfo          .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      w-exe-prm-v02        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-num-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo interno riga documento          *
      *                  *---------------------------------------------*
           move      w-exe-prm-v03        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prg-bfo          .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      01                   to   w-slc-num-bft-ndp      .
      *                  *---------------------------------------------*
      *                  * Anno documento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      03                   to   v-car                  .
           move      w-exe-saa-bfo        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-slc-num-bft-nsa      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      06                   to   v-car                  .
           move      w-exe-num-bfo        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-slc-num-bft-npg      .
      *
           move      w-slc-num-bft-nds    to   w-slc-num-bft-num      .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      05                   to   v-car                  .
           move      w-exe-prg-bfo        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-slc-num-bft-prg      .
       ext-prm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-999.
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * [bfk]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [bfk]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
       
           go to     exe-cph-100.
      *              *-------------------------------------------------*
      *              * DEBUG ___ INATTIVO ___                          *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   o-alx                                            .
           display   "<br>"                                           .
           display   w-slc-num-bft-num                                .
           display   "<br>"                                           .
           display   w-slc-num-bft-prg                                .
           display   "<br>"                                           .
           display   "<br>"                                           .
           display   w-exe-saa-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-num-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-prg-bfo                                    .
           display   "<br>"                                           .

       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Subroutine                                      *
      *              *-------------------------------------------------*
           perform   exe-cph-box-000      thru exe-cph-box-999        .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.


      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per visualizzazione colli in riga              *
      *    *-----------------------------------------------------------*
       exe-cph-box-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-snx-pcr          .
       exe-cph-box-100.
      *              *-------------------------------------------------*
      *              * Start su [bfk]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-bft-num    to   rf-bfk-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfk-num-prg         .
           move      zero                 to   rf-bfk-num-prc         .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : fine lettura           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  exe-cph-box-900.
       exe-cph-box-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [bfk]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
      *                  *---------------------------------------------*
      *                  * Se at end : fine lettura                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  exe-cph-box-900.
       exe-cph-box-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
           if        rf-bfk-num-prt       not  = w-slc-num-bft-num
                     go to  exe-cph-box-900.
           if        rf-bfk-num-prg       not  = w-slc-num-bft-prg
                     go to  exe-cph-box-900.
       exe-cph-box-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfk]                              *
      *              *-------------------------------------------------*
       exe-cph-box-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore elementi                   *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-snx-pcr          .
      *              *-------------------------------------------------*
      *              * Apertura box HTML                               *
      *              *-------------------------------------------------*
           if        w-exe-snx-pcr        >    1
                     go to exe-cph-box-600.
           perform   emi-tst-000          thru emi-tst-999            .
       exe-cph-box-600.
      *              *-------------------------------------------------*
      *              * Linea box HTML                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "O"                  to   w-exe-tag-flg          .
           move      "tr"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
      *                  *---------------------------------------------*
      *                  * Contrassegno                                *
      *                  *---------------------------------------------*
           move      rf-bfk-cts-prc       to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      rf-bfk-qta-prc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "R"                  to   w-exe-ttd-all          .
           move      "20%"                to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "tr"                 to   w-exe-tag-tag          .
           move      "C"                  to   w-exe-tag-flg          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       exe-cph-box-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-box-200.
       exe-cph-box-900.
      *              *-------------------------------------------------*
      *              * Fine corpo                                      *
      *              *-------------------------------------------------*
           display   "</table>"                                       .
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           display   "</body>"                                        .
           display   "</html>"                                        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-box-999.
       exe-cph-box-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-tst-000.
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
______*    display   "<head>"                                         .
______*    display   "<title>Prodotti TANGRAM - ELETTRA</TITLE>"      .
______*    display   "</head>"                                        .
      *              *-------------------------------------------------*
      *              * Corpo                                           *
      *              *-------------------------------------------------*
           display   "<body>"                                         .
      *              *-------------------------------------------------*
      *              * Premessa corpo                                  *
      *              *-------------------------------------------------*
           display   "<table border=1 cellspacing=0 cellpadding=1 class=
      -              "bordotab width=100% >"                          .
       emi-tst-200.
      *              *-------------------------------------------------*
      *              * Prompt tabella                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "O"                  to   w-exe-tag-flg          .
           move      "tr"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Contrassegno                    *
      *                  *---------------------------------------------*
           move      "Contrassegno"       to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Quantita'                       *
      *                  *---------------------------------------------*
           move      "Quantita'"          to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "tr"                 to   w-exe-tag-tag          .
           move      "C"                  to   w-exe-tag-flg          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       emi-tst-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag generica                                    *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-exe-tag-tag = nome del tag                     *
      *    *          w-exe-tag-flg = 'O' o 'C' (Open or Close)        *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa completa                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       emi-htm-tag-000.
      *              *-------------------------------------------------*
      *              * Se testo a spazi                                *
      *              *-------------------------------------------------*
           if        w-exe-tag-tag        =    spaces
                     go to emi-htm-tag-900.
       emi-htm-tag-100.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "<"                  to   w-all-str-cat (1)      .
      *
           if        w-exe-tag-flg        =    "O"
                     move  spaces         to   w-all-str-cat (2)
           else      move  "/"            to   w-all-str-cat (2)      .
      *
           move      w-exe-tag-tag        to   w-all-str-cat (3)      .
           move      ">"                  to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
       emi-htm-tag-300.
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       emi-htm-tag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-tag-999.
       emi-htm-tag-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag 'td' o 'th'                                 *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-exe-ttd-txt = Testo da inserire nel Tag        *
      *    *          w-exe-ttd-col = Numero di colonne occupate       *
      *    *          w-exe-ttd-snh = Si/no header (th o td)           *
      *    *          w-exe-ttd-all = Allineamento (L, C, R)           *
      *    *          w-exe-ttd-wdt = Larghezza (S, M, B)              *
      *    *          w-exe-ttd-stl = Stile (B, N, x)                  *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa compbfta                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       emi-htm-ttd-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione contenuto                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * TAG                                         *
      *                  *---------------------------------------------*
           if        w-exe-ttd-snh        =    spaces or
                     w-exe-ttd-snh        =    "N"
                     move  "<td"          to   w-exe-ttd-tg1
                     move  "</td>"        to   w-exe-ttd-tg2
           else      move  "<th"          to   w-exe-ttd-tg1
                     move  "</th>"        to   w-exe-ttd-tg2          .
      *                  *---------------------------------------------*
      *                  * Testo                                       *
      *                  *---------------------------------------------*
           if        w-exe-ttd-txt        =    spaces
                     move  "&nbsp;"       to   w-exe-ttd-txt          .
      *                  *---------------------------------------------*
      *                  * Larghezza                                   *
      *                  *---------------------------------------------*
           if        w-exe-ttd-wdt        =    "S"
                     move  "width='20%'" 
                                          to   w-exe-ttd-wdw
           else if   w-exe-ttd-all        =    "M"
                     move  "width='40%'" 
                                          to   w-exe-ttd-wdw
           else if   w-exe-ttd-all        =    "B"
                     move  "width='80%'" 
                                          to   w-exe-ttd-wdw
           else      move  spaces         to   w-exe-ttd-wdw          .
      *                      *-----------------------------------------*
      *                      * Attualmente forzata a spazi             *
      *                      *-----------------------------------------*
           move      spaces               to   w-exe-ttd-wdw          .
      *                  *---------------------------------------------*
      *                  * Allineamento                                *
      *                  *---------------------------------------------*
           if        w-exe-ttd-all        =    "L"
                     move  "align='left'" 
                                          to   w-exe-ttd-alw
           else if   w-exe-ttd-all        =    "R"
                     move  "align='right'"
                                          to   w-exe-ttd-alw
           else if   w-exe-ttd-all        =    "C"
                     move  "align='center'"
                                          to   w-exe-ttd-alw
           else      move  spaces         to   w-exe-ttd-alw          .
      *                  *---------------------------------------------*
      *                  * Colonne                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ttd-col        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "colspan='"          to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           move      "'"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-ttd-cow          .
      *                  *---------------------------------------------*
      *                  * Stile                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bold                                    *
      *                      *-----------------------------------------*
           if        w-exe-ttd-stl        =    "B"
                     move  "<B>"          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2
           else      move  spaces         to   w-exe-ttd-st1
                     move  spaces         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Eventuale 'rosso'                       *
      *                      *-----------------------------------------*
           if        w-exe-ttd-stl        not  = "r"
                     go to emi-htm-ttd-800.
           move      "<B style='color:red'>"
                                         to   w-exe-ttd-st1          .
           move      "</B>"              to   w-exe-ttd-st2          .
       emi-htm-ttd-800.
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      10                   to   w-all-str-num          .
           move      w-exe-ttd-tg1        to   w-all-str-cat (1)      .
           move      w-exe-ttd-cow        to   w-all-str-cat (2)      .
           move      w-exe-ttd-alw        to   w-all-str-cat (3)      .
           move      w-exe-ttd-wdw        to   w-all-str-cat (4)      .
           move      ">"                  to   w-all-str-cat (5)      .
           move      w-exe-ttd-st1        to   w-all-str-cat (6)      .
           move      w-exe-ttd-txt        to   w-all-str-cat (7)      .
           move      w-exe-ttd-txt
                    (81 : 40)             to   w-all-str-cat (8)      .
           move      w-exe-ttd-st2        to   w-all-str-cat (9)      .
           move      w-exe-ttd-tg2        to   w-all-str-cat (10)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Emissione TAG 'td' o 'th'                       *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       emi-htm-ttd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-ttd-999.
       emi-htm-ttd-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
